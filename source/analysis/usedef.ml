open Core
open Ast
open Expression
open Statement
open MyUtil


module type UsedefState = sig
  type t [@@deriving show]

  val bottom : t

  val less_or_equal : left:t -> right:t -> bool

  val join : t -> t -> t

  val widen : previous:t -> next:t -> iteration:int -> t

  val is_defined : t -> Reference.t -> bool

  val is_undefined : t -> Reference.t -> bool

  val forward : statement_key:int -> t -> statement:Statement.t -> t

  val backward : statement_key:int -> t -> statement:Statement.t -> t
end


module UsedefState = struct
  module ReferenceSet = SSet (Reference)
  module ReferenceMap = SMap (Reference)

  module VarSet = struct 
    
    type t = ReferenceSet.t
    let empty = ReferenceSet.empty
    let singleton name = ReferenceSet.singleton name
    let union left right = ReferenceSet.union left right

    let inter left right = ReferenceSet.inter left right
    let fold ~init ~f t = ReferenceSet.fold t ~init ~f

  end


  type usedef =
    | Use
    | Def 
    | Both

  type t = { 
    defined : ReferenceSet.t;
    undefined : ReferenceSet.t;
    usedef_table : usedef ReferenceMap.t;
  }

  let show _ = "Not Implemented"

  
  let pp_usedef format usedef =
    Format.fprintf format
    (match usedef with
    | Use -> "Use"
    | Def -> "Def"
    | Both -> "Both"
    ) 

  let pp_table format table = 
    ReferenceMap.iter ~f:(fun key data -> 
      Format.fprintf format "%a -> %a\n" Reference.pp key pp_usedef data
    ) table

  let pp format t = 
    Format.fprintf format "%a\n\n" pp_table t.usedef_table;
    Format.fprintf format "[ Defined Variables ] => \n";
    ReferenceSet.iter t.defined ~f:(fun key -> Format.fprintf format "%a, " Reference.pp key);
    Format.fprintf format "\n";
    Format.fprintf format "[ Undefined Variables ] => \n";
    ReferenceSet.iter t.undefined ~f:(fun key -> Format.fprintf format "%a, " Reference.pp key);
    Format.fprintf format "\n"

  

  
  let usedef_create = ReferenceMap.empty

  let create = {defined=ReferenceSet.empty; undefined=ReferenceSet.empty; usedef_table=usedef_create}
  let bottom = create

  let less_or_equal ~left:_ ~right:_ = true

  let join left right = 
    let undefined = ReferenceSet.union left.undefined right.undefined in
    let defined = ReferenceSet.diff (ReferenceSet.union left.defined right.defined) undefined in
    { defined; undefined; usedef_table=usedef_create; }

  let widen ~previous:_ ~next ~iteration:_ = next

  let add_reference key varset =
    List.fold (Reference.possible_qualifiers key) ~init:(ReferenceSet.add varset key) ~f:(fun varset k -> ReferenceSet.add varset k)

  let get_use_variables usedef_table =
    ReferenceMap.fold usedef_table ~init:ReferenceSet.empty ~f:(fun key data varset ->
      match data with
      | Use | Both -> add_reference key varset
      | Def -> varset
    )

  let get_def_variables usedef_table = 
    ReferenceMap.fold usedef_table ~init:ReferenceSet.empty ~f:(fun key data varset ->
      match data with
      | Def | Both -> add_reference key varset
      | Use -> varset
    )

  let is_defined { defined; _ } reference =
    ReferenceSet.mem defined reference

  let is_undefined t reference =
    not (is_defined t reference)

  let update_defined t usedef_table =
    let use_variables = get_use_variables usedef_table in
    let def_variables = get_def_variables usedef_table in 
    (* if all use variabels are defined, then put def variables in defined set *)
    let _ = if ReferenceSet.is_subset use_variables ~of_:t.defined 
    then ReferenceSet.union t.defined def_variables
    else t.defined
    in
    ReferenceSet.union t.defined def_variables

  let update_undefined t usedef_table =
    let use_variables = get_use_variables usedef_table in
    let def_variables = get_def_variables usedef_table in 
    let undefined = ReferenceMap.fold usedef_table ~init:t.undefined ~f:(fun key _ varset -> 
      if ReferenceSet.mem t.defined key then varset else add_reference key varset
    )
    in
    (* if all use variabels are defined, then put def variables in defined set *)
    let _ =
    if ReferenceSet.is_subset use_variables ~of_:t.defined 
    then undefined
    else  ReferenceSet.union undefined def_variables
    in
    ReferenceSet.diff undefined def_variables


  let update_state t usedef_table =
    {
      defined=update_defined t usedef_table;
      undefined=update_undefined t usedef_table;
      usedef_table;
    }
  
  let rec forward_expression (exp: Expression.t) =
    let forward_list expression_list f =
      List.fold ~init:VarSet.empty ~f:(fun accum e ->
        VarSet.union accum (f e)
      ) expression_list
    in
    let forward_generator (generator: Comprehension.Generator.t) =
      VarSet.union (forward_expression generator.target) (forward_expression generator.iterator)
      |> VarSet.union (forward_list generator.conditions forward_expression)
    in
    let forward_parameter (param: Parameter.t) =
      let param = Node.value param in
      VarSet.union 
        (Option.value_map param.value ~default:VarSet.empty ~f:forward_expression)
        (Option.value_map param.annotation ~default:VarSet.empty ~f:forward_expression)
    in
    let forward_expression_comprehension (comprehension: Expression.t Comprehension.t) =
      VarSet.union (forward_expression comprehension.element) (forward_list comprehension.generators forward_generator)
    in
    match Node.value exp with
    | Name n -> 
      (match name_to_reference n with
      | Some name -> VarSet.singleton name
      | None -> VarSet.empty
      )
    | Await e -> forward_expression e
    | BooleanOperator e -> VarSet.union (forward_expression e.left) (forward_expression e.right)
    | Call e -> 
      let f accum (a: Call.Argument.t) =
        VarSet.union accum (forward_expression a.value)
      in
      VarSet.union (forward_expression e.callee) (List.fold ~init:VarSet.empty ~f e.arguments)
    | ComparisonOperator e -> VarSet.union (forward_expression e.left) (forward_expression e.right)
    | Dictionary e -> 
      let f accum (a: Dictionary.Entry.t) =
        VarSet.union accum (VarSet.union (forward_expression a.key) (forward_expression a.value))
      in
      VarSet.union (List.fold ~init:VarSet.empty ~f e.entries) (forward_list e.keywords forward_expression)
    | Generator e -> forward_expression_comprehension e
    | FormatString e -> forward_list e (fun e ->
        match e with
        | Format e -> forward_expression e
        | _ -> VarSet.empty
      )
    | Lambda e -> VarSet.union (forward_list e.parameters forward_parameter) (forward_expression e.body)
    | List e -> forward_list e forward_expression
    | ListComprehension e -> forward_expression_comprehension e
    | Set e -> forward_list e forward_expression
    | SetComprehension e -> forward_expression_comprehension e
    | Starred e ->
        (match e with
        | Once e | Twice e -> forward_expression e
        )
    | Ternary e -> VarSet.union (forward_expression e.target) (forward_expression e.test) |> VarSet.union (forward_expression e.alternative)
    | Tuple e -> forward_list e forward_expression
    | UnaryOperator e -> forward_expression e.operand
    | WalrusOperator e -> VarSet.union (forward_expression e.target) (forward_expression e.value)
    | Yield (Some e) -> forward_expression e
    | YieldFrom e -> forward_expression e
    | _ -> VarSet.empty


  let forward_assignment ~target ~value usedef_map =
    let target_variables = forward_expression target in
    let value_variables = forward_expression value in
    let both_variables = VarSet.inter target_variables value_variables in
    let usedef_map = VarSet.fold ~init:usedef_map ~f:(fun key usedef_map -> ReferenceMap.add usedef_map ~key ~data:Def) target_variables in
    let usedef_map = VarSet.fold ~init:usedef_map ~f:(fun key usedef_map -> ReferenceMap.add usedef_map ~key ~data:Use) value_variables in
    let usedef_map = VarSet.fold ~init:usedef_map ~f:(fun key usedef_map -> ReferenceMap.add usedef_map ~key ~data:Both) both_variables in
    usedef_map

  let forward_assert test usedef_map =
    let test_variables = forward_expression test in
    VarSet.fold ~init:usedef_map ~f:(fun key usedef_map -> ReferenceMap.add usedef_map ~key ~data:Use) test_variables

  let forward_statement ~(statement: Statement.t) state =
    match Node.value statement with
    | Assign { Assign.target; value; _} ->
      forward_assignment ~target ~value state
    | Assert { Assert.test; _ } -> 
      forward_assert test state
    | _ -> state

  let forward ~statement_key:_ state ~statement =
    let usedef_table = forward_statement ~statement usedef_create in
    update_state state usedef_table

  let backward ~statement_key:_ state ~statement:_ =
    state

end

module type UsedefFixpoint = sig
  type state

  type t = {
    usedef_tables: state Int.Table.t
  }
  [@@deriving show]

  val entry : t -> state option

  val normal_exit : t -> state option

  val exit : t -> state option

  val empty : t

  val get_usedef_tables : t -> state Int.Table.t

  val find : t -> int -> state option

  val find_usedef_table_of_location : t -> Cfg.t -> Location.t -> state option

  val forward : cfg:Cfg.t -> initial:state -> t

  val backward : cfg:Cfg.t -> initial:state -> t

  val equal : f:(state -> state -> bool) -> t -> t -> bool
end

module Make (State : UsedefState) = struct
  type state = State.t


  type t = {
    usedef_tables: State.t Int.Table.t;
  }

  let equal ~f left right =
    Core.Hashtbl.equal f left.usedef_tables right.usedef_tables


  let pp format { usedef_tables; } =
    let print_state ~name ~key ~data =
      Format.fprintf format "%s %d -> \n%a\n" name key State.pp data
    in
    Hashtbl.iteri usedef_tables ~f:(print_state ~name:"UseDef")

  let show fixpoint = Format.asprintf "%a" pp fixpoint

  let find { usedef_tables; _ } node_id = Hashtbl.find usedef_tables node_id
  let entry { usedef_tables; } = Hashtbl.find usedef_tables Cfg.entry_index

  let normal_exit { usedef_tables; } = Hashtbl.find usedef_tables Cfg.normal_index

  let exit { usedef_tables; _ } = Hashtbl.find usedef_tables Cfg.exit_index

  let empty = { usedef_tables=Int.Table.create (); }

  let get_usedef_tables { usedef_tables; _ } = usedef_tables

  let find_usedef_table_of_location t (cfg: Cfg.t) location =
    Int.Table.fold cfg ~init:None ~f:(fun ~key:node_id ~data:node state ->
      if Option.is_some state then state
      else
        let statements = Cfg.Node.statements node in
        List.fold statements ~init:state ~f:(fun state statement -> 
          let start_contains = Location.contains_eq ~location:(Node.location statement) (Location.start location) in
          let stop_contains = Location.contains_eq ~location:(Node.location statement) (Location.stop location) in
          if start_contains && stop_contains then find t node_id else state
        )
    )


  let our_compute_fixpoint cfg ~initial_index ~initial ~predecessors ~successors ~transition =
    (*
     * This is the implementation of a monotonically increasing chaotic fixpoint
     * iteration sequence with widening over a control-flow graph (CFG) using the
     * recursive iteration strategy induced by a weak topological ordering of the
     * nodes in the control-flow graph. The recursive iteration strategy is
     * described in Bourdoncle's paper on weak topological orderings:
     *
     *   F. Bourdoncle. Efficient chaotic iteration strategies with widenings.
     *   In Formal Methods in Programming and Their Applications, pp 128-141.
     *)
    let components = WeakTopologicalOrder.create ~cfg ~entry_index:initial_index ~successors in

    let usedef_tables = Int.Table.create () in

    let join_with_predecessors_usedef_tables node state =
      
      if Int.equal (Cfg.Node.id node) initial_index then
        State.join state initial
      else
        predecessors node
        |> Set.fold ~init:state ~f:(fun sofar predecessor_index ->
               Hashtbl.find usedef_tables predecessor_index
               |> Option.value ~default:State.bottom
               |> State.join sofar)
    in

    let analyze_node node =
      let node_id = Cfg.Node.id node in
      let usedef_table = 
        Hashtbl.find usedef_tables node_id
        |> Option.value ~default:State.bottom
        |> join_with_predecessors_usedef_tables node
      in
      let usedef_table = transition node_id usedef_table (Cfg.Node.statements node) in
      (*Format.printf "[[[ USEDEF TABLE: Node %d ]]] \n\n%a\n\n" node_id State.pp usedef_table;*)
      Hashtbl.set usedef_tables ~key:node_id ~data:usedef_table;

    in
    let rec analyze_component = function
      | { WeakTopologicalOrder.Component.kind = Node node; _ } -> 
        analyze_node node
      | { kind = Cycle { head; components }; _ } ->
          (* Loop에 해당하는 거 같음 *)
          let head_id = Cfg.Node.id head in
          let rec iterate local_iteration =
            analyze_node head;
            List.iter ~f:analyze_component components;
            let current_head_precondition = Hashtbl.find_exn usedef_tables head_id in
            let new_head_precondition =
              join_with_predecessors_usedef_tables head current_head_precondition
            in

            let converged =
              State.less_or_equal ~left:new_head_precondition ~right:current_head_precondition
            in
            Log.log
              ~section:`Fixpoint
              "\n%a\n  { <= (result %b) (iteration = %d) }\n\n%a"
              State.pp
              new_head_precondition
              converged
              local_iteration
              State.pp
              current_head_precondition;
            if not converged then (
              let precondition =
                State.widen
                  ~previous:current_head_precondition
                  ~next:new_head_precondition
                  ~iteration:local_iteration
              in
              Hashtbl.set usedef_tables ~key:head_id ~data:precondition;
              iterate (local_iteration + 1))
            else
              (* At this point, we know we have a local fixpoint.
               * Since operators are monotonic, `new_head_precondition` is also
               * a post fixpoint. This is basically the argument for performing
               * decreasing iteration sequence with a narrowing operator.
               * Therefore, `new_head_precondition` might be more precise,
               * let's use it at the result.
               *)
              Hashtbl.set usedef_tables ~key:head_id ~data:new_head_precondition
          in
          iterate 0
    in
    List.iter ~f:analyze_component components;

    (*
    Hashtbl.iteri class_hierachy ~f:(fun ~key:ref ~data:define_list -> 
      Format.printf "[Reference] \n %a \n" Reference.pp ref;
      Hashtbl.iteri define_list ~f:(fun ~key:define ~data:summary ->
        Format.printf "\n %a \n" Define.pp define;
        Format.printf "\n %a \n" Summary.pp summary;
      );
    );
    *)
    

    { usedef_tables; }

  let forward ~cfg ~initial =
    let transition node_id init statements =
      let forward statement_index before statement =
        let statement_key = [%hash: int * int] (node_id, statement_index) in
        let after = State.forward ~statement_key before ~statement in
        (*
        Format.printf "\n\n  {  %a  } \n\n"
        Statement.pp
        statement
        ;
        *)
        (*Log.log
          ~section:`Fixpoint
          "\n%a\n  {  %a  }\n\n%a"
          State.pp
          before
          Statement.pp
          statement
          State.pp
          after;*)
        after
      in
      List.foldi ~f:forward ~init statements
    in
    our_compute_fixpoint
      cfg
      ~initial_index:Cfg.entry_index
      ~initial
      ~predecessors:Cfg.Node.predecessors
      ~successors:Cfg.Node.successors
      ~transition
    (*
  let our_forward ~cfg ~initial =
    let transition node_id init statements =
      let forward statement_index before ({value; _} as statement : Statement.statement Node.t) =
        let _ = 
          match value with
          | Statement.Statement.Class {body; _ } -> 
            Some body
          | _ -> None
        in
        let statement_key = [%hash: int * int] (node_id, statement_index) in
        let after = State.forward ~statement_key before ~statement in
        Log.log
          ~section:`Fixpoint
          "\n%a\n  {  %a  }\n\n%a"
          State.pp
          before
          Statement.pp
          statement
          State.pp
          after;
        after
      in
      List.foldi ~f:forward ~init statements
    in
    compute_fixpoint
      cfg
      ~initial_index:Cfg.entry_index
      ~initial
      ~predecessors:Cfg.Node.predecessors
      ~successors:Cfg.Node.successors
      ~transition
      *)
  


  let backward ~cfg ~initial =
    let transition node_id init statements =
      let statement_index = ref (List.length statements) in
      let backward statement =
        statement_index := !statement_index - 1;
        let statement_key = [%hash: int * int] (node_id, !statement_index) in
        State.backward ~statement_key ~statement
      in
      List.fold_right ~f:backward ~init statements
    in
    our_compute_fixpoint
      cfg
      ~initial_index:Cfg.exit_index
      ~initial
      ~predecessors:Cfg.Node.successors
      ~successors:Cfg.Node.predecessors
      ~transition

end


module UsedefStruct = Make (UsedefState)
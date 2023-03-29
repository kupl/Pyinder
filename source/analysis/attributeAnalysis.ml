
open Core
open Ast
open Ast.Util
open Expression
open Statement

module SkipMap = LocInsensitiveExpMap

module AttributeStorage = struct
  type t = (Identifier.Set.t) LocInsensitiveExpMap.t
  [@@deriving sexp, equal]

  let empty = LocInsensitiveExpMap.empty

  let map t ~f =
    LocInsensitiveExpMap.map t ~f

  let fold t ~init ~f =
    LocInsensitiveExpMap.fold t ~init ~f

  let pp_identifier_set formatter ident_set =
    let msg = 
      Identifier.Set.fold ident_set ~init:"(" ~f:(fun msg ident ->
        msg ^ ", " ^ ident
      )
      ^ ")"
    in
    Format.fprintf formatter "%s" msg
  
  let pp formatter t = 
    LocInsensitiveExpMap.iteri t ~f:(fun ~key ~data ->
      Format.fprintf formatter "%s ==> %a \n" (LocInsensitiveExp.show key) pp_identifier_set data
    )
    

  let add_attribute target attribute storage =
    let attribute =
      let data =
        if LocInsensitiveExpMap.mem storage target
        then (LocInsensitiveExpMap.find_exn storage target)
        else (Identifier.Set.empty)
      in 
      Identifier.Set.add data attribute
    in
    LocInsensitiveExpMap.set storage ~key:target ~data:attribute
    
  let add_prefix storage ~prefix =
    fold storage ~init:empty ~f:(fun ~key ~data new_storage ->
      match get_identifier_base key with
      | Some base ->
        let key = change_identifier_base key ~data:((Reference.show prefix) ^ base) in
        LocInsensitiveExpMap.set new_storage ~key:key ~data
      | _ -> failwith (Format.sprintf "Why is in here by add? %s" (Expression.show key))
    )

  let filter_by_prefix storage ~prefix =
    fold storage ~init:empty ~f:(fun ~key ~data new_storage ->
      match Node.value key with
      | Constant _ -> new_storage
      | _ ->
        (
        match get_identifier_base key with
        | Some base ->
          let base_reference = Reference.create base in
          if (Reference.is_prefix ~prefix base_reference) && not (Reference.equal base_reference prefix)
          then 
            (
              let new_base = (Reference.drop_prefix ~prefix base_reference) |> Reference.show in
              let key = change_identifier_base key ~data:new_base in
              LocInsensitiveExpMap.set new_storage ~key:key ~data
            )
          else 
            new_storage
        | _ -> failwith (Format.sprintf "Why is in here by filter? %s" (Expression.show key))
        )
      
    )
    

  let join left right =
    LocInsensitiveExpMap.merge left right ~f:(fun ~key:_ data ->
      match data with
      | `Both (left, right) ->
        Some (Identifier.Set.union left right)
      | `Left data | `Right data -> Some data 
    )

  
end

let is_in_skip_set skip_map base attribute =
  match SkipMap.find skip_map base with
  | Some attr_set -> 
    String.Set.mem attr_set attribute
  | _ -> false

let update_skip_map skip_map callee (arguments: Call.Argument.t list) = 
  if String.equal (Expression.show callee) "hasattr" 
  then
    if (List.length arguments) = 2
    then
      let base = List.nth_exn arguments 0 in
      let attrs = List.nth_exn arguments 1 in
      let string_set = Visit.collect_constant_string attrs.value |> String.Set.of_list in
      SkipMap.set skip_map ~key:base.value ~data:string_set
    else
      failwith "Callee Length is not 2"
  else
    skip_map

let rec forward_expression_list (storage, skip_map) ~exp_list =
  List.fold exp_list ~init:(storage, skip_map) ~f:(fun (storage, skip_map) expression -> forward_expression (storage, skip_map) ~expression)

and forward_expression ?(is_assign=false) ~expression:({ Node.value; _ } as expression) (storage, skip_map) =
  let _ = expression in
  let forward_generators (storage, skip_map) ~generators =
    List.fold generators ~init:(storage, skip_map) ~f:(fun (storage, skip_map) { Comprehension.Generator.target; iterator; conditions; _ } -> 
      (storage, skip_map)
      |> forward_expression ~expression:target
      |> forward_expression ~expression:iterator
      |> forward_expression_list ~exp_list:conditions
    )
  in

  match value with
  | Expression.Name (Attribute { base; attribute; _ }) ->
    if (not is_assign) && (is_in_skip_set skip_map base attribute)
    then
      (storage, skip_map) 
    else
      (

      (storage
      |> AttributeStorage.add_attribute base attribute,
      skip_map)
    |> forward_expression ~expression:base
      )
  | Name (Identifier _) -> (storage, skip_map)
  | Await expression -> forward_expression (storage, skip_map) ~expression
  | BooleanOperator { left; right; _ }
  | ComparisonOperator { left; right; _ } ->
    (storage, skip_map)
    |> forward_expression ~expression:left
    |> forward_expression ~expression:right
  | Call { callee; arguments; } ->
    (storage, skip_map)
    |> forward_expression ~expression:callee
    |> (fun (storage, skip_map) -> 
        List.fold arguments ~init:(storage, skip_map) ~f:(fun (storage, skip_map) { value; _ } -> forward_expression (storage, skip_map) ~expression:value)
    )
  | Dictionary { entries; keywords; } ->
    List.fold entries ~init:(storage, skip_map) ~f:(fun (storage, skip_map) { key; value; } -> 
      (storage, skip_map)
      |> forward_expression ~expression:key
      |> forward_expression ~expression:value
    )
    |> forward_expression_list ~exp_list:keywords
  | DictionaryComprehension { element={ key; value;}; generators; } ->
    (storage, skip_map)
    |> forward_expression ~expression:key
    |> forward_expression ~expression:value
    |> forward_generators ~generators
  | Generator { element; generators; } 
  | ListComprehension { element; generators; }
  | SetComprehension { element; generators; }
  ->
    (storage, skip_map)
    |> forward_expression ~expression:element
    |> forward_generators ~generators    
  | FormatString substr_list ->
    List.fold substr_list ~init:(storage, skip_map) ~f:(fun (storage, skip_map) substr ->
      match substr with
      | Format expression -> forward_expression (storage, skip_map) ~expression
      | _ -> (storage, skip_map)  
    )
  | Lambda { parameters; body; } ->
    List.fold parameters ~init:(storage, skip_map) ~f:(fun (storage, skip_map) { Node.value; _ } ->
        match value.value with
        | Some expression -> forward_expression (storage, skip_map) ~expression
        | _ -> (storage, skip_map)
      )
    |> forward_expression ~expression:body
  | List exp_list | Set exp_list | Tuple exp_list ->
    (storage, skip_map) |> forward_expression_list ~exp_list
  | Starred t ->
    (match t with
    | Once expression
    | Twice expression ->
      (storage, skip_map) |> forward_expression ~expression
    )
  | Ternary { target; test; alternative; } ->
    (storage, skip_map)
    |> forward_expression ~expression:target
    |> forward_expression ~expression:test
    |> forward_expression ~expression:alternative
  | UnaryOperator { operand; _ } ->
    (storage, skip_map) |> forward_expression ~expression:operand
  | WalrusOperator { target; value; } ->
    (storage, skip_map)
    |> forward_expression ~expression:target
    |> forward_expression ~expression:value
  | Yield expression_opt ->
    (match expression_opt with
    | Some expression -> forward_expression (storage, skip_map) ~expression
    | _ -> (storage, skip_map)
    )
  | YieldFrom expression -> forward_expression (storage, skip_map) ~expression
  | Constant _ -> (storage, skip_map)
  

let rec forward_statement (storage, skip_map) ~statement:({ Node.value; _ } as statement) =
  let _ = statement in
  (*Log.dump "%a" Statement.pp statement;*)
  let forward_statement_list (storage, skip_map) ~stmt_list =
    List.fold stmt_list ~init:(storage, skip_map) ~f:(fun (storage, skip_map) statement -> forward_statement (storage, skip_map) ~statement)
  in

  let rec forward_pattern (storage, skip_map) ~pattern:{Node.value; _} =
    let forward_pattern_list (storage, skip_map) ~patterns =
      List.fold patterns ~init:(storage, skip_map) ~f:(fun (storage, skip_map) pattern -> forward_pattern (storage, skip_map) ~pattern)
    in

    match value with
    | Match.Pattern.MatchAs { pattern=Some pattern; _ } -> forward_pattern (storage, skip_map) ~pattern
    | MatchClass { patterns; keyword_patterns; _ } ->
      (storage, skip_map)
      |> forward_pattern_list ~patterns
      |> forward_pattern_list ~patterns:keyword_patterns
    | MatchMapping { keys; patterns; _ } ->
      (storage, skip_map)
      |> forward_expression_list ~exp_list:keys
      |> forward_pattern_list ~patterns
    | MatchOr patterns | MatchSequence patterns ->
      forward_pattern_list (storage, skip_map) ~patterns
    | MatchValue expression -> forward_expression (storage, skip_map) ~expression
    | _ -> (storage, skip_map) 
  in

  let forward_case storage ~case:{ Match.Case.pattern; guard; body; } =
    storage
    |> forward_pattern ~pattern
    |> (fun storage -> 
        match guard with
        | Some expression -> forward_expression storage ~expression
        | _ -> storage
      )
    |> forward_statement_list ~stmt_list:body
  in

  match value with
  | Statement.Assign { target; value; _ } ->
    (storage, skip_map)
    |> forward_expression ~is_assign:true ~expression:target
    |> forward_expression ~expression:value
  | Assert { test; message; _ } ->
    (storage, skip_map)
    |> forward_expression ~expression:test
    |> 
    (match message with
    | Some expression -> forward_expression ~is_assign:false ~expression
    | _ -> (fun (storage, skip_map) -> (storage, skip_map))
    )
  | Class { body; _ } ->  
    (storage, skip_map) |> forward_statement_list ~stmt_list:body
  | Delete exp_list -> forward_expression_list (storage, skip_map) ~exp_list
  | Expression expression -> forward_expression (storage, skip_map) ~expression
  | For { iterator; body; orelse; _ } ->
    (storage, skip_map)
    |> forward_expression ~expression:iterator
    |> forward_statement_list ~stmt_list:body
    |> forward_statement_list ~stmt_list:orelse
  | If { test=({ Node.value = Call { callee; arguments; }; _ } as test); body; orelse; } 
    when String.equal (Expression.show callee) "hasattr"     
    ->
      let storage, skip_map = forward_expression (storage, skip_map) ~expression:test in
      let updated_skip_map = update_skip_map skip_map callee arguments in
      let storage, _ = forward_statement_list (storage, updated_skip_map) ~stmt_list:body in
      let storage, _ = forward_statement_list (storage, updated_skip_map) ~stmt_list:orelse in
      (storage, skip_map)
  | If { test; body; orelse; } ->
    (storage, skip_map) |> forward_expression ~expression:test 
    |> forward_statement_list ~stmt_list:body
    |> forward_statement_list ~stmt_list:orelse
  | Match { subject; cases; } -> 
    (storage, skip_map) 
    |> forward_expression ~expression:subject
    |> (fun storage -> List.fold cases ~init:storage ~f:(fun storage case -> forward_case storage ~case))
  | Return { expression; _ } ->
    (match expression with
    | Some expression -> forward_expression (storage, skip_map) ~expression
    | None -> (storage, skip_map)
    )
  | Try { body; orelse; finally; _ } ->
    (storage, skip_map)
    |> forward_statement_list ~stmt_list:body
    |> forward_statement_list ~stmt_list:orelse
    |> forward_statement_list ~stmt_list:finally
  | With { items; body; _ } ->
    List.fold items ~init:(storage, skip_map) ~f:(fun (storage, skip_map) (expression, expression_opt) ->
      (storage, skip_map)
      |> forward_expression ~expression
      |> (fun (storage, skip_map) ->
          match expression_opt with
          | Some expression -> forward_expression (storage, skip_map) ~expression
          | None -> (storage, skip_map)
        )  
    )
    |> forward_statement_list ~stmt_list:body
  | While { test; body; orelse; } ->
    (storage, skip_map)
    |> forward_expression ~expression:test
    |> forward_statement_list ~stmt_list:body
    |> forward_statement_list ~stmt_list:orelse
  | Define { body; _ } ->
    (storage, skip_map) |> forward_statement_list ~stmt_list:body
  | Global _
  | Import _
  | Raise _ 
  | Nonlocal _
  | Break | Continue | Pass
    -> (storage, skip_map)
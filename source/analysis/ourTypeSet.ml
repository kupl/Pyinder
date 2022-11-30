(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Usedef

exception DuplicateException;;
let weaken_typ typ =
  let weaken_typ = Type.weaken_literals typ in
  let weaken_typ =
    match weaken_typ with
    | Type.IntExpression _ -> Type.Primitive "int"
    | _ -> weaken_typ
  in
  weaken_typ

module VarType = struct
  type t = Reference.t * Type.t [@@deriving compare, sexp, equal, hash, to_yojson, show]
end

include VarType
module FunctionSet = Set.Make (Reference)
module VarTypeMap = Map.Make (VarType)


module ClassSummary = struct
  type t = { 
    store_info: (Reference.t, Refinement.Store.t) Base.Hashtbl.t;
    class_vartype: (Reference.t, (FunctionSet.t VarTypeMap.t)) Base.Hashtbl.t;
    (*
    * 이 self varaible (reference.map.t) 는 다음과 같은 annotation (annotationMap) 이 가능하고
    * 그 annotation은 다음과 같은 함수 reference (reference.t list) 에서 온다   
    *)
  }

  let create () = { 
    store_info=Hashtbl.create (module Reference);
    class_vartype=Hashtbl.create (module Reference);
  }

  
  let extract_self store =
    (*Format.printf "[[[ BEFORE EXTRACT SELF ]]] \n\n%a\n\n" Refinement.Store.pp store;*)
    Refinement.Store.update_with_filter ~old_store:Refinement.Store.empty ~new_store:store ~filter:(fun reference _ -> 
      String.is_suffix ~suffix:"$self" (Reference.last reference)
    )

  let extract_element_type store =
    (*Format.printf "[[[ BEFORE EXTRACT ELEMENT ]]] \n\n%a\n\n" Refinement.Store.pp store;*)
    let filter_element_type (typ: Type.t) =
      
      match typ with
      | Bottom | Any | Top -> false
      | _ -> true
      
    in
    let rec fold_unit u =
      let base = 
        match Refinement.Unit.base u with
        | Some base -> 
          if filter_element_type base.annotation then Some base else None
        | None -> None
      in
      let attributes = 
        Map.Tree.fold (Refinement.Unit.attributes u) ~init:(Identifier.Map.Tree.empty) ~f:(fun ~key ~data u ->
          Identifier.Map.Tree.set u ~key ~data:(fold_unit data)
        )
      in
      Refinement.Unit.create_all base attributes
    in
    let store = Refinement.Store.fold_map ~f:(fun ~key:reference ~data:unit store ->
      (*Format.printf "[ ITER ] \n\n%a \n\n%a \n\n" Reference.pp reference Refinement.Unit.pp unit;*)
      Reference.Map.set store ~key:reference ~data:(fold_unit unit)
    ) Refinement.Store.empty store
    in
    let x = Refinement.Store.update_with_filter ~old_store:Refinement.Store.empty ~new_store:store ~filter:(fun _ typ -> 
      (*Format.printf "%a \n\n%a \n\n" Reference.pp reference Annotation.pp typ;*)
        (match typ.annotation with
        | Bottom | Any | Top -> false
        | _ -> true
        )
    )
    in
    (*Format.printf "[ After Extract ] \n\n%a \n\n" Refinement.Store.pp x;*)
    x
  
  let set { store_info; _ } ~key ~data =
    Hashtbl.set store_info ~key ~data:(extract_element_type (extract_self data))

  let get { store_info; _ } ~key =
    Hashtbl.find store_info key

  let meet ~global_resolution (({ store_info; _ } as t): t) class_name method_postcondition =
    let current_postcondition = Hashtbl.find store_info class_name in
    match current_postcondition with
    | Some postcondition ->
      set t ~key:class_name ~data:(Refinement.Store.meet ~global_resolution postcondition method_postcondition)
    | None -> 
      set t ~key:class_name ~data:method_postcondition
    

  let outer_join ~global_resolution (({ store_info; _ } as t): t) class_name method_postcondition =
    let current_postcondition = Hashtbl.find store_info class_name |> Option.value ~default:(Refinement.Store.empty) in
    if Refinement.Store.less_or_equal ~global_resolution ~left:current_postcondition ~right:Refinement.Store.empty 
    then 
      set t ~key:class_name ~data:method_postcondition
    else
      set t ~key:class_name ~data:(Refinement.Store.outer_join ~global_resolution current_postcondition method_postcondition)

    
  let join_with_merge ~global_resolution (({ store_info; _ } as t): t) class_name method_postcondition =
    (*
    여기서 postcondition의 변수 하나하나를 저장한다   
    *)
    

    let current_postcondition = Hashtbl.find store_info class_name |> Option.value ~default:(Refinement.Store.empty) in
    set t ~key:class_name ~data:(Refinement.Store.join_with_merge ~global_resolution current_postcondition method_postcondition)

  let outer_widen ~global_resolution ({ store_info; _ }: t) class_name method_postcondition =
    let current_postcondition = Hashtbl.find store_info class_name |> Option.value ~default:(Refinement.Store.empty) in
    Hashtbl.set store_info ~key:class_name ~data:(Refinement.Store.outer_widen ~global_resolution current_postcondition method_postcondition ~iteration:0 ~widening_threshold:3)

  let pp_functionset format functionset = 
    FunctionSet.iter functionset ~f:(fun func_name -> 
      Format.fprintf format "%a, " Reference.pp func_name)

  let pp_vartype_map format vartype_map =
    VarTypeMap.iteri vartype_map ~f:(fun ~key:(var_name, typ) ~data ->
      Format.fprintf format "%a: %a => { %a }\n" Reference.pp var_name Type.pp typ pp_functionset data
    )

  let pp_class_vartype format { class_vartype; _ } =
    Hashtbl.iteri class_vartype ~f:(fun ~key ~data ->
      Format.fprintf format "[[[ Class VarType: %a ]]] \n%a\n" Reference.pp key pp_vartype_map data
    )

  let pp format t =
    Hashtbl.iteri t.store_info ~f:(fun ~key ~data ->
      Format.fprintf format "[[[ Class Info ]]] \n %a \n -> %a \n" Reference.pp key Refinement.Store.pp data
    )

  let pp_json t =
    let json_string =
    Hashtbl.fold t.store_info ~init:"" ~f:(fun ~key ~data json_string ->
      json_string ^ (Format.asprintf {|{"%a" : {%a},|} Reference.pp key Refinement.Store.to_yojson data)
    )
    in
    let json_string = String.chop_suffix_exn json_string ~suffix:"," in
    let json_string = json_string ^ "}" in
    json_string

  let equal t1 t2 =
    Hashtbl.equal (fun ref1 ref2 -> Refinement.Store.equal ref1 ref2) t1.store_info t2.store_info

  let copy t = 
    { 
      store_info = Hashtbl.copy t.store_info;
      class_vartype = t.class_vartype;
    }

  let update_map_function_of_types { class_vartype; _ } class_name vartype_map =
    let class_vartype_map = Hashtbl.find class_vartype class_name |> Option.value ~default:(VarTypeMap.empty) in
    let class_vartype_map = 
      VarTypeMap.merge class_vartype_map vartype_map ~f:(fun ~key:_ v -> 
        match v with
        | `Left v | `Right v -> Some v
        | `Both (v1, v2) -> Some (FunctionSet.union v1 v2)
      )
    in
    Hashtbl.set class_vartype ~key:class_name ~data:class_vartype_map

  let find_map_function_of_types { class_vartype; _ } class_name var_name var_type =
    let class_vartype_map = Hashtbl.find class_vartype class_name |> Option.value ~default:(VarTypeMap.empty) in
    VarTypeMap.find class_vartype_map (var_name, var_type) |> Option.value ~default:FunctionSet.empty

end

module ArgTypes = struct
  type t = (Identifier.t, Type.t) Base.Hashtbl.t

  let create () = Hashtbl.create (module Identifier)

  let add_arg_type t ident typ =
    let modified_typ = weaken_typ typ in
    let exn_typ = Hashtbl.find t ident |> Option.value ~default:modified_typ in
    match exn_typ with
    | Bottom | Any | Top -> ()
    | _ ->
      Hashtbl.set t ~key:ident ~data:(Type.union [modified_typ; exn_typ])
(*
  let modify_arg_typ t ident typ =
    Hashtbl.set t ~key:ident ~data:typ
*)

  let equal t1 t2 =
    Hashtbl.equal (fun typ1 typ2 -> Type.equal typ1 typ2) t1 t2

  let pp format t =
    Hashtbl.iteri t ~f:(fun ~key ~data ->
      Format.fprintf format "%a -> %a \n" Identifier.pp key Type.pp data;
    )

  let get_type t ident =
    Hashtbl.find t ident |> Option.value ~default:Type.Bottom

  let export_to_resolution t resolution = 
    Hashtbl.fold t ~init:resolution ~f:(fun ~key ~data resolution ->
        Resolution.new_local resolution ~reference:(Reference.create key) ~annotation:(Annotation.create_immutable data)
    )

end

module FunctionSummary = struct
  type t = {
    arg_types : ArgTypes.t;
    return_types : Type.t;
    possible_condition : Refinement.Store.t;
    usedef_tables : UsedefStruct.t option;
    cfg : Cfg.t option;
  }

  let create () = { 
    arg_types = ArgTypes.create (); 
    return_types = Type.Bottom; 
    possible_condition = Refinement.Store.empty; 
    usedef_tables = None;
    cfg = None;
  }

    (*
  let extract_self store =
    Refinement.Store.update_with_filter ~old_store:Refinement.Store.empty ~new_store:store ~filter:(fun reference _ -> 
      String.is_suffix ~suffix:"$self" (Reference.last reference)
    )

    
  let meet_refinement ~global_resolution self_types method_postcondition =
      Refinement.Store.meet ~global_resolution self_types method_postcondition

  let meet_type return_types actual =
    Type.union [return_types; actual]

  let meet_summary ~global_resolution {self_types=left_self_types; return_types=left_return_types;} {self_types=right_self_types; return_types=right_return_types;} =
    {
      self_types = meet_refinement ~global_resolution left_self_types right_self_types;
      return_types = meet_type left_return_types right_return_types;
    }
    *)
  let equal {arg_types=ref1; return_types=typ1; _} {arg_types=ref2; return_types=typ2; _} =
      (ArgTypes.equal ref1 ref2) && (Type.equal typ1 typ2)

  let pp format {arg_types; return_types; possible_condition; _} =
    Format.fprintf format "%a \nreturn : %a \n\n%a\n" ArgTypes.pp arg_types Type.pp return_types Refinement.Store.pp possible_condition

  let add_arg_types {arg_types; _} arg_typ_list =
    List.iter arg_typ_list ~f:(fun (arg, typ) -> ArgTypes.add_arg_type arg_types arg typ)

  let add_return_info {arg_types; return_types; possible_condition; usedef_tables; cfg; _} return_type =
    let modified_typ = weaken_typ return_type in
    let return_types = Type.union [return_types; modified_typ] in
    {arg_types; return_types; possible_condition; usedef_tables; cfg;}

  let set_possible_condition { arg_types; return_types; usedef_tables; cfg; _ } possible_condition =
    { arg_types; return_types; possible_condition=(ClassSummary.extract_self possible_condition); usedef_tables; cfg;}
 
  let set_usedef_tables { arg_types; return_types; possible_condition; cfg; _ } usedef_tables =
    { arg_types; return_types; possible_condition; usedef_tables; cfg; }

  let set_cfg { arg_types; return_types; possible_condition; usedef_tables; _ } cfg =
    { arg_types; return_types; possible_condition; usedef_tables; cfg; }

  let get_usedef_tables {usedef_tables; _} = usedef_tables

  let get_possible_condition { possible_condition; _ } = possible_condition

  let get_arg_types {arg_types; _} = arg_types

  let get_return_types {return_types; _} = return_types

  let get_cfg { cfg; _ } = cfg

  let make_map_function_of_types { possible_condition; _ }  =
    let f anno_option =
      match anno_option with
      | Some anno ->
        (match Annotation.annotation anno with
        | Type.Union t_list -> Some t_list
        | t -> Some [t]
        )
      | None -> None
    in
    let vartype_map = Refinement.Store.make_map_function_of_types possible_condition f in
    
    vartype_map


end

module FunctionTable = struct
  type t = (Reference.t, FunctionSummary.t) Base.Hashtbl.t
  let create () = Hashtbl.create (module Reference)

  let copy t = Hashtbl.copy t
  let equal t1 t2 =
    Hashtbl.equal (fun fs1 fs2 -> 
      FunctionSummary.equal fs1 fs2
    ) t1 t2

  let pp format table =
    Hashtbl.iteri table ~f:(fun ~key ~data ->
      Format.fprintf format "[[[ Function Info ]]] \n%a \n%a \n" Reference.pp key FunctionSummary.pp data
    )

  let add_arg_types t reference arg_typ_list =
    let func_summary = Hashtbl.find t reference |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.add_arg_types func_summary arg_typ_list;
    Hashtbl.set t ~key:reference ~data:func_summary

  let add_return_info t func return_type =
    let func_summary = Hashtbl.find t func |> Option.value ~default:(FunctionSummary.create ()) in
    Hashtbl.set t ~key:func ~data:(FunctionSummary.add_return_info func_summary return_type)

  let set_possible_condition t func possible_condition =
    let func_summary = Hashtbl.find t func |> Option.value ~default:(FunctionSummary.create ()) in
    Hashtbl.set t ~key:func ~data:(FunctionSummary.set_possible_condition func_summary possible_condition)

  let set_usedef_tables t func usedef_tables =
    let func_summary = Hashtbl.find t func |> Option.value ~default:(FunctionSummary.create ()) in
    Hashtbl.set t ~key:func ~data:(FunctionSummary.set_usedef_tables func_summary usedef_tables)

  let set_cfg t func cfg =
    let func_summary = Hashtbl.find t func |> Option.value ~default:(FunctionSummary.create ()) in
    Hashtbl.set t ~key:func ~data:(FunctionSummary.set_cfg func_summary cfg)

  let get_usedef_tables t func_name =
    let func_summary = Hashtbl.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_usedef_tables func_summary

  let get_possible_condition t func_name =
    let func_summary = Hashtbl.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_possible_condition func_summary

  let get_func_arg_types t func_name =
    let func_summary = Hashtbl.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_arg_types func_summary

  let get_func_return_types t func_name =
    let func_summary = Hashtbl.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_return_types func_summary

  let get_cfg t func_name =
    let func_summary = Hashtbl.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_cfg func_summary

  let make_map_function_of_types t class_name =
    (*
    make reference + type => function map   
    *)
    let vartype_map = VarTypeMap.empty in

    Hashtbl.fold t ~init:vartype_map ~f:(fun ~key:func_name ~data vartype_map ->
      if Reference.is_contain ~base:func_name ~target:class_name then
        let candidates_vartype = FunctionSummary.make_map_function_of_types data in
        List.fold candidates_vartype ~init:vartype_map ~f:(fun vartype_map (str_list, anno) -> 
          VarTypeMap.update vartype_map (Reference.create_from_list str_list, Option.value_exn anno) ~f:(fun func_set_opt -> 
            FunctionSet.add (Option.value func_set_opt ~default:FunctionSet.empty) func_name
          )
        )
      else
        vartype_map
    )

end

module OurSummary = struct
  type t = {
    class_summary : ClassSummary.t;
    function_table : FunctionTable.t;
    current_function : Reference.t option;
    current_possiblecondition : Refinement.Store.t option;
  }
  [@@deriving equal]

  let create () = {
    class_summary = ClassSummary.create (); 
    function_table = FunctionTable.create (); 
    current_function = None;
    current_possiblecondition = None  
  }

  let class_summary {class_summary; _} = class_summary

  let set_current_function {class_summary; function_table; current_possiblecondition; _} current_function = 
    {class_summary; 
    function_table; 
    current_function=Some current_function;
    current_possiblecondition
    }

  let set_current_possiblecondition {class_summary; function_table; current_function; _} current_possiblecondition = 
    {
      class_summary; 
      function_table; 
      current_function;
      current_possiblecondition=current_possiblecondition
    }

  let join_with_merge_current_possiblecondition t ~global_resolution possiblecondition =
    let update_possiblecondition =
    match t.current_possiblecondition with
    | Some current_possiblecondition -> Refinement.Store.join_with_merge ~global_resolution current_possiblecondition possiblecondition
    | None -> possiblecondition
    in
    set_current_possiblecondition t (Some update_possiblecondition)

  let copy {class_summary; function_table; current_function; current_possiblecondition } = 
    {
      class_summary = ClassSummary.copy class_summary;
      function_table = FunctionTable.copy function_table;
      current_function;
      current_possiblecondition
    }
  
  let equal {class_summary=cs1; function_table=ft1; _} {class_summary=cs2; function_table=ft2; _} =
    (ClassSummary.equal cs1 cs2) && (FunctionTable.equal ft1 ft2)

  let pp_class format {class_summary; _} =
    Format.fprintf format "%a" ClassSummary.pp class_summary
  let pp_func format {function_table; _} = 
    Format.fprintf format "%a" FunctionTable.pp function_table

  let pp formatter t =
    Format.fprintf formatter "%a\n\n%a" pp_class t pp_func t

  let add_arg_types { function_table; _} reference arg_typ_list =
    FunctionTable.add_arg_types function_table reference arg_typ_list

  let add_return_info {function_table; current_function; _} return_type =
    match current_function with
    | Some func ->
      FunctionTable.add_return_info function_table func return_type
    | None -> ()

  let set_possible_condition {function_table; _} func_name possible_condition =
    FunctionTable.set_possible_condition function_table func_name possible_condition
  let set_usedef_tables {function_table; _} func_name usedef_tables =
    FunctionTable.set_usedef_tables function_table func_name usedef_tables

  let set_cfg {function_table; _} func_name cfg =
    FunctionTable.set_cfg function_table func_name cfg

  let get_usedef_tables {function_table; _} func_name = 
    FunctionTable.get_usedef_tables function_table func_name

  let get_possible_condition {function_table; _} func_name = 
    FunctionTable.get_possible_condition function_table func_name

  let get_current_usedef_tables ({current_function; _} as t) =
    match current_function with
    | Some func_name -> get_usedef_tables t func_name
    | None -> None

  let get_func_arg_types {function_table; _} func_name =
    FunctionTable.get_func_arg_types function_table func_name

  let get_func_return_types {function_table; _} func_name =
    FunctionTable.get_func_return_types function_table func_name

  let get_cfg {function_table; _} func_name =
    FunctionTable.get_cfg function_table func_name

  let get_current_possiblecondition { current_possiblecondition; _ } = current_possiblecondition


  let make_map_function_of_types { function_table; _ } class_name =
    FunctionTable.make_map_function_of_types function_table class_name

  let update_map_function_of_types { class_summary; _ } class_name vartype_map =
    ClassSummary.update_map_function_of_types class_summary class_name vartype_map

  let search_suspicious_variable t ~global_resolution parent =
    (*let usedef_table = get_usedef_table t func_name |> Option.value ~default:UsedefState.bottom in*)
    let possible_condition = ClassSummary.get t.class_summary ~key:parent in
    match possible_condition with
    | None -> []
    | Some possible_condition -> 
      let total_annotation = Refinement.Store.combine_join_with_merge ~global_resolution possible_condition in
      
      (* split each variables and types *)
      let f ~key ~data sofar = 
        let rec split_attrs (target_unit: Refinement.Unit.t)  = 
          let cand_attrs = Identifier.Map.Tree.fold (Refinement.Unit.attributes target_unit) ~init:[] ~f:(fun ~key:attr ~data:inner_unit cand -> 
            let cand_attrs = split_attrs inner_unit in
            cand@(List.map cand_attrs ~f:(fun cand_attr -> Identifier.Map.Tree.add Identifier.Map.Tree.empty ~key:attr ~data:cand_attr))
          )
          in
          if List.is_empty cand_attrs 
          then 
            let new_base_list =
              let new_anno_list =
                match (Refinement.Unit.base target_unit) with
                | None -> [None]
                | Some anno ->
                  (match Annotation.annotation anno with
                  | Type.Union t_list ->  List.map t_list ~f:(fun t -> Some (Annotation.create_mutable t))
                  | _ -> [Some anno]
                  )
              in
              List.map new_anno_list ~f:(fun new_anno -> Refinement.Unit.create_all new_anno Identifier.Map.Tree.empty)
            in
            new_base_list
          else
          List.map cand_attrs ~f:(fun cand_attr -> 
            match cand_attr with
            | `Duplicate -> raise DuplicateException
            | `Ok cand_attr -> 
            (*  
            Identifier.Map.Tree.iteri cand_attr ~f:(fun ~key ~data -> Log.dump "cand_attr %s >>> %a" key Refinement.Unit.pp data);
            *)
              Refinement.Unit.create_all (Refinement.Unit.base target_unit) cand_attr)
        in
        let candidates = split_attrs data in
        sofar@(List.map candidates ~f:(fun cand -> Reference.Map.set Reference.Map.empty ~key ~data:cand ))
      in

      let candidates = Reference.Map.fold total_annotation ~init:[] ~f:f in
      (*
      List.iter candidates ~f:(fun cand -> Reference.Map.iteri cand ~f:(fun ~key ~data -> Format.printf "%a -> %a\n" Reference.pp key Refinement.Unit.pp data));
      *)
      candidates
end

let our_model = ref (OurSummary.create ());;

let our_models = ref []

let is_search_mode = ref false;;

let single_errors = ref [];
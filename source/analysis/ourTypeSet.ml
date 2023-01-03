(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

 (*
let _ =
  let target_path = "/home/wonseok/test/test.marshalled" in
  let data_oc = open_out target_path in
  let test = Core.Int.Set.empty in 
  Marshal.to_channel data_oc test [];
  close_out data_oc
*)

open Core
open Ast
open Usedef

exception DuplicateException;;
exception NotEqualException;;
let weaken_typ typ =
  let weaken_typ = Type.weaken_literals typ in
  let weaken_typ =
    match weaken_typ with
    | Type.IntExpression _ -> Type.Primitive "int"
    | _ -> weaken_typ
  in
  weaken_typ

module VarType = struct
  type t = Reference.t * Type.t [@@deriving sexp, compare]

  (*
  let json_string (reference, typ) =
    Format.asprintf "(%a, %a)" Reference.pp reference Type.pp typ
    *)
end

module FunctionSet = Set.Make (Reference)
module VarTypeMap = Map.Make (VarType)

module StoreMap = Map.Make (Reference)
module ClassMap = Map.Make (Reference)

module ClassSummary = struct
  
  type t = { 
    store_info : Refinement.Store.t StoreMap.t;
    class_vartype: (FunctionSet.t VarTypeMap.t) ClassMap.t;
    (*store_info: (Reference.t, Refinement.Store.t) Base.Hashtbl.t;
    class_vartype: (Reference.t, (FunctionSet.t VarTypeMap.t)) Base.Hashtbl.t;
    *)
    (*
    * 이 self varaible (reference.map.t) 는 다음과 같은 annotation (annotationMap) 이 가능하고
    * 그 annotation은 다음과 같은 함수 reference (reference.t list) 에서 온다   
    *)
  } [@@deriving sexp]

  let create () = { 
    store_info=StoreMap.empty;
    class_vartype=ClassMap.empty;
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
  
  let set { store_info; class_vartype; } ~key ~data =
    {
      store_info = StoreMap.set ~key ~data:(extract_element_type (extract_self data)) store_info;
      class_vartype;
    }

  let get { store_info; _ } ~key =
    StoreMap.find store_info key

  let meet ~global_resolution (({ store_info; _ } as t): t) class_name method_postcondition =
    let current_postcondition = StoreMap.find store_info class_name in
    match current_postcondition with
    | Some postcondition ->
      set t ~key:class_name ~data:(Refinement.Store.meet ~global_resolution postcondition method_postcondition)
    | None -> 
      set t ~key:class_name ~data:method_postcondition
    

  let outer_join ~global_resolution (({ store_info; _ } as t): t) class_name method_postcondition =
    let current_postcondition = StoreMap.find store_info class_name |> Option.value ~default:(Refinement.Store.empty) in
    if Refinement.Store.less_or_equal ~global_resolution ~left:current_postcondition ~right:Refinement.Store.empty 
    then 
      set t ~key:class_name ~data:method_postcondition
    else
      set t ~key:class_name ~data:(Refinement.Store.outer_join ~global_resolution current_postcondition method_postcondition)

    
  let join_with_merge ~global_resolution (({ store_info; _ } as t): t) class_name method_postcondition =
    (*
    여기서 postcondition의 변수 하나하나를 저장한다   
    *)
    let current_postcondition = StoreMap.find store_info class_name |> Option.value ~default:(Refinement.Store.empty) in
    set t ~key:class_name ~data:(Refinement.Store.join_with_merge ~global_resolution current_postcondition method_postcondition)

  let outer_widen ~global_resolution ({ store_info; _ } as t) class_name method_postcondition =
    let current_postcondition = StoreMap.find store_info class_name |> Option.value ~default:(Refinement.Store.empty) in
    { t with store_info = StoreMap.set ~key:class_name ~data:(
        Refinement.Store.outer_widen ~global_resolution current_postcondition method_postcondition ~iteration:0 ~widening_threshold:3
      ) store_info
    }

  let join ~global_resolution left right =
    let store_info_merge ~key data =
      let _ = key in
      match data with
      | `Left v | `Right v -> Some v
      | `Both (v1, v2) -> Some (Refinement.Store.join_with_merge ~global_resolution v1 v2)
    in

    let class_vartype_merge ~key data =
      let _ = key in
      match data with
      | `Left v | `Right v -> Some v
      | `Both (v1, v2) ->
        let vartype_map_merge ~key data =
          let _ = key in 
          (match data with
          | `Left v | `Right v -> Some v
          | `Both (v1, v2) -> Some (FunctionSet.union v1 v2)
          )
        in
        Some (VarTypeMap.merge v1 v2 ~f:vartype_map_merge)
    in

    {
      store_info=StoreMap.merge left.store_info right.store_info ~f:store_info_merge;
      class_vartype=ClassMap.merge left.class_vartype right.class_vartype ~f:class_vartype_merge; 
    }

  let pp_functionset format functionset = 
    FunctionSet.iter ~f:(fun func_name -> 
    Format.fprintf format "%a, " Reference.pp func_name) functionset

  let pp_vartype_map format vartype_map =
    VarTypeMap.iteri ~f:(fun ~key:(var_name, typ) ~data ->
    Format.fprintf format "%a: %a => { %a }\n" Reference.pp var_name Type.pp typ pp_functionset data
  ) vartype_map

  let pp_class_vartype format { class_vartype; _ } =
    ClassMap.iteri ~f:(fun ~key ~data ->
      Format.fprintf format "[[[ Class VarType: %a ]]] \n%a\n" Reference.pp key pp_vartype_map data
    ) class_vartype
 
  let pp_store_info format { store_info; _ } =
    StoreMap.iteri ~f:(fun ~key ~data ->
      Format.fprintf format "[[[ Class Info ]]] \n %a \n -> %a \n" Reference.pp key Refinement.Store.pp data
  ) store_info

  let pp format t =
    Format.fprintf format "%a\n%a" pp_store_info t pp_class_vartype t 

    (*
  let store_info_json_string { store_info; _ } =
    let json_string =
    Hashtbl.fold store_info ~init:"{" ~f:(fun ~key ~data json_string ->
      json_string ^ (Format.asprintf {|"%a" : {%a},|} Reference.pp key Refinement.Store.to_yojson data)
    )
    in
    let json_string = String.chop_suffix_exn json_string ~suffix:"," in
    let json_string = json_string ^ "}" in
    json_string

  let function_set_json_string function_set =
    let json_string = FunctionSet.fold function_set ~init:"{" ~f:(fun json_string func_name ->
      json_string ^ (Format.asprintf {|%a,|} Reference.pp func_name)
    )
    in
    let json_string = String.chop_suffix_exn json_string ~suffix:"," in
    let json_string = json_string ^ "}" in
    json_string

  let vartype_map_json_string vartype_map =
    let json_string = VarTypeMap.fold vartype_map ~init:"{" ~f:(fun ~key ~data json_string -> 
      json_string ^ (Format.asprintf {|"%s" : %s,|} (VarType.json_string key) (function_set_json_string data))
    )
    in
    let json_string = String.chop_suffix_exn json_string ~suffix:"," in
    let json_string = json_string ^ "}" in
    json_string

  let class_vartype_json_string { class_vartype; _ } =
    let json_string =
    Hashtbl.fold class_vartype ~init:"{" ~f:(fun ~key ~data json_string ->
      json_string ^ (Format.asprintf {|"%a : %s|} Reference.pp key (vartype_map_json_string data))
    )
    in
    let json_string = String.chop_suffix_exn json_string ~suffix:"," in
    let json_string = json_string ^ "}" in
    json_string
    *)



  let equal t1 t2 =
    StoreMap.equal (fun ref1 ref2 -> Refinement.Store.equal ref1 ref2) t1.store_info t2.store_info

    (*
  let copy t = 
    { 
      store_info = StoreMap.copy t.store_info;
      class_vartype = t.class_vartype;
    }
    *)

  let update_map_function_of_types ({ class_vartype; _ } as t) class_name vartype_map =
    let class_vartype_map = ClassMap.find class_vartype class_name |> Option.value ~default:(VarTypeMap.empty) in
    let class_vartype_map = 
      VarTypeMap.merge ~f:(fun ~key:_ data -> 
      match data with
      | `Left d | `Right d -> Some d
      | `Both (left, right) -> Some (FunctionSet.union left right)
    ) class_vartype_map vartype_map 
    in
    { t with class_vartype = ClassMap.set ~key:class_name ~data:class_vartype_map class_vartype }

  let find_map_function_of_types { class_vartype; _ } class_name var_name var_type =
    let class_vartype_map = ClassMap.find class_vartype class_name |> Option.value ~default:(VarTypeMap.empty) in
    if VarTypeMap.mem class_vartype_map (var_name, var_type)
    then VarTypeMap.find_exn class_vartype_map (var_name, var_type)  
    else FunctionSet.empty

end

module ArgTypeMap = Map.Make (Identifier)

module ArgTypes = struct
  type t = Type.t ArgTypeMap.t [@@deriving sexp]

  let create () = ArgTypeMap.empty

  let add_arg_type t ident typ =
    let modified_typ = weaken_typ typ in
    let exn_typ = ArgTypeMap.find t ident |> Option.value ~default:modified_typ in
    match exn_typ with
    | Bottom | Any | Top -> t
    | _ ->
    ArgTypeMap.set ~key:ident ~data:(Type.union [modified_typ; exn_typ]) t
(*
  let modify_arg_typ t ident typ =
    Hashtbl.set t ~key:ident ~data:typ
*)

  let equal t1 t2 =
    ArgTypeMap.equal (fun typ1 typ2 -> Type.equal typ1 typ2) t1 t2

  let join left right =
    ArgTypeMap.merge left right ~f:(fun ~key:_ data ->
      match data with
      | `Left t | `Right t -> Some t
      | `Both (t1, t2) -> Some (Type.union_join t1 t2)
    ) 

  let pp format t =
    ArgTypeMap.iteri ~f:(fun ~key ~data ->
      Format.fprintf format "%a -> %a \n" Identifier.pp key Type.pp data;
    ) t

  let get_type t ident =
    ArgTypeMap.find t ident |> Option.value ~default:Type.Bottom

  let export_to_resolution t resolution = 
    ArgTypeMap.fold ~f:(fun ~key ~data resolution ->
        Resolution.new_local resolution ~reference:(Reference.create key) ~annotation:(Annotation.create_immutable data)
    ) t ~init:resolution

end

module FunctionSummary = struct
  type t = {
    arg_types : ArgTypes.t;
    return_types : Type.t;
    possible_condition : Refinement.Store.t;
    usedef_tables : UsedefStruct.t option;
    cfg : Cfg.t option;
  } [@@deriving sexp]

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

  let join ~global_resolution left right =
    (*
    * type 정보만 다를 뿐, usedef_tables와 cfg는 반드시 같아야 한다   
    *)
    let usedef_tables = 
      (match left.usedef_tables, right.usedef_tables with
      | None, None -> None
      | Some t1, Some t2 -> 
        if UsedefStruct.equal ~f:UsedefState.equal t1 t2 then Some t1 else raise NotEqualException
      | Some t, None | None, Some t -> Some t
      )
    in
    let cfg =
      match left.cfg, right.cfg with
      | None, None -> None
      | Some c1, Some c2 -> 
        if Hashtbl.equal Cfg.Node.location_insensitive_equal c1 c2 then Some c1 else raise NotEqualException
      | Some t, None | None, Some t -> Some t
      
    in

    (*Log.dump "%a\n%a\n" Refinement.Store.pp left.possible_condition Refinement.Store.pp right.possible_condition;*)

    {
      arg_types = ArgTypes.join left.arg_types right.arg_types;
      return_types = Type.union_join left.return_types right.return_types;
      possible_condition = Refinement.Store.join_with_merge ~global_resolution left.possible_condition right.possible_condition;
      usedef_tables;
      cfg;
    }

  let pp format {arg_types; return_types; possible_condition; _} =
    Format.fprintf format "%a \nreturn : %a \n\n%a\n" ArgTypes.pp arg_types Type.pp return_types Refinement.Store.pp possible_condition

  let add_arg_types ({arg_types; _} as t) arg_typ_list =
    { t with arg_types = List.fold arg_typ_list ~init:arg_types ~f:(fun arg_types (arg, typ) -> ArgTypes.add_arg_type arg_types arg typ) }

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

module FunctionSummaryMap = Map.Make (Reference)

module FunctionTable = struct
  type t = FunctionSummary.t FunctionSummaryMap.t [@@deriving sexp]
  let create () = FunctionSummaryMap.empty

  (*let copy t = Hashtbl.copy t*)

  let equal t1 t2 =
    FunctionSummaryMap.equal (fun fs1 fs2 -> 
      FunctionSummary.equal fs1 fs2
    ) t1 t2

  let join ~global_resolution left right = 
    FunctionSummaryMap.merge left right ~f:(fun ~key:_ data ->
      match data with
      | `Left v | `Right v -> Some v
      | `Both (v1, v2) -> Some (FunctionSummary.join ~global_resolution v1 v2)
    )

  let pp format table =
    FunctionSummaryMap.iteri ~f:(fun ~key ~data ->
      Format.fprintf format "[[[ Function Info ]]] \n%a \n%a \n" Reference.pp key FunctionSummary.pp data
    ) table

  let add_arg_types t reference arg_typ_list =
    let func_summary = FunctionSummaryMap.find t reference |> Option.value ~default:(FunctionSummary.create ()) in
    let func_summary = FunctionSummary.add_arg_types func_summary arg_typ_list in
    FunctionSummaryMap.set ~key:reference ~data:func_summary t

  let add_return_info t func return_type =
    let func_summary = FunctionSummaryMap.find t func |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummaryMap.set ~key:func ~data:(FunctionSummary.add_return_info func_summary return_type) t

  let set_possible_condition t func possible_condition =
    let func_summary = FunctionSummaryMap.find t func |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummaryMap.set ~key:func ~data:(FunctionSummary.set_possible_condition func_summary possible_condition) t

  let set_usedef_tables t func usedef_tables =
    let func_summary = FunctionSummaryMap.find t func |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummaryMap.set ~key:func ~data:(FunctionSummary.set_usedef_tables func_summary usedef_tables) t

  let set_cfg t func cfg =
    let func_summary = FunctionSummaryMap.find t func |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummaryMap.set ~key:func ~data:(FunctionSummary.set_cfg func_summary cfg) t

  let get_usedef_tables t func_name =
    let func_summary = FunctionSummaryMap.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_usedef_tables func_summary

  let get_possible_condition t func_name =
    let func_summary = FunctionSummaryMap.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_possible_condition func_summary

  let get_func_arg_types t func_name =
    let func_summary = FunctionSummaryMap.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_arg_types func_summary

  let get_func_return_types t func_name =
    let func_summary = FunctionSummaryMap.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_return_types func_summary

  let get_cfg t func_name =
    let func_summary = FunctionSummaryMap.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_cfg func_summary

  let make_map_function_of_types t class_name =
    (*
    make reference + type => function map   
    *)
    let vartype_map = VarTypeMap.empty in

    FunctionSummaryMap.fold ~f:(fun ~key:func_name ~data vartype_map ->
      if Reference.is_contain ~base:func_name ~target:class_name then
        let candidates_vartype = FunctionSummary.make_map_function_of_types data in
        List.fold candidates_vartype ~init:vartype_map ~f:(fun vartype_map (str_list, anno) -> 
          VarTypeMap.update vartype_map (Reference.create_from_list str_list, Option.value_exn anno) ~f:(fun func_set -> 
            FunctionSet.add (Option.value func_set ~default:FunctionSet.empty) func_name
          ) 
        )
      else
        vartype_map
    ) t ~init:vartype_map
    (*
    Hashtbl.fold t ~init:vartype_map ~f:(fun ~key:func_name ~data vartype_map ->
      if Reference.is_contain ~base:func_name ~target:class_name then
        let candidates_vartype = FunctionSummary.make_map_function_of_types data in
        List.fold candidates_vartype ~init:vartype_map ~f:(fun vartype_map (str_list, anno) -> 
          VarTypeMap.update (Reference.create_from_list str_list, Option.value_exn anno) (fun func_set_opt -> 
            Some (FunctionSet.add func_name (Option.value func_set_opt ~default:FunctionSet.empty))
          ) vartype_map
        )
      else
        vartype_map
    )
    *)

end

module OurSummary = struct
  type t = {
    class_summary : ClassSummary.t;
    function_table : FunctionTable.t;
  }
  [@@deriving equal, sexp]

  let create () = {
    class_summary = ClassSummary.create (); 
    function_table = FunctionTable.create (); 
  }

  let class_summary {class_summary; _} = class_summary

  let set_class_summary t class_summary =
    { t with class_summary; }

  let join_with_merge_function_possiblecondition ~global_resolution ({function_table; _ } as t) func_name possiblecondition =
    let update_function_table = 
      FunctionTable.get_possible_condition function_table func_name 
      |> Refinement.Store.join_with_merge ~global_resolution possiblecondition
      |> FunctionTable.set_possible_condition function_table func_name
    in
    { t with function_table=update_function_table }

    (*
  let copy {class_summary; function_table; current_function; current_possiblecondition } = 
    {
      class_summary = ClassSummary.copy class_summary;
      function_table = FunctionTable.copy function_table;
      current_function;
      current_possiblecondition
    }
    *)
  
  let equal {class_summary=cs1; function_table=ft1; _} {class_summary=cs2; function_table=ft2; _} =
    (ClassSummary.equal cs1 cs2) && (FunctionTable.equal ft1 ft2)

  let join ~global_resolution left right = 
    {
      class_summary = ClassSummary.join ~global_resolution left.class_summary right.class_summary;
      function_table = FunctionTable.join ~global_resolution left.function_table right.function_table;
    }

  let pp_class format {class_summary; _} =
    Format.fprintf format "%a" ClassSummary.pp class_summary
  let pp_func format {function_table; _} = 
    Format.fprintf format "%a" FunctionTable.pp function_table

  let pp formatter t =
    Format.fprintf formatter "%a\n\n%a" pp_class t pp_func t

  let add_arg_types ({ function_table; _} as t) reference arg_typ_list =
    { t with function_table = FunctionTable.add_arg_types function_table reference arg_typ_list }

  let add_return_info ({function_table; _} as t) func_name return_type =
    { t with function_table = FunctionTable.add_return_info function_table func_name return_type }

  let set_possible_condition ({function_table; _} as t) func_name possible_condition =
    { t with function_table=FunctionTable.set_possible_condition function_table func_name possible_condition }
  let set_usedef_tables ({function_table; _} as t) func_name usedef_tables =
    { t with function_table=FunctionTable.set_usedef_tables function_table func_name usedef_tables }

  let set_cfg ({function_table; _} as t) func_name cfg =
    { t with function_table=FunctionTable.set_cfg function_table func_name cfg }

  let get_usedef_tables {function_table; _} func_name = 
    FunctionTable.get_usedef_tables function_table func_name

  let get_possible_condition {function_table; _} func_name = 
    FunctionTable.get_possible_condition function_table func_name

  let get_func_arg_types {function_table; _} func_name =
    FunctionTable.get_func_arg_types function_table func_name

  let get_func_return_types {function_table; _} func_name =
    FunctionTable.get_func_return_types function_table func_name

  let get_cfg {function_table; _} func_name =
    FunctionTable.get_cfg function_table func_name

  let make_map_function_of_types { function_table; _ } class_name =
    FunctionTable.make_map_function_of_types function_table class_name

  let update_map_function_of_types ({ class_summary; _ } as t) class_name vartype_map =
    { t with class_summary = ClassSummary.update_map_function_of_types class_summary class_name vartype_map }

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


let check_dir : string -> bool 
= fun path ->
  match Sys.is_directory path with
  | `Yes -> true
  | _ -> false

let check_and_make_dir : string -> unit
= fun path ->
  if check_dir path then ()
  else Unix.mkdir path

(*
let check_file : string -> bool
= fun path ->
match Sys.file_exists path with
| `Yes -> true
| _ -> false
  *)
  
let data_path = ref ""

let set_data_path (configuration: Configuration.Analysis.t) =
  if String.equal !data_path "" then
    data_path :=
      (List.nth_exn configuration.source_paths 0 
      |> SearchPath.get_root
      |> PyrePath.show) ^ "/pyinder_analysis"


let our_model = ref (OurSummary.create ());;

let cache = ref false;;

let is_search_mode = String.equal "search"

let is_inference_mode = String.equal "inference"

let single_errors = ref [];;

let global_resolution = ref None;;

let save_mode (mode: string) =
  check_and_make_dir !data_path;
  let target_path = !data_path ^ "/mode" ^ ".marshalled" in
  let data_out = open_out target_path in
  let sexp = String.sexp_of_t mode in
  Marshal.to_channel data_out sexp [];
  close_out data_out

let load_mode () =
  let data_in = open_in (!data_path ^ "/mode" ^ ".marshalled") in
  let mode = String.t_of_sexp (Marshal.from_channel data_in) in
  close_in data_in;
  mode

let save_summary (summary: OurSummary.t) func_name =
  check_and_make_dir !data_path;
  let target_path = !data_path ^ "/" ^ (Reference.show func_name) ^ ".marshalled" in
  let data_out = open_out target_path in
  let sexp = OurSummary.sexp_of_t summary in
  Marshal.to_channel data_out sexp [];
  close_out data_out

let load_summary func_name =
  let data_in = open_in (!data_path ^ "/" ^ (Reference.show func_name) ^ ".marshalled") in
  let our_summary = OurSummary.t_of_sexp (Marshal.from_channel data_in) in
  close_in data_in;
  our_summary

let load_all_summary global_resolution =
  if !cache then
    ()
  else
  (
    cache := true;
    let list_files = Sys.readdir !data_path |> Array.to_list in 
    our_model := List.fold list_files ~init:(OurSummary.create ()) ~f:(fun summary file -> 
      if String.equal file "mode.marshalled" then summary
      else
      (
        let data_in = open_in (!data_path ^ "/" ^ file) in
        let other_summary = OurSummary.t_of_sexp (Marshal.from_channel data_in) in
        close_in data_in;
        OurSummary.join ~global_resolution summary other_summary
      )
    )
  )

let select_our_model func_name =
  if is_inference_mode (load_mode ()) then
    load_summary func_name
  else 
    !our_model


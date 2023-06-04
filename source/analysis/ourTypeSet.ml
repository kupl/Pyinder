open Core
open Ast
open Ast.Util
open AttributeAnalysis
open OurDomain

let ( >>| ) = Option.( >>| )

module Store = Refinement.Store
module Unit = Refinement.Unit

(*
module VarType = struct
  type t = Reference.t * Type.t [@@deriving sexp, compare]
end

module StringSet = Set.Make (String)
module VarTypeMap = struct 
  include Map.Make (VarType)
  
  let join left right ~f =
    let merge_f ~key:_ data = 
      (match data with
      | `Left v | `Right v -> Some v
      | `Both (v1, v2) -> Some (f v1 v2)
      )
    in
    merge left right ~f:merge_f
  
end

module FunctionSet = ReferenceSet
module StoreMap = ReferenceMap
module ClassMap = ReferenceMap

module AttrsSet = StringSet
*)


module ClassSummaryResolution = struct
  include ClassSummary

  (*
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
  *)

  let join ~type_join left right =
    {
      class_var_type = ReferenceMap.join left.class_var_type right.class_var_type ~data_join:type_join;
      class_attributes = ClassAttributes.join left.class_attributes right.class_attributes;
      usage_attributes = AttributeStorage.join left.usage_attributes right.usage_attributes;
    }

  let get_type_of_class_attribute ~attribute { class_var_type; _ } =
    let name = Reference.create attribute in
    ReferenceMap.find class_var_type name |> Option.value ~default:Type.Unknown

  let get_self_attributes_tree t =
    (*let name = Reference.create "$parameter$self" in*)
    let reference_map = 
      get_class_var_type t
    in
    ReferenceMap.fold ~init:Unit.empty ~f:(fun ~key ~data sofar ->
        Unit.set_annotation ~attribute_path:key ~annotation:(Annotation.create_mutable data) sofar
    ) reference_map
    |> Unit.attributes

  let add_class_var_type ~type_join class_var_type reference typ =
    ReferenceMap.update class_var_type reference ~f:(fun origin ->
      origin |> Option.value ~default:Type.Bottom |> type_join typ
    )

  let add_parent_attributes ({ class_attributes; usage_attributes; _ } as t) storage =
    let storage = 
      AttributeStorage.filter_keys storage ~f:(fun key -> 
        match Expression.get_identifier_base key with
        | Some base -> ClassAttributes.is_used_attr class_attributes base
        | _ -> false
      ) 
    in
    { t with usage_attributes=AttributeStorage.join usage_attributes storage }
    
  let join_with_merge_class_var_type ~type_join ({ class_var_type; _ } as t) class_param (method_postcondition: Refinement.Store.t) =
    (*
    여기서 postcondition의 변수 하나하나를 저장한다   
    *)


    let filter_keys = Reference.create class_param in

    let rec attribute_fold ~base_reference ~attributes class_var_type =
      Identifier.Map.Tree.fold ~init:class_var_type ~f:(fun ~key ~data class_var_type ->
        unit_fold ~unit:data ~base_reference:(Reference.combine base_reference (Reference.create key)) class_var_type
      ) attributes
    
    and unit_fold ~unit ~base_reference class_var_type =
      let typ = unit |> Refinement.Unit.base >>| Annotation.annotation |> Option.value ~default:Type.Unknown in
      let class_var_type = add_class_var_type ~type_join class_var_type base_reference typ in
      attribute_fold ~base_reference ~attributes:(unit |> Refinement.Unit.attributes) class_var_type
    in 
    
    let annotation_fold annotation class_var_type =
      Reference.Map.fold ~init:class_var_type ~f:(fun ~key ~data class_var_type ->
        if Reference.is_prefix ~prefix:filter_keys key then (
          let x = unit_fold ~unit:data ~base_reference:Reference.empty class_var_type in
          x
        )
        else
          class_var_type
      ) annotation
    in

    let class_var_type = 
      let x =
      ReferenceMap.empty
      |> annotation_fold method_postcondition.annotations
      |> annotation_fold method_postcondition.temporary_annotations
      in

      x
      |> ReferenceMap.join ~data_join:type_join class_var_type
    in

    ClassSummary.{ t with class_var_type }

end

module ClassTableResolution = struct
  include ClassTable

  let join ~type_join left right =
    ClassMap.join left right ~data_join:(ClassSummary.join ~type_join)

  let add_parent_attributes t class_name storage = 
    let class_info = find_default t class_name in
    ClassMap.set t ~key:class_name ~data:(ClassSummaryResolution.add_parent_attributes class_info storage)

  let get_type_of_class_attribute t class_name attribute = 
    let class_info = find_default t class_name in
    ClassSummaryResolution.get_type_of_class_attribute class_info ~attribute

  let get_self_attributes_tree t class_name = 
    let class_info = find_default t class_name in
    ClassSummaryResolution.get_self_attributes_tree class_info

  let join_with_merge_class_var_type ~type_join t class_name class_param method_postcondition =
    let class_summary = find_default t class_name in
    let data = ClassSummaryResolution.join_with_merge_class_var_type ~type_join class_summary class_param method_postcondition in
    ClassMap.set ~key:class_name ~data t

  let find_classes_from_attributes t attributes =
    ClassMap.fold t ~init:[] ~f:(fun ~key ~data:{ ClassSummary.class_attributes; _ } candidate_classes ->
      if ClassAttributes.is_subset_with_total_attributes class_attributes attributes
      then key::candidate_classes
      else candidate_classes  
    )

  let get_analysis_set ~get_functions prev next =
    ClassMap.fold2 prev next ~init:ReferenceSet.empty ~f:(fun ~key ~data ref_set ->
      match data with
      | `Right _ -> 
        get_functions key |> ReferenceSet.union ref_set
      | `Both (prev, next) -> 
        (
        if not (ClassSummary.equal prev next)
        then get_functions key
        else ReferenceSet.empty
        ) |> ReferenceSet.union ref_set 
      | `Left _ -> failwith "Why prev is bigger?"
    )
end

let weaken_typ typ =
  let weaken_typ = Type.weaken_literals typ in
  let weaken_typ =
    match weaken_typ with
    | Type.IntExpression _ -> Type.Primitive "int"
    | _ -> weaken_typ
  in
  weaken_typ

module ArgTypesResolution = struct
  include ArgTypes

  let add_arg_type ~join t ident typ =
    let modified_typ = weaken_typ typ in
    let exn_typ = IdentifierMap.find t ident |> Option.value ~default:modified_typ in
    match exn_typ with
    | Bottom | Any | Top | Unknown -> t
    | _ ->
      IdentifierMap.set ~key:ident ~data:(join modified_typ exn_typ) t

  let join ~type_join left right =
    IdentifierMap.merge left right ~f:(fun ~key:_ data ->
      match data with
      | `Left t | `Right t -> Some t
      | `Both (t1, t2) -> Some (type_join t1 t2)
    ) 

  let import_from_resolution ~join resolution =
    let annotation_store = Resolution.get_annotation_store resolution in

    let iter_base annotation arg_types =
      Reference.Map.fold annotation ~init:arg_types ~f:(fun ~key ~data arg_types ->
        match data |> Unit.base with
        | None -> arg_types
        | Some annotation -> ArgTypes.add_arg_type ~join arg_types (key |> Reference.show) (Annotation.annotation annotation)
      )
    in

    ArgTypes.empty
    |> iter_base annotation_store.annotations
    |> iter_base annotation_store.temporary_annotations


  let export_to_resolution t resolution = 
    IdentifierMap.fold ~f:(fun ~key ~data resolution ->
        Resolution.new_local resolution ~reference:(Reference.create key) ~annotation:(Annotation.create_immutable data)
    ) t ~init:resolution

  let join_to_resolution ~join t resolution = 
    IdentifierMap.fold ~f:(fun ~key ~data resolution ->
      let reference = Reference.create key in
      let old_type = Resolution.resolve_reference resolution reference in
      let new_type = join data old_type in
      Resolution.refine_local resolution ~reference ~annotation:(Annotation.create_mutable new_type)
    ) t ~init:resolution

end

module FunctionSummaryResolution = struct
  include FunctionSummary

  let add_arg_types ~join ({arg_types; _} as t) arg_typ_list =
    { t with arg_types = List.fold arg_typ_list ~init:arg_types ~f:(fun arg_types (arg, typ) -> ArgTypes.add_arg_type ~join arg_types arg typ) }

  let join ~type_join left right = 
    (*
    let usedef_tables = 
      (match left.usedef_tables, right.usedef_tables with
      | None, None -> None
      | Some t1, Some t2 -> 
        if UsedefStruct.equal t1 t2 then Some t1 else raise NotEqualException
      | Some t, None | None, Some t -> Some t
      )
    in
    *)
    
  {
    arg_types=ArgTypesResolution.join ~type_join left.arg_types right.arg_types;
    arg_annotation=ArgTypesResolution.join ~type_join left.arg_annotation right.arg_annotation;
    return_var_type=ReferenceMap.join ~data_join:type_join left.return_var_type right.return_var_type;
    return_type=type_join left.return_type right.return_type;
    callers=CallerSet.union left.callers right.callers;
    usage_attributes=AttributeStorage.join left.usage_attributes right.usage_attributes;
    (*usedef_tables=usedef_tables;*)
  }

  let join_return_type ~type_join ({return_type=origin; _} as t) return_type =
    { t with return_type=type_join origin return_type; }

  let store_to_return_var_type ({ return_var_type; _ } as t) (store: Refinement.Store.t) =
    (* parameter만 *)
    let rec attribute_fold ~base_reference ~attributes return_var_type =
      Identifier.Map.Tree.fold ~init:return_var_type ~f:(fun ~key ~data return_var_type ->
        unit_fold ~unit:data ~base_reference:(Reference.combine base_reference (Reference.create key)) return_var_type
      ) attributes
    
    and unit_fold ~unit ~base_reference return_var_type =
      let typ = unit |> Refinement.Unit.base >>| Annotation.annotation |> Option.value ~default:Type.Unknown in
      let return_var_type = ReferenceMap.set return_var_type ~key:base_reference ~data:typ in
      attribute_fold ~base_reference ~attributes:(unit |> Refinement.Unit.attributes) return_var_type
    in 
    
    let annotation_fold annotation return_var_type =
      Reference.Map.fold ~init:return_var_type ~f:(fun ~key ~data return_var_type ->
        if Reference.is_parameter key then
          let x = unit_fold ~unit:data ~base_reference:key return_var_type in
          x
        else
          return_var_type
      ) annotation
    in

    let return_var_type = 
      let x =
      return_var_type
      |> annotation_fold store.annotations
      |> annotation_fold store.temporary_annotations
      in
      x
    in

    FunctionSummary.{ t with return_var_type; }

  let find_class_of_attributes ~class_table { usage_attributes; _ } parent_usage_attributes =
    let identifier_to_string t =
      Identifier.Set.fold t ~init:AttrsSet.empty ~f:(fun set attr -> AttrsSet.add set attr)
    in

    (* have to make proper filter *)
    let extract_class classes =
      List.nth classes 0
    in
    let usage_attributes =
      parent_usage_attributes
      |> AttributeStorage.join usage_attributes
    in
    
    
    
    AttributeStorage.map usage_attributes ~f:(fun attributes -> 
        attributes
        |> identifier_to_string
        |> ClassTableResolution.find_classes_from_attributes class_table
        |> extract_class
    )
    |> LocInsensitiveExpMap.filter_map ~f:(fun v -> 
      match v with
      | Some v -> Some v
      | _ -> v  
    )

end

module FunctionTableResolution = struct
  include FunctionTable

  let add_arg_types ~join t reference arg_typ_list =
    let func_summary = FunctionMap.find t reference |> Option.value ~default:FunctionSummary.empty in
    let func_summary = FunctionSummaryResolution.add_arg_types ~join func_summary arg_typ_list in
    FunctionMap.set ~key:reference ~data:func_summary t

  let join ~type_join left right =
    ReferenceMap.join left right ~data_join:(FunctionSummaryResolution.join ~type_join)

  let join_return_type ~type_join t func return_type =
    let func_summary = FunctionMap.find t func |> Option.value ~default:FunctionSummary.empty in
    FunctionMap.set ~key:func ~data:(FunctionSummaryResolution.join_return_type ~type_join func_summary return_type) t



  let store_to_return_var_type t func_name (store: Refinement.Store.t) =
    let func_summary = FunctionMap.find t func_name |> Option.value ~default:FunctionSummary.empty in
    FunctionMap.set t ~key:func_name ~data:(FunctionSummaryResolution.store_to_return_var_type func_summary store)

  let find_class_of_attributes ~class_table (t: t) func_name parent_usage_attributes =
    let func_summary = FunctionMap.find t func_name |> Option.value ~default:FunctionSummary.empty in
    FunctionSummaryResolution.find_class_of_attributes ~class_table func_summary parent_usage_attributes

end

module OurSummaryResolution = struct
  include OurSummary

  let join ~type_join left right = 
    {
      class_table = ClassTableResolution.join ~type_join left.class_table right.class_table;
      function_table = FunctionTableResolution.join ~type_join left.function_table right.function_table;
    }

  let join_return_type ~type_join ({function_table; _} as t) func_name return_type =
    { t with function_table = FunctionTable.join_return_type ~type_join function_table func_name return_type }

  let add_arg_types ~join ({ function_table; _} as t) reference arg_typ_list =
    { t with function_table = FunctionTable.add_arg_types ~join function_table reference arg_typ_list }

  let get_analysis_set prev next =
    let get_functions =
      FunctionTable.get_functions next.function_table
    in
    ReferenceSet.union
      (ClassTable.get_analysis_set ~get_functions prev.class_table next.class_table)
      (FunctionTable.get_analysis_set prev.function_table next.function_table)
    
  let get_skip_set prev next =
    let analysis_set = get_analysis_set prev next in
    let get_all_functions = FunctionTable.get_all_functions next.function_table in
    ReferenceSet.diff get_all_functions analysis_set

  let store_to_return_var_type ({ function_table; _ } as t) func_name store = 
    { t with function_table=FunctionTableResolution.store_to_return_var_type function_table func_name store; }

  let get_type_of_class_attribute { class_table; _ } class_name attribute = ClassTableResolution.get_type_of_class_attribute class_table class_name attribute
  
  let get_self_attributes_tree { class_table; _ } class_name = ClassTableResolution.get_self_attributes_tree class_table class_name

  let add_parent_attributes ({ class_table; _ } as t) storage class_name class_var =
    (* 이거 짜야 댕 *)
    let filtered_storage = AttributeStorage.filter_by_prefix storage ~prefix:(Reference.create class_var) in
    let class_table = ClassTableResolution.add_parent_attributes class_table class_name filtered_storage in
    { t with class_table; }


  (*
  let search_suspicious_variable t ~store_combine parent =
    (*let usedef_table = get_usedef_table t func_name |> Option.value ~default:UsedefState.bottom in*)
    let possible_condition = ClassTableResolution.get_class_var_type t.class_table parent in
    let total_annotation = store_combine possible_condition in
    
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
  *)

  let find_class_of_attributes { class_table; function_table; } func_name parent_usage_attributes  =
    FunctionTableResolution.find_class_of_attributes ~class_table function_table func_name parent_usage_attributes
end
(*
let global_summary = "Pyinder.finalSummary"
let check_dir : string -> bool 
= fun path ->
  match Sys.is_directory path with
  | `Yes -> true
  | _ -> false

let check_and_make_dir : string -> unit
= fun path ->
  if check_dir path then ()
  else Unix.mkdir path
let data_path = ref ""

let is_func_model_exist () = check_dir !data_path

let set_data_path (configuration: Configuration.Analysis.t) =
  if String.equal !data_path "" then
    data_path :=
      (List.nth_exn configuration.source_paths 0 
      |> SearchPath.get_root
      |> PyrePath.show) ^ "/pyinder_analysis"
let cache = ref false;;

let load_all_summary ?(use_cache=true) ~type_join ~skip_set prev_model =
  if use_cache && !cache then
    ()
  else
  (
    cache := true;
    let list_files = Sys.readdir !data_path |> Array.to_list in 
    our_model := List.fold list_files ~init:prev_model ~f:(fun summary file -> 
      if (String.equal file "mode.marshalled") || 
        (Reference.Set.exists skip_set ~f:(fun ref -> String.is_prefix file ~prefix:(Reference.show ref)))
      then (
        summary
      )
      else
      (
        let data_in = open_in (!data_path ^ "/" ^ file) in
        let other_summary = OurSummary.t_of_sexp (Marshal.from_channel data_in) in
        close_in data_in;
        OurSummary.join ~type_join summary other_summary
      )
    )
  )
*)
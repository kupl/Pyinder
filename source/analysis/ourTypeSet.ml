open Core
open Ast
open Ast.Util
open Usedef
open AttributeAnalysis

exception DuplicateException;;
exception NotEqualException;;

module Store = Refinement.Store
module Unit = Refinement.Unit

module VarType = struct
  type t = Reference.t * Type.t [@@deriving sexp, compare]
end

module ReferenceSet = Set.Make (Reference)
module ReferenceMap = Map.Make (Reference)
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


let weaken_typ typ =
  let weaken_typ = Type.weaken_literals typ in
  let weaken_typ =
    match weaken_typ with
    | Type.IntExpression _ -> Type.Primitive "int"
    | _ -> weaken_typ
  in
  weaken_typ

let extract_self store =
  Refinement.Store.update_with_filter ~old_store:Store.empty ~new_store:store ~filter:(fun reference _ -> 
    String.is_suffix ~suffix:"$self" (Reference.last reference)
  )

let extract_parameter store =
  Refinement.Store.update_with_filter ~old_store:Store.empty ~new_store:store ~filter:(fun reference _ -> 
    Reference.is_parameter reference
  )

module ClassAttributes = struct
  type t = {
    attributes: AttrsSet.t;
    properties: AttrsSet.t;
    methods: AttrsSet.t;
  } [@@deriving sexp, equal]

  let empty = {
    attributes=AttrsSet.empty;
    properties=AttrsSet.empty;
    methods=AttrsSet.empty;
  }
  
  let pp_attrs_set format attrs_set =
    let attrs_string = (AttrsSet.fold attrs_set ~init:"{" ~f:(fun acc attr ->
      acc ^ ", " ^ attr
    )) ^ "}"
  in
  Format.fprintf format "%s" attrs_string

  let pp format { attributes; properties; methods; } =
    Format.fprintf format "
      [[ Attributes ]]\n%a\n
      [[ Properties ]]\n%a\n
      [[ Methods ]]\n%a\n
    "
    pp_attrs_set attributes pp_attrs_set properties pp_attrs_set methods

  let join left right =
    {
      attributes=AttrsSet.union left.attributes right.attributes;
      properties=AttrsSet.union left.properties right.properties;
      methods=AttrsSet.union left.methods right.methods;
    }

  let add_attribute ({ attributes; _} as t) attr =
    { t with attributes=AttrsSet.add attributes attr }

  let add_property ({ properties; _} as t) prop =
    { t with properties=AttrsSet.add properties prop }

  let add_method ({ methods; _} as t) meth =
    { t with methods=AttrsSet.add methods meth }

  let total_attributes { attributes; properties; methods; } =
    AttrsSet.union_list [attributes; properties; methods;]

  let is_used_attr { attributes; properties; methods; } attr =
    AttrsSet.exists (AttrsSet.union_list [attributes; properties; methods]) ~f:(fun elem -> String.equal elem attr)

  let is_subset_with_total_attributes t attributes =
    AttrsSet.is_subset attributes ~of_:(total_attributes t)
end 

module ClassInfo = struct
  type t = {
    class_variable_type : Store.t; (* 객체 변수들의 타입 정보 *)
    class_attributes : ClassAttributes.t; (* 클래스에서 사용 가능한 attribute의 Set *)
    class_vartype : FunctionSet.t VarTypeMap.t; (* ?? *)
    usage_attributes : AttributeStorage.t; (* 객체 변수가 사용한 attributes *)
  } [@@deriving sexp, equal]

  let empty = {
    class_variable_type = Store.empty;
    class_attributes = ClassAttributes.empty;
    class_vartype = VarTypeMap.empty;
    usage_attributes = AttributeStorage.empty;
  }

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
  
  let set_class_variable_type t ~data =
    {
      t with
      class_variable_type = (extract_element_type (extract_self data));
    }

    (*
  let set_class_vartype t class_vartype = { t with class_vartype }
*)

  let get_class_variable_type { class_variable_type; _ } = class_variable_type

  let get_usage_attributes { usage_attributes; _ } = usage_attributes
  let get_self_attributes_tree ~global_resolution t =
    let name = Reference.create "$parameter$self" in
    let reference_map = 
      get_class_variable_type t
      |> Refinement.Store.combine_join_with_merge ~global_resolution
    in
    Reference.Map.find reference_map name
    |> Option.value ~default:Unit.empty
    |> Unit.attributes


  let get_type_of_class_attribute ~attribute { class_variable_type; _ } =
    let name = Reference.create "$parameter$self" in
    Refinement.Store.get_annotation ~name ~attribute_path:(Reference.create attribute) class_variable_type

  let add_attribute ({ class_attributes; _} as t) attr =
    let class_attributes = ClassAttributes.add_attribute class_attributes attr in
    { t with class_attributes }

  let add_property ({ class_attributes; _} as t) property =
    let class_attributes = ClassAttributes.add_property class_attributes property in
    { t with class_attributes }

  let add_method ({ class_attributes; _} as t) meth =
    let class_attributes = ClassAttributes.add_method class_attributes meth in
    { t with class_attributes }

  let add_usage_attributes ({ usage_attributes; _ } as t) storage =
    { t with usage_attributes=AttributeStorage.join usage_attributes storage }

  let add_parent_attributes ({ class_attributes; usage_attributes; _ } as t) storage =
    let storage = 
      AttributeStorage.filter_keys storage ~f:(fun key -> 
        match Expression.get_identifier_base key with
        | Some base -> ClassAttributes.is_used_attr class_attributes base
        | _ -> false
      ) 
    in
    { t with usage_attributes=AttributeStorage.join usage_attributes storage }

    

  let outer_join_class_variable_type ~global_resolution ({ class_variable_type; _ } as t) method_postcondition =
    set_class_variable_type t ~data:(
      if Refinement.Store.less_or_equal ~global_resolution ~left:class_variable_type ~right:Refinement.Store.empty 
      then 
        method_postcondition
      else
        Refinement.Store.outer_join ~global_resolution class_variable_type method_postcondition
  )
    
  let join_with_merge_class_variable_type ~global_resolution ({ class_variable_type; _ } as t) method_postcondition =
    (*
    여기서 postcondition의 변수 하나하나를 저장한다   
    *)
    set_class_variable_type t ~data:(Refinement.Store.join_with_merge ~global_resolution class_variable_type method_postcondition)

  let join_class_vartype left right =
    VarTypeMap.join left right ~f:FunctionSet.union

  let join ~global_resolution left right =
    {
      class_variable_type = Store.join_with_merge ~global_resolution left.class_variable_type right.class_variable_type;
      class_attributes = ClassAttributes.join left.class_attributes right.class_attributes;
      class_vartype = join_class_vartype left.class_vartype right.class_vartype;
      usage_attributes = AttributeStorage.join left.usage_attributes right.usage_attributes;
    }

  let update_map_function_of_types ({ class_vartype; _ } as t) vartype =
    { t with class_vartype=join_class_vartype class_vartype vartype } 

  let find_map_function_of_types { class_vartype; _ } var_name var_type =
    if VarTypeMap.mem class_vartype (var_name, var_type)
    then VarTypeMap.find_exn class_vartype (var_name, var_type)  
    else FunctionSet.empty

  let pp_functionset format functionset = 
    FunctionSet.iter ~f:(fun func_name -> 
    Format.fprintf format "%a, " Reference.pp func_name) functionset

  let pp_class_vartype format { class_vartype; _ } =
    VarTypeMap.iteri ~f:(fun ~key:(var_name, typ) ~data ->
    Format.fprintf format "%a: %a => { %a }\n" Reference.pp var_name Type.pp typ pp_functionset data
  ) class_vartype

  let pp_store_info format { class_variable_type; _ } =
      Format.fprintf format "[[[ Class Variable Type ]]] \n%a\n" Store.pp class_variable_type

  let pp_class_info format { class_attributes; _ } =
      Format.fprintf format "[[[ Class Info ]]] \n%a\n" ClassAttributes.pp class_attributes

  let pp_usage_attributes format { usage_attributes; _ } =
      Format.fprintf format "[[[ Class Usage Attrs ]]] \n%a\n" AttributeStorage.pp usage_attributes

  let pp format t =
    Format.fprintf format "%a\n%a\n%a\n%a" pp_store_info t pp_class_vartype t pp_class_info t pp_usage_attributes t
end

module ClassSummary = struct
  type t = ClassInfo.t ClassMap.t [@@deriving sexp, equal]

  let empty = ClassMap.empty

  let find_default t name = ClassMap.find t name |> Option.value ~default:ClassInfo.empty 
  let add_attribute t class_name attr =
    let class_info = find_default t class_name in
    ClassMap.set t ~key:class_name ~data:(ClassInfo.add_attribute class_info attr)

  let add t ~class_name ~data ~f =
    let class_info = find_default t class_name in
    ClassMap.set t ~key:class_name ~data:(f class_info data)
    
  let add_property t class_name property = add t ~class_name ~data:property ~f:ClassInfo.add_property

  let add_method t class_name meth = add t ~class_name ~data:meth ~f:ClassInfo.add_method

  let add_usage_attributes t class_name storage = add t ~class_name ~data:storage ~f:ClassInfo.add_usage_attributes

  let add_parent_attributes t class_name storage = add t ~class_name ~data:storage ~f:ClassInfo.add_parent_attributes

  let set_class_info t class_name class_info =
    ClassMap.set t ~key:class_name ~data:class_info

  let get t ~class_name ~f = 
    let class_info = find_default t class_name in
    f class_info

  let get_class_info t class_name = get t ~class_name ~f:(fun x -> x)

  let get_self_attributes_tree ~global_resolution t class_name = get t ~class_name ~f:(ClassInfo.get_self_attributes_tree ~global_resolution)

  let get_class_variable_type t class_name = get t ~class_name ~f:ClassInfo.get_class_variable_type

  let get_usage_attributes t class_name = get t ~class_name ~f:ClassInfo.get_usage_attributes

  let get_type_of_class_attribute t class_name attribute = get t ~class_name ~f:(ClassInfo.get_type_of_class_attribute ~attribute)

  let outer_join_class_variable_type ~global_resolution t class_name method_postcondition =
    let class_info = find_default t class_name in
    let data = ClassInfo.outer_join_class_variable_type ~global_resolution class_info method_postcondition in
    ClassMap.set ~key:class_name ~data t

  let join_with_merge_class_variable_type ~global_resolution t class_name method_postcondition =
    let class_info = find_default t class_name in
    let data = ClassInfo.join_with_merge_class_variable_type ~global_resolution class_info method_postcondition in
    ClassMap.set ~key:class_name ~data t
    

  let join ~global_resolution left right =
    ClassMap.merge left right ~f:(fun ~key:_ data ->
      (match data with
      | `Left v | `Right v -> Some v
      | `Both (v1, v2) -> Some (ClassInfo.join ~global_resolution v1 v2)
      )
    )

  let update_map_function_of_types t class_name vartype =
    let class_info = find_default t class_name in
    let class_info = ClassInfo.update_map_function_of_types class_info vartype in
    ClassMap.set ~key:class_name ~data:class_info t

  let find_map_function_of_types t class_name var_name var_type =
    let class_info = find_default t class_name in
    ClassInfo.find_map_function_of_types class_info var_name var_type

  let find_classes_from_attributes t attributes =
    ClassMap.fold t ~init:[] ~f:(fun ~key ~data:{ ClassInfo.class_attributes; _ } candidate_classes ->
      if ClassAttributes.is_subset_with_total_attributes class_attributes attributes
      then key::candidate_classes
      else candidate_classes  
    )

  let pp format t =
    ClassMap.iteri ~f:(fun ~key ~data ->
      Format.fprintf format "[[[ Class : %a ]]] \n%a\n" Reference.pp key ClassInfo.pp data
    ) t
end

module ArgTypeMap = Map.Make (Identifier)

module ArgTypes = struct
  type t = Type.t ArgTypeMap.t [@@deriving sexp, equal]

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

(*
  let equal t1 t2 =
    ArgTypeMap.equal (fun typ1 typ2 -> Type.equal typ1 typ2) t1 t2
*)
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

  let join_to_resolution ~join t resolution = 
    ArgTypeMap.fold ~f:(fun ~key ~data resolution ->
      let reference = Reference.create key in
      let old_type = Resolution.resolve_reference resolution reference in
      let new_type = join data old_type in
      Resolution.refine_local resolution ~reference ~annotation:(Annotation.create_mutable new_type)
    ) t ~init:resolution

end

module FunctionSummary = struct
  type t = {
    arg_annotation : Refinement.Store.t; (* Annotation Type *)
    arg_types : ArgTypes.t; (* 실제 쓰인 타입 *)
    return_condition : Refinement.Store.t; (* Return 시 변수 정보들 *)
    return_annotation : Type.t; (* Return Annotation *)
    return_types : Type.t; (* 실제 쓰인 타입 *)
    usage_attributes : AttributeStorage.t;
    possible_condition : Refinement.Store.t;
    usedef_tables : UsedefStruct.t option;
    cfg : Cfg.t option;
  } [@@deriving sexp]

  let create () = { 
    arg_annotation = Refinement.Store.empty;
    arg_types = ArgTypes.create (); 
    return_condition = Refinement.Store.empty;
    return_annotation = Type.Top; 
    return_types = Type.Bottom; 
    usage_attributes = AttributeStorage.empty;
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

    
  let equal 
    {arg_annotation=arg_anno1; arg_types=ref1; return_condition=cond1; return_types=typ1; usage_attributes=use1; _ } 
    {arg_annotation=arg_anno2; arg_types=ref2; return_condition=cond2; return_types=typ2; usage_attributes=use2; _ } =
    (*
    Log.dump "%b %b %b %b %b" 
    (Refinement.Store.equal arg_anno1 arg_anno2) 
    (ArgTypes.equal ref1 ref2)  
    (Refinement.Store.equal cond1 cond2)
    (Type.equal typ1 typ2) (AttributeStorage.equal use1 use2);
    if not ((Type.equal typ1 typ2) ) then (
      Log.dump "%a \n vs \n %a" Type.pp typ1 Type.pp typ2;
      match typ1, typ2 with
      | Union t1_list, Union t2_list ->
        let _ =
        List.iter2 t1_list t2_list ~f:(fun t1 t2 -> 
            if not (Type.equal t1 t2) then (
              let rec find t1 t2 =
                Log.dump "%a\nvs (%b)\n%a" Type.pp t1 (Type.equal t1 t2) Type.pp t2;
                match t1, t2 with 
                | Parametric t1, Parametric t2 ->
                  let t1 = List.nth_exn t1.parameters 0 in
                  let t2 = List.nth_exn t2.parameters 0 in
                  (match t1, t2 with
                  | Single t1, Single t2 ->  find t1 t2
                  | _ -> Log.dump "No Single"
                  )
                | Variable t1, Variable t2 -> 

                  Log.dump "%b %b %b %b %b %b"
                    (Identifier.equal t1.variable t2.variable)
                    ([%equal: Type.t Type.Variable.constraints] t1.constraints t2.constraints)
                    ([%equal: Type.Variable.variance] t1.variance t2.variance)
                    ([%equal: Type.Variable.state] t1.state t2.state)
                    ([%equal: Type.Variable.RecordNamespace.t] t1.namespace t2.namespace)
                    ([%equal: Type.t Type.Variable.RecordUnary.record] t1 t2)
                | _ -> Log.dump "NO..."
              in
              find t1 t2
            );
            Log.dump "%a\nvs (%b)\n%a" Type.pp t1 (Type.equal t1 t2) Type.pp t2
          )
        in
        ()
      | _ -> ()      
    ); 
    *)
      (Refinement.Store.equal arg_anno1 arg_anno2)
      && (ArgTypes.equal ref1 ref2) 
      && (Refinement.Store.equal cond1 cond2)
      && (Type.equal typ1 typ2) 
      && (AttributeStorage.equal use1 use2)
  
  let update ~global_resolution left right =
    (*
    * type 정보만 다를 뿐, usedef_tables와 cfg는 반드시 같아야 한다   
    *)
    let usedef_tables = 
      (match left.usedef_tables, right.usedef_tables with
      | None, None -> None
      | Some t1, Some t2 -> 
        if UsedefStruct.equal t1 t2 then Some t1 else raise NotEqualException
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
    (*
    Log.dump "  [ 1 ]\n%a\n  [ 2 ]\n%a\n" Refinement.Store.pp left.arg_annotation Refinement.Store.pp right.arg_annotation;
    Log.dump "  [ Result ]\n%a" Refinement.Store.pp (Refinement.Store.meet ~global_resolution left.arg_annotation right.arg_annotation);
  *)
    {
      arg_annotation = Refinement.Store.meet ~global_resolution left.arg_annotation right.arg_annotation; (* annotation은 meet으로 줄여야 됨 *)
      arg_types = ArgTypes.join left.arg_types right.arg_types;
      return_condition = Refinement.Store.join_with_merge ~global_resolution left.return_condition right.return_condition;
      return_annotation = GlobalResolution.meet global_resolution left.return_annotation right.return_annotation; (* annotation은 meet으로 줄여야 됨 *)
      return_types = Type.union_join left.return_types right.return_types;
      usage_attributes = AttributeStorage.join left.usage_attributes right.usage_attributes;
      possible_condition = Refinement.Store.join_with_merge ~global_resolution left.possible_condition right.possible_condition;
      usedef_tables;
      cfg;
    }

  let pp format {arg_types; arg_annotation; return_condition; return_annotation; return_types; usage_attributes; possible_condition; _} =
    Format.fprintf format 
      "<Arg Annotation>\n%a\n\n<Arg Types>\n%a\n\n<Return Condition>\n%a\n\n<Return Annotation> %a \n\n<Return Type> %a\n\n<Usage Attributes>\n%a \n\n<Possible Condition>\n%a\n" 
      Refinement.Store.pp arg_annotation ArgTypes.pp arg_types Refinement.Store.pp return_condition Type.pp return_annotation Type.pp return_types AttributeStorage.pp usage_attributes Refinement.Store.pp possible_condition

  let add_arg_types ({arg_types; _} as t) arg_typ_list =
    { t with arg_types = List.fold arg_typ_list ~init:arg_types ~f:(fun arg_types (arg, typ) -> ArgTypes.add_arg_type arg_types arg typ) }

  let add_return_info ({return_types; _} as t) return_type =
    let modified_typ = weaken_typ return_type in
    let return_types = Type.union [return_types; modified_typ] in
    {t with return_types;}

  let add_usage_attributes ({usage_attributes; _ } as t) storage =
    let x = { t with usage_attributes=AttributeStorage.join usage_attributes storage} in
    x

  let set_arg_annotation t arg_annotation = 
    { t with arg_annotation }
  let set_possible_condition t possible_condition =
    { t with possible_condition=(extract_self possible_condition); }

  let set_return_condition t return_condition =
    { t with return_condition=(extract_parameter return_condition); }

  let set_return_annotation t return_annotation =
    { t with return_annotation }

  let set_usedef_tables t usedef_tables =
    { t with usedef_tables; }

  let set_cfg t cfg =
    { t with cfg; }

  let get_usedef_tables {usedef_tables; _} = usedef_tables

  let get_arg_annotation { arg_annotation; _ } = arg_annotation
  let get_return_condition { return_condition; _ } = return_condition

  let get_return_annotation { return_annotation; _ } = return_annotation

  let get_usage_attributes { usage_attributes; _ } = usage_attributes

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

  let find_class_of_attributes ~class_summary { usage_attributes; _ } parent_usage_attributes =
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
    (*
    Log.dump "TTT : %a" AttributeStorage.pp usage_attributes;
    *)
    AttributeStorage.map usage_attributes ~f:(fun attributes -> 
        attributes
        |> identifier_to_string
        |> ClassSummary.find_classes_from_attributes class_summary
        |> extract_class
    )
    |> LocInsensitiveExpMap.filter_map ~f:(fun v -> 
      match v with
      | Some v -> Some v
      | _ -> v  
    )

end

module FunctionSummaryMap = Map.Make (Reference)

module FunctionTable = struct
  type t = FunctionSummary.t FunctionSummaryMap.t [@@deriving sexp, equal]
  let create () = FunctionSummaryMap.empty

  (*let copy t = Hashtbl.copy t*)

  (*
  let equal t1 t2 =
    FunctionSummaryMap.equal (fun fs1 fs2 -> 
      FunctionSummary.equal fs1 fs2
    ) t1 t2
  *)
  let update ~global_resolution left right = 
    FunctionSummaryMap.merge left right ~f:(fun ~key:_ data ->
      match data with
      | `Left v | `Right v -> Some v
      | `Both (v1, v2) -> Some (FunctionSummary.update ~global_resolution v1 v2)
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

  let add_usage_attributes t func storage =
    let func_summary = FunctionSummaryMap.find t func |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummaryMap.set ~key:func ~data:(FunctionSummary.add_usage_attributes func_summary storage) t

  let set_arg_annotation t func arg_annotation =
    let func_summary = FunctionSummaryMap.find t func |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummaryMap.set ~key:func ~data:(FunctionSummary.set_arg_annotation func_summary arg_annotation) t
  
  let set_return_condition t func return_condition =
    let func_summary = FunctionSummaryMap.find t func |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummaryMap.set ~key:func ~data:(FunctionSummary.set_return_condition func_summary return_condition) t

  let set_return_annotation t func return_annotation =
    let func_summary = FunctionSummaryMap.find t func |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummaryMap.set ~key:func ~data:(FunctionSummary.set_return_annotation func_summary return_annotation) t
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

  let get_arg_annotation t func_name =
    let func_summary = FunctionSummaryMap.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_arg_annotation func_summary

  let get_return_condition t func_name =
    let func_summary = FunctionSummaryMap.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_return_condition func_summary

  let get_return_annotation t func_name =
    let func_summary = FunctionSummaryMap.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_return_annotation func_summary
  let get_usage_attributes t func_name =
    let func_summary = FunctionSummaryMap.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.get_usage_attributes func_summary

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

  let find_class_of_attributes ~class_summary t func_name parent_usage_attributes =
    let func_summary = FunctionSummaryMap.find t func_name |> Option.value ~default:(FunctionSummary.create ()) in
    FunctionSummary.find_class_of_attributes ~class_summary func_summary parent_usage_attributes

end

module OurSummary = struct
  type t = {
    class_summary : ClassSummary.t;
    function_table : FunctionTable.t;
  }
  [@@deriving equal, sexp]

  let create () = {
    class_summary = ClassSummary.empty; 
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
      function_table = FunctionTable.update ~global_resolution left.function_table right.function_table;
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

  let add_usage_attributes ?class_name ?class_var {class_summary; function_table; } func_name storage =
    let class_summary, storage =
      match class_name, class_var with
      | Some class_name, Some class_var ->
        let filtered_storage = AttributeStorage.filter_by_prefix storage ~prefix:(Reference.create class_var) in
        let class_summary = ClassSummary.add_usage_attributes class_summary class_name filtered_storage in
        let filter_class_var_storage = AttributeStorage.filter_class_var storage ~prefix:(Reference.create class_var) in
        class_summary, filter_class_var_storage
      | _ -> class_summary, storage
    in

    (*let class_summary =  in*)
    let function_table = FunctionTable.add_usage_attributes function_table func_name storage in
    { class_summary; function_table; }

  let add_parent_attributes ({ class_summary; _ } as t) storage class_name class_var =
    (* 이거 짜야 댕 *)
    let filtered_storage = AttributeStorage.filter_by_prefix storage ~prefix:(Reference.create class_var) in
    let class_summary = ClassSummary.add_parent_attributes class_summary class_name filtered_storage in
    { t with class_summary; }

  let set_arg_annotation({function_table; _} as t) func_name arg_annotation =
    { t with function_table=FunctionTable.set_arg_annotation function_table func_name arg_annotation }
  let set_return_condition ({function_table; _} as t) func_name return_condition =
    { t with function_table=FunctionTable.set_return_condition function_table func_name return_condition }

  let set_return_annotation ({function_table; _} as t) func_name return_annotation =
    { t with function_table=FunctionTable.set_return_annotation function_table func_name return_annotation }
  let set_possible_condition ({function_table; _} as t) func_name possible_condition =
    { t with function_table=FunctionTable.set_possible_condition function_table func_name possible_condition }
  let set_usedef_tables ({function_table; _} as t) func_name usedef_tables =
    { t with function_table=FunctionTable.set_usedef_tables function_table func_name usedef_tables }

  let set_cfg ({function_table; _} as t) func_name cfg =
    { t with function_table=FunctionTable.set_cfg function_table func_name cfg }

  let get_usedef_tables {function_table; _} func_name = 
    FunctionTable.get_usedef_tables function_table func_name
  
  let get_arg_annotation {function_table; _} func_name =
    FunctionTable.get_arg_annotation function_table func_name
  let get_return_condition {function_table; _} func_name =
    FunctionTable.get_return_condition function_table func_name

  let get_return_annotation {function_table; _} func_name =
    FunctionTable.get_return_annotation function_table func_name
  let get_usage_attributes_from_func { function_table; _ } func_name =
    FunctionTable.get_usage_attributes function_table func_name

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

  let add_class_attribute ({class_summary; _} as t) parent attr =
    { t with class_summary = ClassSummary.add_attribute class_summary parent attr }

  let add_class_property ({class_summary; _} as t) parent property =
    { t with class_summary = ClassSummary.add_property class_summary parent property }

  let add_class_method ({class_summary; _} as t) parent meth =
    { t with class_summary = ClassSummary.add_method class_summary parent meth }

  let set_class_info ({ class_summary; _ } as t) class_name class_info =
    { t with class_summary = ClassSummary.set_class_info class_summary class_name class_info }

  let get_class_info { class_summary; _ } class_name =
    ClassSummary.get_class_info class_summary class_name

  let get_usage_attributes_from_class { class_summary; _ } class_name = 
    ClassSummary.get_usage_attributes class_summary class_name

  let get_self_attributes_tree ~global_resolution { class_summary; _ } class_name = 
    ClassSummary.get_self_attributes_tree ~global_resolution class_summary class_name

  let get_type_of_class_attribute { class_summary; _ } class_name attribute =
    ClassSummary.get_type_of_class_attribute class_summary class_name attribute
  
  let update_map_function_of_types ({ class_summary; _ } as t) class_name vartype_map =
    { t with class_summary = ClassSummary.update_map_function_of_types class_summary class_name vartype_map }

  let search_suspicious_variable t ~global_resolution parent =
    (*let usedef_table = get_usedef_table t func_name |> Option.value ~default:UsedefState.bottom in*)
    let possible_condition = ClassSummary.get_class_variable_type t.class_summary parent in
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

  let find_class_of_attributes { class_summary; function_table; } func_name parent_usage_attributes  =
    FunctionTable.find_class_of_attributes ~class_summary function_table func_name parent_usage_attributes
end

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



(*
let check_file : string -> bool
= fun path ->
match Sys.file_exists path with
| `Yes -> true
| _ -> false
  *)
  
let data_path = ref ""

let is_func_model_exist () = check_dir !data_path

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

let save_global_summary () = save_summary !our_model (Reference.create global_summary)

let load_summary func_name =
  let filename = !data_path ^ "/" ^ (Reference.show func_name) ^ ".marshalled" in
  let x =
  match Sys.file_exists filename with
  | `Yes ->
      let data_in = open_in filename in
      let our_summary = OurSummary.t_of_sexp (Marshal.from_channel data_in) in
      close_in data_in;
      our_summary
  | _ ->
    OurSummary.create ()
  in
  x

let load_global_summary () = load_summary (Reference.create global_summary)

let load_all_summary_test () =
  (
    cache := true;
    let list_files = Sys.readdir !data_path |> Array.to_list in 
    let _ = List.iter list_files ~f:(fun file -> 
      if String.equal file "mode.marshalled" then ()
      else
      (
        Log.dump "%s Load..." file;
        let data_in = open_in (!data_path ^ "/" ^ file) in
        let t = OurSummary.t_of_sexp (Marshal.from_channel data_in) in
        close_in data_in;
        Log.dump "%a" OurSummary.pp t;
        ()
      )
    )
    in
    ()
  )

let global_cache = ref false

let rec load_global_summary_cache () =
  Thread.delay((Random.float 1.0) /. 1000.0);
  if !global_cache then (
    ()
  )
  else
  (
    
    (*
    global_cache := true;
    
    let data_out = open_out filename in
    Marshal.to_channel data_out "" [];
    close_out data_out;
    
    our_model := load_global_summary ();
    *)
      
    let filename = !data_path ^ "/" ^ "lock" in
    match Sys.file_exists filename with
    | `Yes ->

      Thread.delay((Random.float 1.0) /. 1000.0);
      load_global_summary_cache ()
    | _ ->
      global_cache := true;
      let data_out = open_out filename in
      Marshal.to_channel data_out "" [];
      close_out data_out;
      our_model := load_global_summary ();
      Sys.remove filename;
      
  )

let load_all_summary ?(use_cache=true) global_resolution =
  if use_cache && !cache then
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
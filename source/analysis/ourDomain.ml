open Core
open Ast

open AttributeAnalysis
(*
open Usedef
exception NotEqualException;;
*)
module ReferenceSet = Reference.Set
module ReferenceMap = struct
  include Map.Make (Reference)

   
  let join ~data_join ~equal left right =
    merge left right ~f:(fun ~key:_ data ->
      match data with
      | `Both (left, right) -> if equal left right then Some left else Some (data_join left right)
      | `Left data | `Right data -> Some data
    )

  let pp ~data_pp format t =
    iteri ~f:(fun ~key ~data ->
      Format.fprintf format "%a => %a\n" Reference.pp key data_pp data
    ) t
end

module StringSet = Set.Make (String)


(*
module VarType = struct
  type t = Reference.t * Type.t [@@deriving sexp, compare]
end

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
*)

module FunctionSet = ReferenceSet
module CallerSet = ReferenceSet
module StoreMap = ReferenceMap
module ClassMap = ReferenceMap
module FunctionMap = ReferenceMap

module AttrsSet = StringSet

module IdentifierMap = Map.Make (Identifier)

let weaken_typ typ =
  let weaken_typ = Type.weaken_literals typ in
  let weaken_typ =
    match weaken_typ with
    | Type.IntExpression _ -> Type.Primitive "int"
    | _ -> weaken_typ
  in
  weaken_typ

module type ArgTypes = sig
  type t = Type.t IdentifierMap.t
end

module ArgTypes = struct
  type t = Type.t IdentifierMap.t [@@deriving sexp, equal]

  let empty = IdentifierMap.empty



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

  let pp format t =
    IdentifierMap.iteri ~f:(fun ~key ~data ->
      Format.fprintf format "%a -> %a \n" Identifier.pp key Type.pp data;
    ) t

  let get_type t ident =
    IdentifierMap.find t ident |> Option.value ~default:Type.Unknown
end


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

module ClassSummary = struct
  type t = {
    class_var_type: Type.t ReferenceMap.t;
    class_attributes: ClassAttributes.t;
    usage_attributes: AttributeStorage.t;
  } [@@deriving sexp, equal]

  let empty = {
    class_var_type=ReferenceMap.empty;
    class_attributes=ClassAttributes.empty;
    usage_attributes=AttributeStorage.empty;
  }
  let get_class_var_type { class_var_type; _ } = class_var_type

  let get_usage_attributes { usage_attributes; _ } = usage_attributes

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

  let join ~type_join left right =
    {
      class_var_type = ReferenceMap.join left.class_var_type right.class_var_type ~equal:Type.equal ~data_join:type_join;
      class_attributes = ClassAttributes.join left.class_attributes right.class_attributes;
      usage_attributes = AttributeStorage.join left.usage_attributes right.usage_attributes;
    }

  let pp_class_var_type format { class_var_type; _ } =
      Format.fprintf format "[[[ Class Variable Type ]]] \n%a\n" (ReferenceMap.pp ~data_pp:Type.pp) class_var_type 

  let pp_class_info format { class_attributes; _ } =
      Format.fprintf format "[[[ Class Info ]]] \n%a\n" ClassAttributes.pp class_attributes

  let pp_usage_attributes format { usage_attributes; _ } =
      Format.fprintf format "[[[ Class Usage Attrs ]]] \n%a\n" AttributeStorage.pp usage_attributes

  let pp format t =
    Format.fprintf format "%a\n%a\n%a" pp_class_var_type t pp_class_info t pp_usage_attributes t
end

module type ClassTable = sig
  type t = ClassSummary.t ClassMap.t 
end

module ClassTable = struct
  type t = ClassSummary.t ClassMap.t [@@deriving sexp, equal]

  let empty = ClassMap.empty

  let find_default t name = ClassMap.find t name |> Option.value ~default:ClassSummary.empty 


  let add ~class_name ~data ~f t =
    let class_info = find_default t class_name in
    ClassMap.set t ~key:class_name ~data:(f class_info data)

  let add_attribute t class_name attr = add t ~class_name ~data:attr ~f:ClassSummary.add_attribute

  let add_property t class_name property = add t ~class_name ~data:property ~f:ClassSummary.add_property

  let add_method t class_name meth = add t ~class_name ~data:meth ~f:ClassSummary.add_method

  let add_usage_attributes t class_name storage = add t ~class_name ~data:storage ~f:ClassSummary.add_usage_attributes

  let set_class_info t class_name class_info =
    ClassMap.set t ~key:class_name ~data:class_info

  let get ~class_name ~f t = 
    let class_info = find_default t class_name in
    f class_info

  let get_class_info t class_name = get t ~class_name ~f:(fun x -> x)

  let get_class_var_type t class_name = get t ~class_name ~f:ClassSummary.get_class_var_type

  let get_usage_attributes t class_name = get t ~class_name ~f:ClassSummary.get_usage_attributes

  let join ~type_join left right =
    ClassMap.join left right ~equal:ClassSummary.equal ~data_join:(ClassSummary.join ~type_join)

  let pp format t =
    ClassMap.iteri ~f:(fun ~key ~data ->
      Format.fprintf format "[[[ Class : %a ]]] \n%a\n" Reference.pp key ClassSummary.pp data
    ) t

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

module type FunctionSummary = sig
  type t = {
    arg_types: ArgTypes.t; (* Argumets의 Input Type *)
    arg_annotation: ArgTypes.t; (* Argument Annotation *)
    return_var_type: Type.t ReferenceMap.t; (* Return 했을 때의 parameter 정보 *)
    return_type: Type.t; (* Function의 Return Type *)
    callers: CallerSet.t;
    usage_attributes : AttributeStorage.t;
    (*usedef_tables: UsedefStruct.t option;*)
  }

  val add_return_var_type : t -> Reference.t -> Type.t -> t
end


module FunctionSummary = struct
  type t = {
    arg_types: ArgTypes.t; (* Argumets의 Type*)
    arg_annotation: ArgTypes.t; (* Argument Annotation *)
    return_var_type: Type.t ReferenceMap.t; (* Return 했을 때의 parameter 정보 *)
    return_type: Type.t; (* Function의 Return Type *)
    callers: CallerSet.t;
    usage_attributes : AttributeStorage.t;
    (*usedef_tables: UsedefStruct.t option;*)
  } [@@deriving sexp, equal]

  let empty = {
    arg_types=ArgTypes.empty;
    arg_annotation=ArgTypes.empty;
    return_var_type=ReferenceMap.empty;
    return_type=Type.Unknown;
    callers=CallerSet.empty;
    usage_attributes=AttributeStorage.empty;
    (*usedef_tables=None;*)
  }

  let add_arg_types ~join ({arg_types; _} as t) arg_typ_list =
    { t with arg_types = List.fold arg_typ_list ~init:arg_types ~f:(fun arg_types (arg, typ) -> ArgTypes.add_arg_type ~join arg_types arg typ) }

  let add_usage_attributes ({usage_attributes; _ } as t) storage =
    let x = { t with usage_attributes=AttributeStorage.join usage_attributes storage} in
    x

  let add_return_var_type ({ return_var_type; _ } as t) reference typ =
    { t with return_var_type=(ReferenceMap.set return_var_type ~key:reference ~data:typ); }

  let add_caller ({ callers; _ } as t) caller =
    { t with callers=(CallerSet.add callers caller); }
  

    let set_arg_types t arg_types = 
    { t with arg_types }

  let set_arg_annotation t arg_annotation = 
    { t with arg_annotation }

  let set_return_var_type t return_var_type =
    { t with return_var_type; }

  let set_return_type t return_type =
    { t with return_type }

    (*
  let set_usedef_tables t usedef_tables =
    { t with usedef_tables; }

  let get_usedef_tables {usedef_tables; _} = usedef_tables
    *)

  let get_arg_types { arg_types; _ } = arg_types

  let get_arg_annotation { arg_annotation; _ } = arg_annotation
  let get_return_var_type { return_var_type; _ } = return_var_type

  let get_callers { callers; _ } = callers

  let get_usage_attributes { usage_attributes; _ } = usage_attributes
  let get_return_type {return_type; _} = return_type

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
    arg_types=ArgTypes.join ~type_join left.arg_types right.arg_types;
    arg_annotation=ArgTypes.join ~type_join left.arg_annotation right.arg_annotation;
    return_var_type=ReferenceMap.join ~equal:Type.equal ~data_join:type_join left.return_var_type right.return_var_type;
    return_type=type_join left.return_type right.return_type;
    callers=CallerSet.union left.callers right.callers;
    usage_attributes=AttributeStorage.join left.usage_attributes right.usage_attributes;
    (*usedef_tables=usedef_tables;*)
  }

  let join_return_type ~type_join ({return_type=origin; _} as t) return_type =
    { t with return_type=type_join origin return_type; }

  let pp_reference_set ~data_pp format t =
      ReferenceSet.iter ~f:(fun data ->
        Format.fprintf format "%a, " data_pp data
      ) t

  let pp format {arg_types; return_var_type; return_type; usage_attributes; callers; _} =
    Format.fprintf format 
      "<Arg Types>\n%a\n\n<Return Var Type>\n%a\n\n<Return Type> %a\n\n<Usage Attributes>\n%a\n<Callers>\n%a\n" 
     ArgTypes.pp arg_types 
     (ReferenceMap.pp ~data_pp:Type.pp) return_var_type 
     Type.pp return_type 
     AttributeStorage.pp usage_attributes
     (pp_reference_set ~data_pp:Reference.pp) callers

  let get_analysis_set prev next =
    let should_analysis = 
      if not (ArgTypes.equal prev.arg_types next.arg_types)
      then true
      else false
    in

    (* TODO: skip return_var_type? *)
    let analysis_set = 
      if not ((Type.equal prev.return_type next.return_type) && (ReferenceMap.equal Type.equal prev.return_var_type next.return_var_type))
      then next.callers
      else ReferenceSet.empty
    in
    
    should_analysis, analysis_set
end

module type FunctionTable = sig
  type t = FunctionSummary.t FunctionMap.t
end

module FunctionTable = struct
  type t = FunctionSummary.t FunctionMap.t [@@deriving sexp, equal]

  let empty = FunctionMap.empty

  let add_arg_types ~join t reference arg_typ_list =
    let func_summary = FunctionMap.find t reference |> Option.value ~default:FunctionSummary.empty in
    let func_summary = FunctionSummary.add_arg_types ~join func_summary arg_typ_list in
    FunctionMap.set ~key:reference ~data:func_summary t



  let add_usage_attributes t func storage =
    let func_summary = FunctionMap.find t func |> Option.value ~default:FunctionSummary.empty in
    FunctionMap.set ~key:func ~data:(FunctionSummary.add_usage_attributes func_summary storage) t

  let add_caller t func caller =
    let func_summary = FunctionMap.find t func |> Option.value ~default:FunctionSummary.empty in
    FunctionMap.set ~key:func ~data:(FunctionSummary.add_caller func_summary caller) t

  let set_arg_types t func arg_types =
    let func_summary = FunctionMap.find t func |> Option.value ~default:FunctionSummary.empty in
    FunctionMap.set ~key:func ~data:(FunctionSummary.set_arg_types func_summary arg_types) t

  let set_arg_annotation t func arg_annotation =
    let func_summary = FunctionMap.find t func |> Option.value ~default:FunctionSummary.empty in
    FunctionMap.set ~key:func ~data:(FunctionSummary.set_arg_annotation func_summary arg_annotation) t
  
  let set_return_var_type t func return_var_type =
    let func_summary = FunctionMap.find t func |> Option.value ~default:FunctionSummary.empty in
    FunctionMap.set ~key:func ~data:(FunctionSummary.set_return_var_type func_summary return_var_type) t

  let set_return_type t func return_type =
    let func_summary = FunctionMap.find t func |> Option.value ~default:FunctionSummary.empty in
    FunctionMap.set ~key:func ~data:(FunctionSummary.set_return_type func_summary return_type) t

    (*
  let set_usedef_tables t func usedef_tables =
    let func_summary = FunctionMap.find t func |> Option.value ~default:FunctionSummary.empty in
    FunctionMap.set ~key:func ~data:(FunctionSummary.set_usedef_tables func_summary usedef_tables) t

  let get_usedef_tables t func_name =
    let func_summary = FunctionMap.find t func_name |> Option.value ~default:FunctionSummary.empty in
    FunctionSummary.get_usedef_tables func_summary
    *)

  let get_arg_types t func_name =
    let func_summary = FunctionMap.find t func_name |> Option.value ~default:FunctionSummary.empty in
    FunctionSummary.get_arg_types func_summary

  let get_arg_annotation t func_name =
    let func_summary = FunctionMap.find t func_name |> Option.value ~default:FunctionSummary.empty in
    FunctionSummary.get_arg_annotation func_summary

  let get_return_var_type t func_name =
    let func_summary = FunctionMap.find t func_name |> Option.value ~default:FunctionSummary.empty in
    FunctionSummary.get_return_var_type func_summary

  let get_return_type t func_name =
    let func_summary = FunctionMap.find t func_name |> Option.value ~default:FunctionSummary.empty in
    FunctionSummary.get_return_type func_summary

  let get_callers t func_name =
    let func_summary = FunctionMap.find t func_name |> Option.value ~default:FunctionSummary.empty in
    FunctionSummary.get_callers func_summary

  let get_usage_attributes t func_name =
    let func_summary = FunctionMap.find t func_name |> Option.value ~default:FunctionSummary.empty in
    FunctionSummary.get_usage_attributes func_summary

  let join ~type_join left right =
    ReferenceMap.join left right ~equal:FunctionSummary.equal ~data_join:(FunctionSummary.join ~type_join)

  let join_return_type ~type_join t func return_type =
    let func_summary = FunctionMap.find t func |> Option.value ~default:FunctionSummary.empty in
    FunctionMap.set ~key:func ~data:(FunctionSummary.join_return_type ~type_join func_summary return_type) t

  let pp format table =
    FunctionMap.iteri ~f:(fun ~key ~data ->
      Format.fprintf format "[[[ Function Info ]]] \n%a \n%a \n" Reference.pp key FunctionSummary.pp data
    ) table

  let get_functions t prefix =
    List.filter (FunctionMap.keys t) ~f:(fun key ->
      Reference.is_prefix key ~prefix  
    )
    |>
    List.fold ~init:ReferenceSet.empty ~f:(fun ref_set key ->
      ReferenceSet.add ref_set key
    )

  let get_all_functions t =
    List.fold (FunctionMap.keys t) ~init:ReferenceSet.empty ~f:(fun ref_set key ->
      ReferenceSet.add ref_set key
    )

  let get_analysis_set prev next =
    FunctionMap.fold2 prev next ~init:ReferenceSet.empty ~f:(fun ~key ~data ref_set ->
      match data with
      | `Right next -> 
        ReferenceSet.add ref_set key
        |> ReferenceSet.union (FunctionSummary.get_callers next)
      | `Both (prev, next) -> 
        let should_analysis, analysis_set = FunctionSummary.get_analysis_set prev next in
        (if should_analysis
        then ReferenceSet.add analysis_set key
        else analysis_set
        ) |> ReferenceSet.union ref_set 
      | `Left _ -> failwith "Why prev is bigger?"
    )

end

module type OurSummary = sig
  type t = {
    class_table : ClassTable.t;
    function_table : FunctionTable.t;
  }
end

module OurSummary = struct
  type t = {
    class_table : ClassTable.t;
    function_table : FunctionTable.t;
  }
  [@@deriving equal, sexp]

  let empty = {
    class_table=ClassTable.empty;
    function_table=FunctionTable.empty;
  }

  let join ~type_join left right = 
    let timer = Timer.start () in
    let class_table = ClassTable.join ~type_join left.class_table right.class_table in
    let class_time = Timer.stop_in_sec timer in
    let function_table = FunctionTable.join ~type_join left.function_table right.function_table in
    let function_time = Timer.stop_in_sec timer -. class_time in
    (*Log.dump "Class %f \nFunction %f" class_time function_time;*)
    let _ = function_time in
    {
      class_table;
      function_table;
    }

  let join_return_type ~type_join ({function_table; _} as t) func_name return_type =
    { t with function_table = FunctionTable.join_return_type ~type_join function_table func_name return_type }

  let add_arg_types ~join ({ function_table; _} as t) reference arg_typ_list =
    { t with function_table = FunctionTable.add_arg_types ~join function_table reference arg_typ_list }



  let add_usage_attributes ?class_name ?class_var {class_table; function_table; } func_name storage =
    let class_table, storage =
      match class_name, class_var with
      | Some class_name, Some class_var ->
        let filtered_storage = AttributeStorage.filter_by_prefix storage ~prefix:(Reference.create class_var) in
        let class_table = ClassTable.add_usage_attributes class_table class_name filtered_storage in
              
        let filter_class_var_storage = AttributeStorage.filter_class_var storage ~prefix:(Reference.create class_var) in
        class_table, filter_class_var_storage
      | _ -> class_table, storage
    in

    (*let class_summary =  in*)
    let function_table = FunctionTable.add_usage_attributes function_table func_name storage in
    { class_table; function_table; }

  let add_caller ({ function_table; _} as t) ~caller callee =
    { t with function_table = FunctionTable.add_caller function_table callee caller }

  let set_arg_types ({function_table; _} as t) func_name arg_types =
    { t with function_table=FunctionTable.set_arg_types function_table func_name arg_types }

  let set_arg_annotation ({function_table; _} as t) func_name arg_annotation =
    { t with function_table=FunctionTable.set_arg_annotation function_table func_name arg_annotation }
  let set_return_var_type ({function_table; _} as t) func_name return_var_type =
    { t with function_table=FunctionTable.set_return_var_type function_table func_name return_var_type }

  let set_return_type ({function_table; _} as t) func_name return_type =
    { t with function_table=FunctionTable.set_return_type function_table func_name return_type }
    (*
  let set_usedef_tables ({function_table; _} as t) func_name usedef_tables =
    { t with function_table=FunctionTable.set_usedef_tables function_table func_name usedef_tables }

*)
  let get_class_table { class_table; _ } = class_table
(*
  let get_usedef_tables {function_table; _} func_name = 
    FunctionTable.get_usedef_tables function_table func_name
    *)
  
  let get_arg_types {function_table; _} func_name =
    FunctionTable.get_arg_types function_table func_name

  let get_arg_annotation {function_table; _} func_name =
    FunctionTable.get_arg_annotation function_table func_name
  let get_return_var_type {function_table; _} func_name =
    FunctionTable.get_return_var_type function_table func_name

  let get_return_type {function_table; _} func_name =
    FunctionTable.get_return_type function_table func_name

  let get_callers {function_table; _} func_name =
    FunctionTable.get_callers function_table func_name

  let get_usage_attributes_from_func { function_table; _ } func_name =
    FunctionTable.get_usage_attributes function_table func_name

  let add_class_attribute ({class_table; _} as t) parent attr =
    { t with class_table = ClassTable.add_attribute class_table parent attr }

  let add_class_property ({class_table; _} as t) parent property =
    { t with class_table = ClassTable.add_property class_table parent property }

  let add_class_method ({class_table; _} as t) parent meth =
    { t with class_table = ClassTable.add_method class_table parent meth }

  let set_class_summary ({ class_table; _ } as t) class_name class_info =
    { t with class_table = ClassTable.set_class_info class_table class_name class_info }

  let set_class_table t class_table =
    { t with class_table; }

  let get_class_summary { class_table; _ } class_name =
    ClassTable.get_class_info class_table class_name

  let get_usage_attributes_from_class { class_table; _ } class_name = 
    ClassTable.get_usage_attributes class_table class_name

  let pp_class format {class_table; _} =
    Format.fprintf format "%a" ClassTable.pp class_table
  let pp_func format {function_table; _} = 
    Format.fprintf format "%a" FunctionTable.pp function_table

  let pp formatter t =
    Format.fprintf formatter "%a\n\n%a" pp_class t pp_func t

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



let our_model = ref (OurSummary.empty);;

let our_summary = ref (OurSummary.empty);;

let cache = ref false;;

let is_search_mode = String.equal "search"

let is_inference_mode = String.equal "inference"

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
    OurSummary.empty
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

let load_specific_file () =
  let list_files = [
    "airflow.dag.serialization.serialization.Serialization._serialize.marshalled";
  ]
  in
  List.iter list_files ~f:(fun file ->
    let data_in = open_in (!data_path ^ "/" ^ file) in
    let other_summary = OurSummary.t_of_sexp (Marshal.from_channel data_in) in
    Log.dump "START %a" OurSummary.pp other_summary;
    close_in data_in;
  )

let select_our_model func_name =
  if is_inference_mode (load_mode ()) then
    load_summary func_name
  else 
    !our_model
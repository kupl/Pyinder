(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

 open Ast
(*open Usedef*)
open Core
open AttributeAnalysis

module ReferenceMap : sig
  include Map.S with type Key.t = Reference.t

  val join : data_join:('a -> 'a -> 'a) -> equal:('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
end

module ReferenceSet = Reference.Set
module IdentifierMap : Map.S with type Key.t = Identifier.t

module CallerSet = ReferenceSet
module ClassMap = ReferenceMap
module FunctionMap = ReferenceMap

module AttrsSet : Set.S with type Elt.t = String.t

module ClassAttributes: sig
  type t = {
    attributes: AttrsSet.t;
    properties: AttrsSet.t;
    methods: AttributeAnalysis.CallSet.t Identifier.Map.t;
  }

  
  val is_used_attr : t -> string -> bool
  (*
  val is_subset_with_total_attributes : t -> AttrsSet.t -> bool
  *)
end

module ClassSummary: sig
  type t = {
    class_var_type: Type.t ReferenceMap.t;
    class_attributes: ClassAttributes.t;
    usage_attributes: AttributeStorage.t;
  }

  val join : type_join:(Type.t -> Type.t -> Type.t) -> t -> t -> t

  val get_class_var_type : t -> Type.t FunctionMap.t

  val pp_class_var_type : Format.formatter -> t -> unit

  val pp : Format.formatter -> t -> unit
end

module type ClassTable = sig
  type t = ClassSummary.t ClassMap.t 
end

module ClassTable: sig
  type t = ClassSummary.t ClassMap.t 

  val find_default : t -> Reference.t -> ClassSummary.t

  val add : class_name:Reference.t -> data:'a -> f:(ClassSummary.t -> 'a -> ClassSummary.t) -> t -> t

  val get : class_name:Reference.t -> f:(ClassSummary.t -> 'a) -> t -> 'a

  val get_class_var_type : t -> Reference.t -> Type.t FunctionMap.t

  val pp : Format.formatter -> t -> unit
end

module type ArgTypes = sig
  type t = Type.t IdentifierMap.t
end


module ArgTypes : sig
  type t = Type.t IdentifierMap.t

  val empty : t

  val add_arg_type : join:(Type.t -> Type.t -> Type.t) -> t -> string -> Type.t -> t

  val pp : Format.formatter -> t -> unit

  val get_type : t -> Identifier.t -> Type.t
end

module type FunctionSummary = sig
  type t = {
    arg_types: ArgTypes.t; (* Argumets의 Type*)
    arg_annotation : ArgTypes.t;
    return_var_type: Type.t ReferenceMap.t; (* Return 했을 때의 parameter 정보 *)
    return_type: Type.t; (* Function의 Return Type *)
    callers: CallerSet.t;
    usage_attributes : AttributeStorage.t;
    (*usedef_tables: UsedefStruct.t option;*)
  }

  val add_return_var_type : t -> Reference.t -> Type.t -> t
end

module FunctionSummary : sig
  type t = {
    arg_types : ArgTypes.t;
    arg_annotation : ArgTypes.t;
    return_var_type : Type.t ReferenceMap.t;
    return_type : Type.t;
    callers: CallerSet.t;
    usage_attributes : AttributeStorage.t;
    (*usedef_tables : UsedefStruct.t option;*)
  }

  val empty : t

  val add_return_var_type : t -> Reference.t -> Type.t -> t
end

module type FunctionTable = sig
  type t = FunctionSummary.t FunctionMap.t
end

module FunctionTable : sig
  type t = FunctionSummary.t FunctionMap.t
end

module type OurSummary = sig
  type t = {
    class_table : ClassTable.t;
    function_table : FunctionTable.t;
  }
end

module OurSummary : sig
  type t = {
    class_table : ClassTable.t;
    function_table : FunctionTable.t;
  }
  [@@deriving equal, sexp]

  val empty : t

  val join : type_join:(Type.t -> Type.t -> Type.t) -> t -> t -> t

  val join_return_type : type_join:(Type.t -> Type.t -> Type.t) -> t -> Reference.t -> Type.t -> t

  val pp_class : Format.formatter -> t -> unit

  val pp_func : Format.formatter -> t -> unit

  val pp : Format.formatter -> t -> unit

  (*val copy : t -> t*)

  val add_arg_types : join:(Type.t -> Type.t -> Type.t) -> t -> Reference.t -> (Identifier.t * Type.t) list -> t

  val add_usage_attributes : ?class_name:Reference.t -> ?class_var:string -> t -> Reference.t -> AttributeStorage.t -> t

  val add_caller : t -> caller:Reference.t -> Reference.t -> t

  val set_arg_types : t -> Reference.t -> ArgTypes.t -> t

  val set_arg_annotation : t -> Reference.t -> ArgTypes.t -> t

  val set_return_var_type : t -> Reference.t -> Type.t FunctionMap.t -> t

  val set_return_type : t -> Reference.t -> Type.t -> t
(*
  val set_usedef_tables : t -> Reference.t -> UsedefStruct.t option -> t
*)
  val get_class_table : t -> ClassTable.t

  (*
  val get_usedef_tables : t -> Reference.t -> UsedefStruct.t option
*)
  val get_arg_types : t -> Reference.t -> ArgTypes.t

  val get_arg_annotation : t -> Reference.t -> ArgTypes.t

  val get_return_var_type : t -> Reference.t -> Type.t ReferenceMap.t

  val get_return_type : t -> Reference.t -> Type.t

  val get_callers : t -> Reference.t -> CallerSet.t

  val get_usage_attributes_from_func : t -> Reference.t -> AttributeStorage.t

  val add_class_attribute : t -> Reference.t -> string -> t

  val add_class_property : t -> Reference.t -> string -> t

  val add_class_method : t -> Reference.t -> AttributeAnalysis.CallInfo.t -> string -> t

  val set_class_summary : t -> Reference.t -> ClassSummary.t -> t

  val set_class_table : t -> ClassTable.t -> t

  val get_class_summary : t -> Reference.t -> ClassSummary.t

  val get_usage_attributes_from_class : t -> Reference.t -> AttributeStorage.t

  val get_skip_set : t -> t -> ReferenceSet.t
end

val global_summary : string

val is_func_model_exist : unit -> bool

val set_data_path : Configuration.Analysis.t -> unit

val our_model : OurSummary.t ref

val our_summary : OurSummary.t ref

val is_search_mode : string -> bool

val is_inference_mode : string -> bool

val save_mode : string -> unit

val load_mode : unit -> string

val save_summary : OurSummary.t -> Reference.t -> unit

val save_global_summary : unit -> unit

val load_summary : Reference.t -> OurSummary.t

val load_global_summary : unit -> OurSummary.t

val load_global_summary_cache : unit -> unit

val load_all_summary_test : unit -> unit

val load_all_summary : ?use_cache:bool -> type_join:(Type.t -> Type.t -> Type.t) -> skip_set:Reference.Set.t -> OurSummary.t -> unit

val load_specific_file : unit -> unit

val select_our_model : Reference.t -> OurSummary.t
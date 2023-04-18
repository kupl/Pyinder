(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


open Ast
open Ast.Util
open Usedef
open Core
open AttributeAnalysis

module Store = Refinement.Store
module Unit = Refinement.Unit

module VarType : sig
  type t = Reference.t * Type.t [@@deriving compare]
end

module VarTypeMap : Map.S with type Key.t = VarType.t
module ReferenceSet : Set.S with type Elt.t = Reference.t
module ReferenceMap : Map.S with type Key.t = Reference.t

module FunctionSet = ReferenceSet
module StoreMap = ReferenceMap
module ClassMap = ReferenceMap

module AttrsSet : Set.S with type Elt.t = string

module ClassAttributes : sig
  type t = {
    attributes: AttrsSet.t;
    properties: AttrsSet.t;
    methods: AttrsSet.t;
  }
end

module ClassInfo : sig
  type t = {
    class_variable_type : Store.t; (* 객체 변수들의 타입 정보 *)
    class_attributes : ClassAttributes.t; (* 클래스에서 사용 가능한 attribute의 Set *)
    class_vartype : FunctionSet.t VarTypeMap.t; (* ?? *)
    usage_attributes : AttributeStorage.t; (* 객체 변수가 사용한 attributes *)
  }
end

module ClassSummary : sig
  type t = ClassInfo.t ClassMap.t
  [@@deriving equal]

  val empty : t

  val outer_join_class_variable_type : global_resolution:GlobalResolution.t -> t -> Reference.t -> Refinement.Store.t -> t

  (*
  val outer_widen : global_resolution:GlobalResolution.t -> t -> Reference.t -> Refinement.Store.t -> t
  *)
  val join_with_merge_class_variable_type : global_resolution:GlobalResolution.t -> t -> Reference.t -> Refinement.Store.t -> t

  val pp : Format.formatter -> t -> unit

  (*val copy : t -> t*)

  val find_map_function_of_types : t -> Reference.t -> Reference.t -> Type.t -> FunctionSet.t
end

module ArgTypeMap : Map.S with type Key.t = Identifier.t

module ArgTypes : sig
  type t = Type.t ArgTypeMap.t

  val pp : Format.formatter -> t -> unit

  val get_type : t -> Identifier.t -> Type.t

  val export_to_resolution : t -> Resolution.t -> Resolution.t
end

module FunctionSummary : sig
  type t = {
    arg_annotation : Refinement.Store.t;
    arg_types : ArgTypes.t;
    return_condition : Refinement.Store.t;
    return_annotation : Type.t;
    return_types : Type.t;
    usage_attributes : AttributeStorage.t;
    possible_condition : Refinement.Store.t;
    usedef_tables : UsedefStruct.t option;
    cfg : Cfg.t option;
  }
end

module FunctionSummaryMap : Map.S with type Key.t = Reference.t

module FunctionTable : sig
  type t = FunctionSummary.t FunctionSummaryMap.t
end

module OurSummary : sig
  type t = {
    class_summary : ClassSummary.t;
    function_table : FunctionTable.t;
  }
  [@@deriving equal]

  val create : unit -> t

  val class_summary : t -> ClassSummary.t

  val set_class_summary : t -> ClassSummary.t -> t

  val join_with_merge_function_possiblecondition : global_resolution:GlobalResolution.t -> t -> Reference.t -> Refinement.Store.t -> t

  val pp_class : Format.formatter -> t -> unit

  val pp_func : Format.formatter -> t -> unit

  val pp : Format.formatter -> t -> unit

  (*val copy : t -> t*)

  val add_arg_types : t -> Reference.t -> (Identifier.t * Type.t) list -> t

  val add_return_info : t -> Reference.t -> Type.t -> t

  val add_usage_attributes : ?class_name:Reference.t -> ?class_var:string -> t -> Reference.t -> AttributeStorage.t -> t

  val set_arg_annotation : t -> Reference.t -> Refinement.Store.t -> t

  val set_return_condition : t -> Reference.t -> Refinement.Store.t -> t

  val set_return_annotation : t -> Reference.t -> Type.t -> t

  val set_possible_condition : t -> Reference.t -> Refinement.Store.t -> t

  val set_usedef_tables : t -> Reference.t -> UsedefStruct.t option -> t

  val set_cfg : t -> Reference.t -> Cfg.t option -> t

  val get_usedef_tables : t -> Reference.t -> UsedefStruct.t option

  val get_arg_annotation : t -> Reference.t -> Refinement.Store.t

  val get_return_condition : t -> Reference.t -> Refinement.Store.t

  val get_return_annotation : t -> Reference.t -> Type.t

  val get_usage_attributes_from_func : t -> Reference.t -> AttributeStorage.t

  val get_possible_condition : t -> Reference.t -> Refinement.Store.t

  val get_func_arg_types : t -> Reference.t -> ArgTypes.t

  val get_func_return_types : t -> Reference.t -> Type.t

  val get_cfg : t -> Reference.t -> Cfg.t option

  val make_map_function_of_types : t -> Reference.t -> FunctionSet.t VarTypeMap.t

  val add_class_attribute : t -> Reference.t -> string -> t

  val add_class_property : t -> Reference.t -> string -> t

  val add_class_method : t -> Reference.t -> string -> t

  val set_class_info : t -> Reference.t -> ClassInfo.t -> t

  val get_class_info : t -> Reference.t -> ClassInfo.t

  val get_usage_attributes_from_class : t -> Reference.t -> AttributeStorage.t

  val get_self_attributes_tree : t -> Reference.t -> Refinement.Unit.t Identifier.Map.Tree.t

  val update_map_function_of_types : t -> Reference.t -> FunctionSet.t VarTypeMap.t -> t

  val search_suspicious_variable : t -> global_resolution:GlobalResolution.t -> Reference.t -> Refinement.Unit.t Reference.Map.t list

  val find_class_of_attributes : ?class_name:Reference.t -> ?class_param:string -> t -> Reference.t -> Reference.t LocInsensitiveExpMap.t 
end

val final_summary : string

val set_data_path : Configuration.Analysis.t -> unit

val our_model : OurSummary.t ref

val is_search_mode : string -> bool

val is_inference_mode : string -> bool

val single_errors : AnalysisError.t list ref

val global_resolution : GlobalResolution.t option ref

val save_mode : string -> unit

val load_mode : unit -> string

val save_summary : OurSummary.t -> Reference.t -> unit

val load_summary : Reference.t -> OurSummary.t

val load_all_summary : ?use_cache:bool -> GlobalResolution.t -> unit

val select_our_model : Reference.t -> OurSummary.t
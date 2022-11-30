(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Usedef
open Core

module VarType : sig
  type t = Reference.t * Type.t [@@deriving compare, sexp, equal, hash, to_yojson, show]
end

module VarTypeMap : Map.S with type Key.t = VarType.t
module FunctionSet : Set.S with type Elt.t = Reference.t

module ClassSummary : sig
  type t = { 
    store_info : (Reference.t, Refinement.Store.t) Base.Hashtbl.t;
    class_vartype : (Reference.t, (FunctionSet.t VarTypeMap.t)) Base.Hashtbl.t;
  }
  [@@deriving equal]

  val create : unit -> t

  val get : t -> key:Reference.t -> Refinement.Store.t option

  val meet : global_resolution:GlobalResolution.t -> t -> Reference.t -> Refinement.Store.t -> unit

  val outer_join : global_resolution:GlobalResolution.t -> t -> Reference.t -> Refinement.Store.t -> unit

  val outer_widen : global_resolution:GlobalResolution.t -> t -> Reference.t -> Refinement.Store.t -> unit

  val join_with_merge : global_resolution:GlobalResolution.t -> t -> Reference.t -> Refinement.Store.t -> unit

  val pp_class_vartype : Format.formatter -> t -> unit

  val pp : Format.formatter -> t -> unit

  val pp_json : t -> string

  val copy : t -> t

  val find_map_function_of_types : t -> Reference.t -> Reference.t -> Type.t -> FunctionSet.t
end

module ArgTypes : sig
  type t = (Identifier.t, Type.t) Base.Hashtbl.t

  val pp : Format.formatter -> t -> unit

  val get_type : t -> Identifier.t -> Type.t

  val export_to_resolution : t -> Resolution.t -> Resolution.t
end

module FunctionSummary : sig
  type t = {
    arg_types : ArgTypes.t;
    return_types : Type.t;
    possible_condition : Refinement.Store.t;
    usedef_tables : UsedefStruct.t option;
    cfg : Cfg.t option;
  }
end

module FunctionTable : sig
  type t = (Reference.t, FunctionSummary.t) Base.Hashtbl.t
end

module OurSummary : sig
  type t = {
    class_summary : ClassSummary.t;
    function_table : FunctionTable.t;
    current_function : Ast.Reference.t option;
    current_possiblecondition : Refinement.Store.t option;
  }
  [@@deriving equal]

  val create : unit -> t

  val class_summary : t -> ClassSummary.t

  val set_current_function : t -> Reference.t -> t

  val set_current_possiblecondition : t -> Refinement.Store.t option -> t

  val join_with_merge_current_possiblecondition : t -> global_resolution:GlobalResolution.t -> Refinement.Store.t -> t

  val pp_class : Format.formatter -> t -> unit

  val pp_func : Format.formatter -> t -> unit

  val pp : Format.formatter -> t -> unit

  val copy : t -> t

  val add_arg_types : t -> Reference.t -> (Identifier.t * Type.t) list -> unit

  val add_return_info : t -> Type.t -> unit

  val set_possible_condition : t -> Reference.t -> Refinement.Store.t -> unit

  val set_usedef_tables : t -> Reference.t -> UsedefStruct.t option -> unit

  val set_cfg : t -> Reference.t -> Cfg.t option -> unit

  val get_usedef_tables : t -> Reference.t -> UsedefStruct.t option

  val get_possible_condition : t -> Reference.t -> Refinement.Store.t

  val get_current_usedef_tables : t -> UsedefStruct.t option

  val get_func_arg_types : t -> Reference.t -> ArgTypes.t

  val get_func_return_types : t -> Reference.t -> Type.t

  val get_cfg : t -> Reference.t -> Cfg.t option

  val get_current_possiblecondition : t -> Refinement.Store.t option

  val make_map_function_of_types : t -> Reference.t -> FunctionSet.t VarTypeMap.t

  val update_map_function_of_types : t -> Reference.t -> FunctionSet.t VarTypeMap.t -> unit

  val search_suspicious_variable : t -> global_resolution:GlobalResolution.t -> Reference.t -> Refinement.Unit.t Reference.Map.t list
end

 
val our_model : OurSummary.t ref

val our_models : OurSummary.t list ref

val is_search_mode : bool ref

val single_errors : AnalysisError.t list ref
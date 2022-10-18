(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module ClassSummary : sig
  type t = (Reference.t, Refinement.Store.t) Base.Hashtbl.t
  [@@deriving equal]

  val create : unit -> t

  val meet : global_resolution:GlobalResolution.t -> t -> Reference.t -> Refinement.Store.t -> unit

  val outer_join : global_resolution:GlobalResolution.t -> t -> Reference.t -> Refinement.Store.t -> unit

  val outer_widen : global_resolution:GlobalResolution.t -> t -> Reference.t -> Refinement.Store.t -> unit

  val pp : Format.formatter -> t -> unit

  val pp_json : t -> string

  val copy : t -> t
end

module ArgTypes : sig
  type t = (Identifier.t, Type.t) Base.Hashtbl.t

  val export_to_resolution : t -> Resolution.t -> Resolution.t
end

module FunctionSummary : sig
  type t = {
    arg_types : ArgTypes.t;
    return_types : Type.t;
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
  }
  [@@deriving equal]

  val create : unit -> t

  val class_summary : t -> ClassSummary.t

  val set_current_function : t -> Reference.t -> t

  val pp_class : Format.formatter -> t -> unit

  val pp_func : Format.formatter -> t -> unit

  val copy : t -> t

  val add_arg_types : t -> Reference.t -> (Identifier.t * Type.t) list -> unit

  val add_return_info : t -> Type.t -> unit

  val get_func_arg_types : t -> Reference.t -> ArgTypes.t

  val get_func_return_types : t -> Reference.t -> Type.t
end

 
val our_model : OurSummary.t ref
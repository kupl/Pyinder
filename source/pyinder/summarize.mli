(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Analysis
open Interprocedural

module TypeSummarize = OurTypeSet.OurSummary
module ReverseCallGraph = OurCallGraph.OurCallGraph

module Summarize : sig
  type t = {
    type_summary : OurTypeSet.OurSummary.t;
    call_graph : ReverseCallGraph.t;
    errors : AnalysisError.t list;
  }

  val create : unit -> t

  val analyze : t -> unit

  val pp : Format.formatter -> t -> unit
end

val errors : (AnalysisError.t list) ref
(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Interprocedural
open AnalysisError
open Ast 

module TypeSummarize = OurTypeSet.OurSummary
module ReverseCallGraph = OurCallGraph.OurCallGraph
module Signature = Ast.Statement.Define.Signature

let errors = ref []

module Summarize = struct
  type t = {
    type_summary : TypeSummarize.t;
    call_graph : ReverseCallGraph.t;
    errors : AnalysisError.t list
  }


  let make_target { Signature.name; _ } =
    Target.create_method name

  let analyze_error type_summary call_graph {AnalysisError.location; kind; signature; } =
    let _ = type_summary, call_graph, location, kind, signature in
    let source_define = make_target (Node.value signature) in
    let target_defines = ReverseCallGraph.find call_graph source_define in
    let _ = target_defines in
    match kind with
    | UndefinedAttribute { attribute; origin; } ->
      let _ = attribute, origin in

      ()
    | _ -> ()

  let analyze { type_summary; call_graph; errors;} =
    let _ = type_summary, call_graph, errors in
    List.iter errors ~f:(analyze_error type_summary call_graph)


  let create () = {
    type_summary = !OurTypeSet.our_model;
    call_graph = !OurCallGraph.our_callgraph;
    errors = !errors
  }

  let pp formatter { type_summary; call_graph; _ } =
    Format.fprintf formatter "%a\n\n%a" TypeSummarize.pp type_summary ReverseCallGraph.pp call_graph; 
end


(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pyre
open Ast
open Core
module PreviousEnvironment = AnnotatedGlobalEnvironment
module Error = AnalysisError

module CheckResultValue = struct
  type t = TypeCheck.CheckResult.t option [@@deriving equal]

  let prefix = Prefix.make ()

  let description = "CheckResult"
end





let produce_check_results global_environment define_name ~dependency =
  Log.dump "111";
  let type_check_controls, call_graph_builder, dependency =
    let controls = AnnotatedGlobalEnvironment.ReadOnly.controls global_environment in
    let configuration = EnvironmentControls.configuration controls in
    OurDomain.set_data_path configuration;
    OurDomain.our_model :=  EnvironmentControls.our_summary controls;
    let type_check_controls = EnvironmentControls.type_check_controls controls in
    let call_graph_builder =
      if EnvironmentControls.populate_call_graph controls then
        (module Callgraph.DefaultBuilder : Callgraph.Builder)
      else
        (module Callgraph.NullBuilder : Callgraph.Builder)
    in
    let dependency =
      if EnvironmentControls.track_dependencies controls then
        dependency
      else
        None
    in
    type_check_controls, call_graph_builder, dependency
  in
  Log.dump "222";
  let x = TypeCheck.check_define_by_name
    ~type_check_controls
    ~call_graph_builder
    ~global_environment
    ~dependency
    define_name
  in
  (*
  let mode = OurDomain.load_mode () in
  let x = 
  if OurDomain.is_search_mode mode then
    TypeCheck.search_define_by_name
      ~type_check_controls
      ~call_graph_builder
      ~global_environment
      ~dependency
      define_name
  else if OurDomain.is_inference_mode mode then (
    (if OurDomain.is_func_model_exist () then () else (
    OurDomain.save_summary OurDomain.OurSummary.empty define_name));
    (*OurTypeSet.load_global_summary_cache ();*)
    let x = TypeCheck.check_define_by_name
      ~type_check_controls
      ~call_graph_builder
      ~global_environment
      ~dependency
      define_name
    in
    (*OurTypeSet.save_summary !OurTypeSet.our_model define_name;*)
    x
  )
  else
    (
    TypeCheck.check_define_by_name_origin
          ~type_check_controls
          ~call_graph_builder
          ~global_environment
          ~dependency
          define_name
    )
  in
  *)
  Log.dump "444";
  x


module CheckResultsTable = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = AnnotatedGlobalEnvironment
  module Key = SharedMemoryKeys.ReferenceKey
  module Value = CheckResultValue

  type trigger = Reference.t [@@deriving sexp, compare]

  module TriggerSet = Reference.Set

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  let show_key = Reference.show

  let overlay_owns_key module_tracker_overlay =
    ModuleTracker.Overlay.owns_reference module_tracker_overlay

  let lazy_incremental = false

  let produce_value = produce_check_results

  let filter_upstream_dependency = function
    | SharedMemoryKeys.TypeCheckDefine name -> Some name
    | _ -> None


  let trigger_to_dependency name = SharedMemoryKeys.TypeCheckDefine name

  let equal_value = CheckResultValue.equal
end)

include CheckResultsTable

let global_environment = CheckResultsTable.Unsafe.upstream

let module_tracker type_environment =
  ast_environment type_environment |> AstEnvironment.module_tracker


let populate_for_definitions ~scheduler environment defines =
  let timer = Timer.start () in
  let read_only = read_only environment in
  let number_of_defines = List.length defines in
  Log.info "Checking %d functions..." number_of_defines;
  let map _ names =
    let analyze_define number_defines name =
      (*
      let t = ReadOnly.get read_only name in
      (match t with
      | Some t ->
        let errors = TypeCheck.CheckResult.errors t in
        List.iter (Option.value errors ~default:[]) ~f:(fun e -> Log.dump "In HERE! : %a" Error.pp e)
      | _ -> ());
      *)
      let () = ReadOnly.add read_only name |> ignore in
      number_defines + 1
    in
    let x = List.fold names ~init:0 ~f:analyze_define in
    x
  in

  let reduce left right =
    let number_defines = left + right in
    Log.log ~section:`Progress "Processed %d of %d functions" number_defines number_of_defines;
    number_defines
  in

  let _ =
    SharedMemoryKeys.DependencyKey.Registry.collected_map_reduce
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_size
           ~minimum_chunk_size:10
           ~minimum_chunks_per_worker:2
           ~preferred_chunk_size:100
           ())
      ~initial:0
      ~map
      ~reduce
      ~inputs:defines
      ()
  in

  Log.dump "666";

  Statistics.performance ~name:"check_TypeCheck" ~phase_name:"Type check" ~timer ()


let populate_for_modules ~scheduler ?type_join ?(skip_set=Reference.Set.empty) environment qualifiers =
  Profiling.track_shared_memory_usage ~name:"Before legacy type check" ();
  let all_defines =
    let unannotated_global_environment =
      global_environment environment
      |> AnnotatedGlobalEnvironment.read_only
      |> AnnotatedGlobalEnvironment.ReadOnly.unannotated_global_environment
    in

    let map _ qualifiers =
      List.concat_map qualifiers ~f:(fun qualifier ->
          UnannotatedGlobalEnvironment.ReadOnly.get_define_names
            unannotated_global_environment
            qualifier)
    in
    Scheduler.map_reduce
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_count
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:100
           ~preferred_chunks_per_worker:5
           ())
      ~initial:[]
      ~map
      ~reduce:List.append
      ~inputs:qualifiers
      ()
  in
  let filtered_defines =
    List.filter all_defines ~f:(fun name ->
      not (Reference.Set.exists skip_set ~f:(Reference.equal name))
    )
  in

  Log.dump "???";


  if List.length filtered_defines < 20 then
    List.iter filtered_defines ~f:(fun r -> Log.dump "Analysis: %a" Reference.pp r);
  populate_for_definitions ~scheduler environment filtered_defines;

  
  let read_only = read_only environment in
  (
  match type_join with
  | Some type_join ->
    let our_summary =
      List.fold filtered_defines ~init:(!OurDomain.our_model) ~f:(fun our_model define ->
        let result = ReadOnly.get read_only define in
        (match result with
        | Some t -> 
          let cur_summary =TypeCheck.CheckResult.our_summary t in
          OurDomain.OurSummary.join ~type_join our_model cur_summary
        | _ -> our_model
        )
      )
    in
    OurDomain.our_model := our_summary
  | _ -> Log.dump "No Join"
  )
  ;


  Statistics.event
    ~section:`Memory
    ~name:"shared memory size post-typecheck"
    ~integers:["size", Memory.heap_size ()]
    ();
  Profiling.track_shared_memory_usage ~name:"After legacy type check" ()




let populate_for_project_modules ~scheduler ?type_join ?(skip_set=Reference.Set.empty) environment =
  let project_qualifiers =
    module_tracker environment
    |> ModuleTracker.read_only
    |> ModuleTracker.ReadOnly.project_qualifiers
  in
  populate_for_modules ~scheduler ?type_join ~skip_set environment project_qualifiers;
  



module ReadOnly = struct
  include CheckResultsTable.ReadOnly

  let global_environment = CheckResultsTable.Testing.ReadOnly.upstream

  let global_resolution environment = global_environment environment |> GlobalResolution.create

  let ast_environment environment =
    unannotated_global_environment environment
    |> UnannotatedGlobalEnvironment.ReadOnly.ast_environment


  let module_tracker environment =
    ast_environment environment |> AstEnvironment.ReadOnly.module_tracker

    (*
  let get_our_summary environment ?dependency reference =
    let x = get ?dependency environment reference in
    match x with
    | Some x ->
      TypeCheck.CheckResult.our_summary x
    | _ -> OurDomain.OurSummary.empty
    *)

  let get_errors environment ?dependency reference =
    let x = get ?dependency environment reference in
    x >>= TypeCheck.CheckResult.errors
    |> Option.value ~default:[]


  let get_local_annotations environment ?dependency reference =
    let x = get ?dependency environment reference in 
    x >>= TypeCheck.CheckResult.local_annotations


  let get_or_recompute_local_annotations environment name =
    let x= get_local_annotations environment name in
    match x with
    | Some _ as local_annotations -> local_annotations
    | None ->
        (* Local annotations not preserved in shared memory in a standard pyre server (they can be,
           via TypeEnvironment.LocalAnnotations, but to save memory we only populate this for pysa
           runs, not the normal server used by LSP). This behavior is controlled by the
           `store_type_check_resolution` flag. *)
        let global_environment = global_environment environment in
        TypeCheck.compute_local_annotations ~global_environment name
end

(* All SharedMemory tables are populated and stored in separate, imperative steps that must be run
   before loading / after storing. These functions only handle serializing and deserializing the
   non-SharedMemory data *)

let store environment =
  CheckResultsTable.store environment;
  SharedMemoryKeys.DependencyKey.Registry.store ()


let load configuration =
  (* Loading the dependency keys needs to happen exactly once in the environment stack; we do it
     here, at the very top. *)
  SharedMemoryKeys.DependencyKey.Registry.load ();
  CheckResultsTable.load configuration


module TypeEnvironmentReadOnly = ReadOnly

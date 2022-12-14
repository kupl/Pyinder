(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

 open Core

 exception NoAstEnvironment;;
 exception UnequalErrorScenario;;

 module ExitStatus = struct
   type t =
     | Ok
     | PyreError
     | BuckInternalError
     | BuckUserError
   [@@deriving sexp, compare, hash]
 
   let exit_code = function
     | Ok -> 0
     | PyreError -> 1
     | BuckInternalError -> 2
     | BuckUserError -> 3
 end
 
 module CheckConfiguration = struct
   type t = {
     base: CommandStartup.BaseConfiguration.t;
     strict: bool;
     show_error_traces: bool;
     additional_logging_sections: string list;
   }
   [@@deriving sexp, compare, hash]
 
   let of_yojson json =
     let open Yojson.Safe.Util in
     let open JsonParsing in
     (* Parsing logic *)
     try
       match CommandStartup.BaseConfiguration.of_yojson json with
       | Result.Error _ as error -> error
       | Result.Ok base ->
           let strict = json |> bool_member "strict" ~default:false in
           let show_error_traces = json |> bool_member "show_error_traces" ~default:false in
           let additional_logging_sections =
             json |> string_list_member "additional_logging_sections" ~default:[]
           in
           Result.Ok { base; strict; show_error_traces; additional_logging_sections }
     with
     | Type_error (message, _)
     | Undefined (message, _) ->
         Result.Error message
     | other_exception -> Result.Error (Exn.to_string other_exception)
 
 
   let analysis_configuration_of
       {
         base =
           {
             CommandStartup.BaseConfiguration.source_paths;
             search_paths;
             excludes;
             checked_directory_allowlist;
             checked_directory_blocklist;
             extensions;
             log_path;
             global_root;
             local_root;
             debug;
             enable_type_comments;
             python_version = { Configuration.PythonVersion.major; minor; micro };
             parallel;
             number_of_workers;
             shared_memory =
               { Configuration.SharedMemory.heap_size; dependency_table_power; hash_table_power };
             remote_logging = _;
             profiling_output = _;
             memory_profiling_output = _;
           };
         show_error_traces;
         strict;
         additional_logging_sections = _;
       }
     =
     Configuration.Analysis.create
       ~parallel
       ~analyze_external_sources:false
       ~filter_directories:checked_directory_allowlist
       ~ignore_all_errors:checked_directory_blocklist
       ~number_of_workers
       ~local_root:(Option.value local_root ~default:global_root)
       ~project_root:global_root
       ~search_paths:(List.map search_paths ~f:SearchPath.normalize)
       ~strict
       ~debug
       ~show_error_traces
       ~excludes
       ~extensions
       ~incremental_style:Configuration.Analysis.Shallow
       ~log_directory:(PyrePath.absolute log_path)
       ~python_major_version:major
       ~python_minor_version:minor
       ~python_micro_version:micro
       ~shared_memory_heap_size:heap_size
       ~shared_memory_dependency_table_power:dependency_table_power
       ~shared_memory_hash_table_power:hash_table_power
       ~enable_type_comments
       ~source_paths:(Configuration.SourcePaths.to_search_paths source_paths)
       ()
 end
 
 let with_performance_tracking ~debug f =
   let timer = Timer.start () in
   let result = f () in
   let { Caml.Gc.minor_collections; major_collections; compactions; _ } = Caml.Gc.stat () in
   Statistics.performance
     ~name:"check"
     ~timer
     ~integers:
       [
         "gc_minor_collections", minor_collections;
         "gc_major_collections", major_collections;
         "gc_compactions", compactions;
       ]
     ~normals:["request kind", "FullCheck"]
     ();
   if debug then
     Memory.report_statistics ();
   result
 
 
 let do_check configuration =
   Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
       with_performance_tracking ~debug:configuration.debug (fun () ->
           
           let environment =
             let read_write_environment =
               Analysis.EnvironmentControls.create ~populate_call_graph:false configuration
               |> Analysis.ErrorsEnvironment.create
             in
             let () =
               Analysis.ErrorsEnvironment.check_and_preprocess read_write_environment ~scheduler
             in
             Analysis.ErrorsEnvironment.read_only read_write_environment
           in
           (*Log.dump "%a" Analysis.OurTypeSet.OurSummary.pp !Analysis.OurTypeSet.our_model;*)
           ( Analysis.ErrorsEnvironment.ReadOnly.get_all_errors environment,
             Analysis.ErrorsEnvironment.ReadOnly.ast_environment environment )))
 
 
 let compute_errors ~configuration ~build_system () =
   let rec fixpoint configuration n =
    let prev_model = !Analysis.OurTypeSet.our_model in
    let errors, ast_environment = do_check configuration in
    let errors = Analysis.AnalysisError.filter_type_error errors in

    if Analysis.OurTypeSet.OurSummary.equal prev_model (!Analysis.OurTypeSet.our_model)
    then errors, ast_environment
    else fixpoint configuration (n+1)
   in
   Log.dump "%s" "Type Inferecne...";
   let single_errors, ast_environment = fixpoint configuration 1 in
   (*Log.dump "%a" Analysis.OurTypeSet.OurSummary.pp !Analysis.OurTypeSet.our_model;*)
   Unix.sleep(1);
   Analysis.OurTypeSet.single_errors := single_errors;

   Log.dump "%s" "Type Error Searching...";
   Analysis.OurTypeSet.is_search_mode := true;
   (*print_endline "[[[ Search Mode ]]]";*)
   let errors, _ = do_check configuration in
   Log.dump "END";
  (*
   Log.dump "%a" Analysis.OurTypeSet.ClassSummary.pp_class_vartype (Analysis.OurTypeSet.OurSummary.class_summary !Analysis.OurTypeSet.our_model);
  *)
   Unix.sleep(1);
   let errors = Analysis.AnalysisError.filter_type_error errors in
   Pyinder.Summarize.ast_environment := Some ast_environment;
   Pyinder.Summarize.errors := errors;
   
   List.map
     (List.sort ~compare:Analysis.AnalysisError.compare errors)
     ~f:(Server.RequestHandler.instantiate_error ~build_system ~configuration ~ast_environment)
 
 
 let print_errors errors =
   Yojson.Safe.to_string
     (`Assoc
       [
         ( "errors",
           `List
             (List.map ~f:(fun error -> Analysis.AnalysisError.Instantiated.to_yojson error) errors)
         );
       ])
   |> Log.print "%s"
 
 
 let print_error_and_scenario check_configuration error_and_scenario_list =
   let { CheckConfiguration.base = { CommandStartup.BaseConfiguration.source_paths; _ }; _ } =
     check_configuration
   in
   let ast_environment = !Pyinder.Summarize.ast_environment in
   match ast_environment with
   | Some ast_environment ->
      Server.BuildSystem.with_build_system source_paths ~f:(fun build_system ->
          let errors =
              List.map
              (List.sort ~compare:(fun (e1, _) (e2, _) -> Analysis.AnalysisError.compare e1 e2) error_and_scenario_list)
              ~f:(fun (error, scenarios) ->
                Server.RequestHandler.instantiate_error 
                ~build_system ~configuration:(CheckConfiguration.analysis_configuration_of check_configuration) 
                ~ast_environment
                ?scenarios:(Some scenarios)
                error
              )
          in

          (*
          let single_errors =
            List.map
            !Analysis.OurTypeSet.single_errors
            ~f:(fun error ->
              Server.RequestHandler.instantiate_error 
              ~build_system ~configuration:(CheckConfiguration.analysis_configuration_of check_configuration) 
              ~ast_environment
              error
            )
          in
          *)

          (* ADD ERRORS *)
          
          (*
          print_endline "";
          Log.print "%s\n" (Analysis.OurTypeSet.ClassSummary.pp_json !Analysis.OurTypeSet.our_model.class_summary);
          *)
          print_errors (errors);
          Lwt.return ExitStatus.Ok)
    | None -> raise NoAstEnvironment
 
  let run_check check_configuration =
    let { CheckConfiguration.base = { CommandStartup.BaseConfiguration.source_paths; _ }; _ } =
     check_configuration
    in
    Server.BuildSystem.with_build_system source_paths ~f:(fun build_system ->
    let errors =
      compute_errors
        ~configuration:(CheckConfiguration.analysis_configuration_of check_configuration)
        ~build_system
        ()
    in

    (* ADD ERRORS *)
    
    (*
    print_endline "";
    Log.print "%s\n" (Analysis.OurTypeSet.ClassSummary.pp_json !Analysis.OurTypeSet.our_model.class_summary);
    *)
    (*print_errors errors;*)
    let _ = errors in
    Lwt.return ExitStatus.Ok)
 
 let on_exception = function
   | Buck.Raw.BuckError { buck_command; arguments; description; exit_code; additional_logs } ->
       Log.error "Cannot build the project: %s. " description;
       (* Avoid spamming the user with repro command if the argument is really long. *)
       if Buck.Raw.ArgumentList.length arguments <= 20 then
         Log.error
           "To reproduce this error, run `%s`."
           (Buck.Raw.ArgumentList.to_buck_command ~buck_command arguments);
       if not (List.is_empty additional_logs) then (
         Log.error "Here are the last few lines of Buck log:";
         Log.error "  ...";
         List.iter additional_logs ~f:(Log.error "  %s"));
       let exit_status =
         match exit_code with
         | Some exit_code when exit_code < 10 -> ExitStatus.BuckUserError
         | _ -> ExitStatus.BuckInternalError
       in
       exit_status
   | Buck.Interface.JsonError message ->
       Log.error "Cannot build the project because Buck returns malformed JSON: %s" message;
       ExitStatus.BuckUserError
   | Buck.Builder.LinkTreeConstructionError message ->
       Log.error
         "Cannot build the project because Pyre encounters a fatal error while constructing a link \
          tree: %s"
         message;
       ExitStatus.BuckUserError
   | Server.ChecksumMap.LoadError message ->
       Log.error "Cannot load external wheel properly. %s" message;
       ExitStatus.PyreError
   | Worker.Worker_exited_abnormally (pid, status)
   | Base.Exn.Finally (Worker.Worker_exited_abnormally (pid, status), _) ->
       let message =
         match status with
         | Caml.Unix.WEXITED return_code -> Format.sprintf "exited with return code %d" return_code
         | Caml.Unix.WSIGNALED signal -> Format.sprintf "was killed with signal %d" signal
         | Caml.Unix.WSTOPPED signal -> Format.sprintf "was stopped with signal %d" signal
       in
       Log.error
         "Pyre encountered an internal exception: Worker_exited_abnormally: process %d %s"
         pid
         message;
       ExitStatus.PyreError
   | _ as exn ->
       print_endline "INTERNAL ERROR !!!";
       print_endline (Exn.to_string exn);
       Printexc.print_backtrace Out_channel.stdout ;
       Log.error "Pyre encountered an internal exception: %s" (Exn.to_string exn);
       ExitStatus.PyreError
 
 
 let our_analysis check_configuration analyze_json = 
    let x = run_check check_configuration in 
    (match Lwt.state x with
    | Return _ -> print_endline "?"
    | Fail exn -> let _ = on_exception exn in ()
    | _ -> ()
    );

    Log.dump "%s" "Analyze Call Graph...";
    Unix.sleep(1);
    let _ = AnalyzeCommand.run_analyze_mine analyze_json in

    let errors = !Pyinder.Summarize.errors in
    let pyinder_model = Pyinder.Summarize.Summarize.create () in
    let candidates_scenarios = Pyinder.Summarize.Summarize.analyze pyinder_model in

    let error_and_scenario_list =
      List.map2 errors candidates_scenarios ~f:(fun error candidate_scenarios -> 
        (error, Pyinder.Summarize.show_candidate_scenarios candidate_scenarios)
      )
    in

    (match error_and_scenario_list with
    | List.Or_unequal_lengths.Ok error_and_scenario_list -> 
      let _ = print_error_and_scenario check_configuration error_and_scenario_list in ()
    | List.Or_unequal_lengths.Unequal_lengths -> raise UnequalErrorScenario
    );
    
    Unix.sleep(1);
    x

    
 let run_mine configuration_file =
   let exit_status =
     match CommandStartup.read_and_parse_json configuration_file ~f:CheckConfiguration.of_yojson with
     | Result.Error message ->
         Log.error "%s" message;
         ExitStatus.PyreError
     | Result.Ok
         ({
            CheckConfiguration.base =
              {
                CommandStartup.BaseConfiguration.global_root;
                local_root;
                debug;
                remote_logging;
                profiling_output;
                memory_profiling_output;
                _;
              };
            additional_logging_sections;
            _;
          } as check_configuration) ->
         CommandStartup.setup_global_states
           ~global_root
           ~local_root
           ~debug
           ~additional_logging_sections
           ~remote_logging
           ~profiling_output
           ~memory_profiling_output
           ();

         let analyze_json = CommandStartup.read_and_parse_json configuration_file ~f:AnalyzeCommand.AnalyzeConfiguration.of_yojson in
 
         Lwt_main.run
           (Lwt.catch
              (fun () -> 
                our_analysis check_configuration analyze_json
              )
              (fun exn -> Lwt.return (on_exception exn)))
   in
   Unix.sleep(1);
   Statistics.flush ();
   exit (ExitStatus.exit_code exit_status)
 
 
 let command =
   Printexc.record_backtrace true;
   let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
   Command.basic
     ~summary:"Runs a full check without a server"
     (Command.Param.map filename_argument ~f:(fun filename () -> run_mine filename))
 
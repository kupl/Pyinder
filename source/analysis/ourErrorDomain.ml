open Ast
open Core
open Pyre
module Error = AnalysisError
module OurErrorListReadOnly = OurErrorDomainReadOnly.OurErrorListReadOnly
module LocationMap = Map.Make (Location.WithModule)


module RefTyp = struct
  type t = Reference.t * Type.t [@@deriving sexp, equal, compare]
end

module RefTypSet = Set.Make (RefTyp)



module Cause = struct
  type cause =
    | Binary of cause * cause
    | RefSet of (Reference.Set.t * Type.t)
    | Exp of (Expression.t * Type.t)
    [@@deriving sexp, compare]

  type t = {
    cause : cause;
    context : AstContext.t;
  } [@@deriving sexp, compare]

  let create ~cause ~context = { cause; context }

  let calc_metric left right =
    (* Reference.Set.iter left_reference_set ~f:(fun r -> Log.dump "%a" Reference.pp r);
    Log.dump "T : %a" Type.pp left_type;
    Reference.Set.iter right_reference_set ~f:(fun r -> Log.dump "%a" Reference.pp r);
    Log.dump "T : %a" Type.pp right_type; *)
    let rec calc_cause_score left right =
      match left, right with
      | Binary (Exp e, RefSet r), _ -> calc_cause_score (Binary (RefSet r, Exp e)) right
      | _, Binary (Exp e, RefSet r) -> calc_cause_score left (Binary (RefSet r, Exp e))
      | Binary (RefSet lr, Exp le), Binary (RefSet rr, Exp re) ->
        let refset = calc_cause_score (RefSet lr) (RefSet rr) in
        let exp = calc_cause_score (Exp le) (Exp re) in
        (1.0 +. exp) *. refset /. 2.0
      | Binary (RefSet r, Exp _), RefSet _ -> calc_cause_score (RefSet r) right
      | Binary (RefSet lr1, RefSet lr2), Binary (RefSet rr1, RefSet rr2) ->
        let s1 = calc_cause_score (RefSet lr1) (RefSet rr1) in
        let s2 = calc_cause_score (RefSet lr1) (RefSet rr2) in
        let s3 = calc_cause_score (RefSet lr2) (RefSet rr1) in
        let s4 = calc_cause_score (RefSet lr2) (RefSet rr2) in
        (* Log.dump "HMM : %f %f %f %f" s1 s2 s3 s4; *)
        Option.value_exn (List.max_elt ~compare:Float.compare [s1; s2; s3; s4])
      | Binary (RefSet only, Exp _), Binary (RefSet other1, RefSet other2)
      | Binary (RefSet other1, RefSet other2), Binary (RefSet only, Exp _) ->
        let s1 = calc_cause_score (RefSet only) (RefSet other1) in
        let s2 = calc_cause_score (RefSet only) (RefSet other2) in
        Option.value_exn (List.max_elt ~compare:Float.compare [s1; s2])
      | RefSet (left_reference_set, left_type), RefSet (right_reference_set, right_type) ->
        let ref_score =
          (Reference.Set.length (Reference.Set.inter left_reference_set right_reference_set)) //
          (Reference.Set.length (Reference.Set.union left_reference_set right_reference_set))
        in
        let type_score =
          Type.calc_type left_type right_type
        in
        (* Reference.Set.iter left_reference_set ~f:(fun t -> Log.dump ">> %a" Reference.pp t);
        Log.dump "VS";
        Reference.Set.iter right_reference_set ~f:(fun t -> Log.dump ">> %a" Reference.pp t);
        Log.dump "%f * %f" ref_score type_score; *)
        ref_score *. type_score
      | Exp ({ Node.value=left_expression; _ }, left_type), Exp ({ Node.value=right_expression; _ }, right_type) ->
        let exp_score = 
          Expression.calc_similarity left_expression right_expression
        in
        let type_score =
          Type.calc_type left_type right_type
        in
        
        exp_score *. type_score
      | _ -> 0.0
    in

    let cause_score = calc_cause_score left right in

    1.0 -. cause_score
end

module CauseMap = Map.Make (Cause)
module ErrorMap = Map.Make (Error)
module ErrorSet = Set.Make (Error)

module OurCauseMap = struct
  type t = (Cause.t) ErrorMap.t [@@deriving compare]

  let empty = ErrorMap.empty

  (* let length t = ReferenceSetMap.length t *)

  let set ~key ~data t = ErrorMap.set ~key ~data t

  (* let find ~key t = ErrorMap.find t key *)

  let fold = ErrorMap.fold

  

  (* let filter_singleton t = 
    (* let errors = ReferenceSetMap.data t in
    List.iter errors ~f:(fun errors -> 
      Log.dump "?? : %i" (List.length errors);
      List.iter errors ~f:(fun error -> Log.dump "E : %a" Error.pp error)
    ); *)
    
    ErrorMap.filter t ~f:(fun data -> List.length data <= 1) *)

  let dbscan ~epsilon ~min_pts t =
    let _ = epsilon in
    let rec find_cluster ~errors check_points cluster_points =
      if ErrorSet.is_empty check_points
      then cluster_points
      else
        let new_check_points = 
          ErrorSet.fold check_points ~init:ErrorSet.empty ~f:(fun new_check_points check_point ->
            let inner_points = 
              ErrorMap.fold errors ~init:[] ~f:(fun ~key ~data:{ Cause.cause; context; } acc -> 
                let { Cause.cause=check_cause; context=check_context; } = (ErrorMap.find_exn errors check_point) in

                if AstContext.compare context check_context = 0
                then (
                  Log.dump "Same Context";
                  acc
                )
                else (

                  let cause_distance = (Cause.calc_metric cause check_cause ) in
                  let context_distance = (AstContext.calc_metric context check_context) in
                  (* Log.dump "[FIRST]\n\n%a\n\n[SECOND]\n\n%a\n\n" Error.pp key Error.pp check_point;
                  Log.dump "Distance : %.3f\n\n" (cause_distance *. 0.6 +. context_distance *. 0.4); *)
                  if Float.(<=.) cause_distance 0.25 && Float.(<=.) context_distance 0.75
                  then key::acc
                  else acc
                )
              )
            in

            (* Log.dump "%i" (List.length inner_points); *)

            if List.length inner_points >= min_pts
            then (
              List.fold inner_points ~init:new_check_points ~f:(fun new_check_points point ->
                  if ErrorSet.mem cluster_points point 
                  then new_check_points
                  else ErrorSet.add new_check_points point
              )
            ) else 
              new_check_points
          )
        in

        find_cluster ~errors new_check_points (ErrorSet.union cluster_points new_check_points)
    in

    let rec get_noise_point t noise_map =
      match ErrorMap.nth t 0 with
      | None -> noise_map
      | Some (key, data) ->
        let new_cluster = find_cluster ~errors:t (ErrorSet.singleton key) (ErrorSet.singleton key) in

        (* if ErrorSet.length new_cluster > 1 then (
          Log.dump "\n[[ CLUSTER ]]";
          (* Log.dump ">> %a" Error.pp key; *)
          ErrorSet.iter new_cluster ~f:(fun error -> Log.dump ">> %a" Error.pp error);
        ); *)

        (* if ErrorSet.length new_cluster = 1 then (
          Log.dump "\n[[ SINGLE ]]";
          (* Log.dump ">> %a" Error.pp key; *)
          ErrorSet.iter new_cluster ~f:(fun error -> Log.dump ">> %a" Error.pp error);
        );
 *)

        let next_t = ErrorMap.filter_keys t ~f:(fun key -> not (ErrorSet.mem new_cluster key)) in

        (* Log.dump "LENGTH %i" (ErrorMap.length next_t); *)

        if ErrorSet.length new_cluster = 1 
        then get_noise_point next_t (ErrorMap.set ~key ~data noise_map)
        else get_noise_point next_t noise_map
    in

    get_noise_point t empty
end

module OurErrorList = struct
  type errors = Error.t list [@@deriving sexp]
  type t = Error.t LocationMap.t [@@deriving sexp]

  let empty = LocationMap.empty

  let set ~key ~data t = LocationMap.set ~key ~data t

  let get ~key t = LocationMap.find t key

  let add ~join ~(errors : Error.t list) t =
    List.fold errors ~init:t ~f:(fun t error -> 
      LocationMap.update t error.location ~f:(fun v ->
        match v with
        | Some origin_error -> 
          let error = Error.join_without_resolution ~type_join:join origin_error error in
          (* if String.equal (Reference.show origin_error.signature.value.name) "test.ParserBase._should_parse_dates"
          then 
          Log.dump "HMM : %a" Error.pp error; *)
          error
        | _ -> error   
      )
    )
  let num t =
    LocationMap.length t

  let cause_map_to_t cause_map =
    OurCauseMap.fold cause_map ~init:empty ~f:(fun ~key:({ Error.location; _ } as error) ~data:_ acc ->
        set ~key:location ~data:error acc  
    )

  let cause_analysis t our_model =
    let cause_map, loc_map =
      LocationMap.fold t ~init:(OurCauseMap.empty, LocationMap.empty) ~f:(
        fun ~key ~data:({ Error.signature={ Node.value={ name; _ }; _ }; context; _ } as error) (acc, locmap) ->
        
        

        let _ = error in
        let location = Location.strip_module key in
        let unique_analysis = OurDomain.OurSummary.get_unique_analysis our_model name in
        let error_expression_list = Error.get_expression_type [error] in

        match error_expression_list with
        | [] -> 
          acc, LocationMap.set ~key ~data:error locmap
        | _ -> 
          let new_acc = 
            List.map error_expression_list ~f:(fun (error_expression, typ) -> 
              match error_expression |> Expression.get_first_name >>= Expression.name_to_reference with
              | Some error_reference -> (
                match UniqueAnalysis.UniqueStruct.find_pre_statements_of_location unique_analysis location with
                  | Some state ->
                    let _ = state in
                    let var_set = UniqueAnalysis.UniqueState.get_all_relative_variables ~reference:error_reference state in
                    (* Log.dump "HMM? %a => %a" Error.pp error UniqueAnalysis.UniqueState.pp state; *)
                    (* Reference.Set.iter var_set ~f:(fun v -> Log.dump "%a" Reference.pp v); *)
                    let var_set =
                      if Reference.Set.is_empty var_set
                      then Reference.Set.singleton error_reference
                      else var_set
                    in

                    Cause.RefSet (var_set, typ)
                  | None -> Cause.Exp (error_expression, typ)
              )
              | _ -> (* Expression 비교 *) Cause.Exp (error_expression, typ)
            )
            |> (function 
              | left::[right] ->
                OurCauseMap.set acc ~key:error ~data:(Cause.create ~cause:(Cause.Binary (left, right)) ~context)
              | [cause] ->
                OurCauseMap.set acc ~key:error ~data:(Cause.create ~cause ~context)
              | _ -> acc
            )
              (* let data = OurCauseMap.find ~key:(error_reference_set, typ) acc |> Option.value ~default:[] in *)
              
            
          in
          new_acc, locmap
      )
    in
    let _ = OurCauseMap.dbscan in
    let x =
    cause_map
    (* |> OurCauseMap.dbscan ~epsilon:0.30 ~min_pts:2 *)
    |> cause_map_to_t
    |> LocationMap.merge loc_map ~f:(fun ~key:_ data ->
      match data with
      | `Left v | `Right v -> Some v
      | `Both (a, b) -> if Error.compare a b = 0 then Some a else None
    )
    in

    (* let other_x =
      cause_map
      |> cause_map_to_t
      |> LocationMap.merge loc_map ~f:(fun ~key:_ data ->
        match data with
        | `Left v | `Right v -> Some v
        | `Both (a, b) -> if Error.compare a b = 0 then Some a else None
      )
    in *)

    Log.dump "%i => %i" (LocationMap.length t) (LocationMap.length x);

    let _ = x in
    x 

    (* LocationMap.fold t ~init:0 ~f:(fun ~key:_ ~data acc ->
      List.length data + acc  
    ) *)

  (* let get_repeated_errors t key_list =
    let reference_type_map =
    ReferenceMap.filter_keys t ~f:(fun key -> List.exists key_list ~f:(Reference.equal key))
    |> ReferenceMap.map ~f:(fun errors ->
      let reference_type_list = 
        Error.get_reference_type errors 
      in

      let empty_set = RefTypSet.empty in
      List.fold reference_type_list ~init:empty_set ~f:(fun acc (r, t) -> RefTypSet.add acc (r, t))
    )
    in

    (* Log.dump "Map : %i" (ReferenceMap.length reference_type_map); *)

    let total_set = ReferenceMap.fold reference_type_map ~init:RefTypSet.empty ~f:(fun ~key:_ ~data acc ->
      RefTypSet.union acc data  
    )
    in

    (* Log.dump "Set : %i" (RefTypSet.length total_set); *)

    let remain_reftyp_set = 
      RefTypSet.filter total_set ~f:(fun (reference, typ) -> 
        let count = 
          ReferenceMap.fold reference_type_map ~init:0 ~f:(fun ~key:_ ~data:reference_type_set count ->
            if RefTypSet.mem reference_type_set (reference, typ) then count+1 else count
          )
        in

        let ref_count =
          List.fold key_list ~init:0 ~f:(fun count key ->
            let attribute_storage = OurDomain.OurSummary.get_usage_attributes_from_func !OurDomain.our_model key in
            let reference_list = AttributeAnalysis.AttributeStorage.get_reference_list attribute_storage in
            if List.exists reference_list ~f:(Reference.equal reference) then count+1 else count
          )
        in

        (* Log.dump "(%a, %a) => %i / %i" Reference.pp reference Type.pp typ count ref_count; *)
        
        (ref_count < 2 && not (Reference.is_parameter reference)) 
        || not (ref_count = 0 || Float.(>=) (Int.(//) count ref_count) 0.5)
      )
    in

    (* Log.dump "START";
    RefTypSet.iter remain_reftyp_set ~f:(fun (r, t) -> Log.dump "(%a, %a)" Reference.pp r Type.pp t);
 *)
    let x= 
    ReferenceMap.mapi t ~f:(fun ~key ~data:errors ->
      let flag = List.exists key_list ~f:(Reference.equal key) in 
      if flag then
        let exist = RefTypSet.mem remain_reftyp_set in
        let after = Error.filter_typical_errors ~exist errors in

        (* List.iter after ~f:(fun e -> Log.dump "ERROR: %a" Error.pp e); *)

        after
      else
        errors
    )
      in
    (* Log.dump "END"; *)
    x *)

  (*
  let equal left right =
    ReferenceMap.equal (fun l_value r_value -> [%compare.equal: Error.t list] l_value r_value) -> left right 
    *)
end

type errors = Error.t list [@@deriving sexp]

let read_only (our_error_list: OurErrorList.t) =
  let reference_map =
    LocationMap.fold our_error_list ~init:Reference.Map.empty ~f:(fun ~key:_ ~data ref_map -> 
      let signature = Node.value data.signature in
      let key = signature.name in
      let error_list = Reference.Map.find ref_map key |> Option.value ~default:[] in
      
      Reference.Map.set ref_map ~key ~data:(data::error_list)
    )
  in
  Reference.Map.fold reference_map ~init:OurErrorListReadOnly.empty ~f:(fun ~key ~data read_only -> 
    OurErrorListReadOnly.set ~key ~data:(sexp_of_errors data) read_only
  )

let get_errors ~key t = 
  OurErrorDomainReadOnly.ReferenceMap.find t key
  |> (function
  | Some errors -> errors_of_sexp errors
  | _ -> []
  )

let our_errors = ref OurErrorList.empty
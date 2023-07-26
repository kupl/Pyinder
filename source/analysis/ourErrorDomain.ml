open Ast
open Core
module Error = AnalysisError
module OurErrorListReadOnly = OurErrorDomainReadOnly.OurErrorListReadOnly
module ReferenceMap = Map.Make (Reference)

module RefTyp = struct
  type t = Reference.t * Type.t [@@deriving sexp, equal, compare]
end

module RefTypSet = Set.Make (RefTyp)

module OurErrorList = struct
  type errors = Error.t list [@@deriving sexp]
  type t = errors ReferenceMap.t [@@deriving sexp]

  let empty = ReferenceMap.empty

  let set ~key ~data t = ReferenceMap.set ~key ~data t

  let get ~key t = ReferenceMap.find t key

  let add ~key ~data t =
    let errors =
      get ~key t
      |> Option.value ~default:[]
    in
    set ~key ~data:((errors@data) |> Error.deduplicate) t

  let num t =
    ReferenceMap.fold t ~init:0 ~f:(fun ~key:_ ~data acc ->
      List.length data + acc  
    )

  let get_repeated_errors t key_list =
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
    x

  (*
  let equal left right =
    ReferenceMap.equal (fun l_value r_value -> [%compare.equal: Error.t list] l_value r_value) -> left right 
    *)
end

type errors = Error.t list [@@deriving sexp]

let read_only (our_error_list: OurErrorList.t) =
  ReferenceMap.fold our_error_list ~init:OurErrorListReadOnly.empty ~f:(fun ~key ~data read_only -> 
    OurErrorListReadOnly.set ~key ~data:(sexp_of_errors data) read_only
  )

let get_errors ~key t = 
  OurErrorDomainReadOnly.ReferenceMap.find t key
  |> (function
  | Some errors -> errors_of_sexp errors
  | _ -> []
  )

let our_errors = ref OurErrorList.empty
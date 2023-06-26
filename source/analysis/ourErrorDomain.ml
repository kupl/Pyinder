open Ast
open Core
module Error = AnalysisError
module OurErrorListReadOnly = OurErrorDomainReadOnly.OurErrorListReadOnly
module ReferenceMap = Map.Make (Reference)


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
open Ast
open Core
module Error = AnalysisError

module ReferenceMap = Map.Make (Reference)

module OurErrorList = struct
  type t = (Error.t list) ReferenceMap.t

  let empty = ReferenceMap.empty

  let set ~key ~data t = ReferenceMap.set ~key ~data t

  let get ~key t = ReferenceMap.find t key

  (*
  let equal left right =
    ReferenceMap.equal (fun l_value r_value -> [%compare.equal: Error.t list] l_value r_value) -> left right 
    *)
end

let our_errors = ref OurErrorList.empty
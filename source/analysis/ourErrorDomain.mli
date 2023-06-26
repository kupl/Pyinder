open Ast
open Core
module OurErrorListReadOnly = OurErrorDomainReadOnly.OurErrorListReadOnly
module Error = AnalysisError

module ReferenceMap : Map.S with type Key.t = Reference.t

module OurErrorList : sig
    type t = Error.t list ReferenceMap.t [@@deriving sexp]

    val empty : t

    val set : key:Reference.t -> data:Error.t list -> t -> t

    val get : key:Reference.t -> t -> Error.t list option

    val add : key:Reference.t -> data:Error.t list -> t -> t

    val num : t -> int
end

type errors = Error.t list [@@deriving sexp]

val read_only :  OurErrorList.t -> OurErrorListReadOnly.t
  
val get_errors : key:Reference.t -> OurErrorListReadOnly.t -> errors
  

val our_errors : OurErrorList.t ref
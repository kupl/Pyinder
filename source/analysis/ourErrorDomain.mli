open Ast
open Core
module OurErrorListReadOnly = OurErrorDomainReadOnly.OurErrorListReadOnly
module Error = AnalysisError

module LocationMap : Map.S with type Key.t = Location.WithModule.t

module OurErrorList : sig
    type t = Error.t LocationMap.t [@@deriving sexp]

    val empty : t

    val set : key:Location.WithModule.t -> data:Error.t -> t -> t

    val get : key:Location.WithModule.t -> t -> Error.t option

    val add : join:(Type.t -> Type.t -> Type.t) -> errors:Error.t list -> t -> t

    val num : t -> int

    (* val get_repeated_errors : t -> Reference.t list -> t *)
end

type errors = Error.t list [@@deriving sexp]

val read_only :  OurErrorList.t -> OurErrorListReadOnly.t
  
val get_errors : key:Reference.t -> OurErrorListReadOnly.t -> errors
  

val our_errors : OurErrorList.t ref
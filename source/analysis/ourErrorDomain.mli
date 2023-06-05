open Ast
open Core
module Error = AnalysisError

module ReferenceMap : Map.S with type Key.t = Reference.t

module OurErrorList : sig
    type t = Error.t list ReferenceMap.t

    val set : key:Reference.t -> data:Error.t list -> t -> t

    val get : key:Reference.t -> t -> Error.t list option
end

val our_errors : OurErrorList.t ref
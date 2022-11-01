open Core

module OurCallGraph : sig
  type t = {
    call_graph : CallGraph.WholeProgramCallGraph.t;
  }

  val create : unit -> t

  val find : t -> Target.t -> Target.t list option

  val set_callgraph : t -> CallGraph.WholeProgramCallGraph.t -> t

  val pp : Format.formatter -> t -> unit
end

val our_callgraph : OurCallGraph.t ref
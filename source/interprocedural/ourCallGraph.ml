open Core

module OurCallGraph = struct
  type t = {
    call_graph : CallGraph.WholeProgramCallGraph.t;
  }

  let create () = {
    call_graph = CallGraph.WholeProgramCallGraph.empty;
  }

  let find { call_graph; } source =
    Target.Map.find call_graph source

  let set_callgraph _ call_graph =
    { call_graph; }

  let pp formatter { call_graph; } =
    Format.fprintf formatter "%a" CallGraph.WholeProgramCallGraph.pp call_graph;
end

let our_callgraph = ref (OurCallGraph.create ());;
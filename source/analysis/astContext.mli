type t [@@deriving compare, sexp, show, hash]

val empty : t

val set_context : t -> Cfg.Node.t -> t

val calc_metric : t -> t -> float

val context : t ref
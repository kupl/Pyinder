open Core
open Ast
open MyUtil

module type UsedefState = sig
  type t [@@deriving show]

  val bottom : t

  val less_or_equal : left:t -> right:t -> bool

  val join : t -> t -> t

  val widen : previous:t -> next:t -> iteration:int -> t

  val is_defined : t -> Reference.t -> bool

  val is_undefined : t -> Reference.t -> bool

  val forward : statement_key:int -> t -> statement:Statement.t -> t

  val backward : statement_key:int -> t -> statement:Statement.t -> t
end


module UsedefState : sig
  module ReferenceSet : module type of SSet (Reference)
  module ReferenceMap : module type of SMap (Reference)
  
  module VarSet : sig
    type t = ReferenceSet.t
  end

  type usedef
  type t = {
    defined: ReferenceSet.t;
    undefined: ReferenceSet.t;
    usedef_table: usedef ReferenceMap.t;
  }
  include UsedefState with type t := t
end 

module type UsedefFixpoint = sig
  type state

  type t = {
    usedef_tables: state Int.Table.t
  }
  [@@deriving show]

  val entry : t -> state option

  val normal_exit : t -> state option

  val exit : t -> state option

  val empty : t

  val get_usedef_tables : t -> state Int.Table.t

  val find : t -> int -> state option

  val find_usedef_table_of_location : t -> Cfg.t -> Location.t -> state option

  val forward : cfg:Cfg.t -> initial:state -> t

  val backward : cfg:Cfg.t -> initial:state -> t

  val equal : f:(state -> state -> bool) -> t -> t -> bool
end

module Make (State : UsedefState) : UsedefFixpoint with type state = State.t

module UsedefStruct : UsedefFixpoint with type state = (UsedefState.t)
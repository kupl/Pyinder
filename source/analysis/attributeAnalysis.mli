open Core
open Ast
open Ast.Util
open Expression
open Statement

(*
module LocInsensitiveExpression : sig
  include module type of struct
    include Expression
  end
end
*)
module SkipMap = LocInsensitiveExpMap

module AttributeStorage :
  sig
    type t = Identifier.Set.t LocInsensitiveExpMap.t [@@deriving sexp, equal]
    val empty : t
    val map : t -> f:(Identifier.Set.t -> 'a) -> 'a LocInsensitiveExpMap.t
    val filter_keys : t -> f:(LocInsensitiveExp.t -> bool) -> t
    val pp_identifier_set : Format.formatter -> Identifier.Set.t -> unit
    val pp :
      Format.formatter -> t -> unit
    val add_attribute :
      Expression.t ->
      Identifier.Set.Elt.t ->
      t ->
      t
    val add_prefix : t -> prefix:Reference.t -> t
    val filter_by_prefix : t -> prefix:Reference.t -> t
    val filter_class_var : t -> prefix:Reference.t -> t
    val join : t -> t -> t
    val join_without_merge : origin:t -> t -> t
  end
val forward_expression_list :
  (AttributeStorage.t * String.Set.t SkipMap.t) ->
  exp_list:expression Node.t list ->
    (AttributeStorage.t * String.Set.t SkipMap.t)
val forward_expression :
  ?is_assign:bool ->
  expression:expression Node.t ->
  (AttributeStorage.t * String.Set.t SkipMap.t) ->
    (AttributeStorage.t * String.Set.t SkipMap.t)
val forward_statement :
(AttributeStorage.t * String.Set.t SkipMap.t) ->
    statement:statement Node.t ->
      (AttributeStorage.t * String.Set.t SkipMap.t)

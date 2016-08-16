type t =
  private
  | Statement of Formula.t list * Formula.t

exception Error

val hypothesis : int -> Formula.t list -> t
val true_intro : Formula.t list -> t
val and_intro : t -> t -> t
val and_elim1 : t -> t
val and_elim2 : t -> t
val imply_intro : t -> t
val imply_elim : t -> t -> t

val weaken : Formula.t -> t -> t
val contract : int -> int -> t -> t
val exchange : int -> int -> t -> t

val to_string : t -> string

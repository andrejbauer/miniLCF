type t =
  private
  | Statement of (string * Formula.t) list * Formula.t

exception Error

val hypothesis : string -> (string * Formula.t) list -> t
val true_intro : (string * Formula.t) list -> t

val cut : string -> t -> t -> t
val and_intro : t -> t -> t
val and_elim1 : t -> t
val and_elim2 : t -> t
val imply_intro : string -> t -> t
val imply_elim : t -> t -> t
val weaken : string -> Formula.t -> t -> t

val to_string : t -> string

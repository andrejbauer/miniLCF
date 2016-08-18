module type KERNEL =
  sig
    type t

    exception Error

    val consequent : t -> Formula.t
    val hypotheses : t -> (string * Formula.t) list

    val hypothesis : string -> (string * Formula.t) list -> t
    val cut : string -> t -> t -> t
    val true_intro : (string * Formula.t) list -> t
    val and_intro : t -> t -> t
    val and_elim1 : t -> t
    val and_elim2 : t -> t
    val imply_intro : string -> t -> t
    val imply_elim : t -> t -> t
    val weaken : string -> Formula.t -> t -> t

    val print : t -> Format.formatter -> unit
  end

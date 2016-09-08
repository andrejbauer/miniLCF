(** The abstract syntax of a fragment of the propositional calculus. *)
type t =
  | Atom of string    (** atomic proposition *)
  | True              (** truth *)
  | And of t * t      (** conjunction *)
  | Imply of t * t    (** implication *)

(** [print frml ppf] pretty-prints formula [frml] using the printing formatter [ppf]. *)
val print : t -> Format.formatter -> unit

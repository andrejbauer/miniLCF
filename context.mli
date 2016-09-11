(** A context is a finite map from strings to formulas, represented as a list
    The type [context] is private so that all contexts must be created with the
    function [context] below, which verifies that a list acutally represents a
    valid context. *)
type t

exception Invalid_context 

(** Fold over a context *)
val fold : ('a -> string * Formula.t -> 'a) -> 'a -> t -> 'a

(** The empty context *)
val empty : t

(** Is the given context empty? *)
val is_empty : t -> bool

(** Are the given contexts equal? *)
val equal : t -> t -> bool

(** [extend_ctx x a ctx] extends context [ctx] with [(x,a)]. It raises
    [Invalid_context] if [x] already appears in [ctx]. *)
val extend : string * Formula.t -> t -> t

(** [make lst] creates a context from the given list, or raises [Invalid_context]
    if the list contains several occurrences of the same variable. *)
val make : (string * Formula.t) list -> t

(** [extract x ctx] removes the hypotheses [x] from the context [ctx]. It returns [None]
    if [x] does not appear in [ctx], and [Some (a, ctx')] if [x] appears as [(x,a)], and
    [ctx'] is the context [ctx] without [(x,a)]. *)
val extract : string -> t -> (Formula.t * t) option

(** [find a ctx] finds the first context entry [(x,a)] and returns [Some x], or [None] if
    there is none. *)
val find : Formula.t -> t -> string option

(** [lookup x ctx] finds the first context etnry [(x,a)] and returns [Some a], or [None]
if there is none. *)
val lookup : string -> t -> Formula.t option

(** Pretty-print a context using the given formatter. *)
val print : ?vertical:bool -> t -> Format.formatter -> unit

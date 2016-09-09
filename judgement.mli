type t = Context.t * Formula.t

val print : ?vertical:bool -> t -> Format.formatter -> unit

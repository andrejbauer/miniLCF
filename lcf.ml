(** The signature of a pre-kernel. *)
module type PREKERNEL =
  sig
    (** The abstract type of judgments *)
    type t

    (** The exception [Error] signals an invalid use of a rule *)
    exception Error

    (** There are a number of constructors, each of which is corresponds to an inference
        rule, a derivable rule, or an admissible rule. In the comments below we indicate
        how constructors work by displaying the corresponding schematic rules. *)

    (** [hypo x1 [(x1,A1);...;(xn,An)]] is
        {v                
            ---------------------- (xi≠xj for i≠j)
             x1:A1,...,xn:An ⊢ Ai
        v}
    *)
    val hypo : string -> Context.t -> t

    (** [cut y (Γ ⊢ B) (Γ,y:B ⊢ C)] is
        {v
                Γ ⊢ B 
            Γ,y:B ⊢ C
           ------------
                Γ ⊢ C
        v}        
    *)
    val cut : string -> t -> t -> t

    (** [true_intro [(x1,A1);...;(xn,An)]] is
        {v                
            ------------------------ (xi≠xj for i≠j)
             x1:A1,...,xn:An ⊢ True
        v}
    *)
    val true_intro : Context.t -> t

    (** [and_intro (Γ ⊢ A) (Γ ⊢ B)] is 
       {v
          Γ ⊢ A
          Γ ⊢ B
         ------------
          Γ ⊢ A /\ B
     v} *)
    val and_intro : t -> t -> t

    (** [and_elim1 (Γ ⊢ A /\ B)] is
        {v
          Γ ⊢ A /\ B
         ------------
          Γ ⊢ A
        v}
    *)
    val and_elim1 : t -> t

    (** [and_elim2 (Γ ⊢ A /\ B)] is
        {v
          Γ ⊢ A /\ B
         ------------
          Γ ⊢ B
        v}
    *)
    val and_elim2 : t -> t

    (** [imply_intro y (Γ,y:A ⊢ B)] is
        {v
          Γ,y:A ⊢ B
         ----------------
              Γ ⊢ A -> B
        v}
    *)
    val imply_intro : string -> t -> t

    (** [imply_elim (Γ ⊢ A -> B) (Γ ⊢ A)] is
        {v
           Γ ⊢ A -> B
           Γ ⊢ A
          ------------
           Γ ⊢ B 
        v}
    *)
    val imply_elim : t -> t -> t

    (** [weaken x A (Γ ⊢ B)] is
        {v
              Γ ⊢ B 
         -----------
          Γ,x:A ⊢ B 

        v}
    *)
    val weaken : string -> Formula.t -> t -> t

    (** [print jdg ppf] pretty-prints judgement [jdg] using formatter [ppf] *)
    val print : t -> Format.formatter -> unit

  end

(** A kernel is a pre-kernel that operates with actual judgements. *)
module type KERNEL =
sig
  include PREKERNEL

  (** A judgment has the form 
        {v
           x1:A1, ..., xn:An ⊢ B
        v}

     where [x1:A1, ..., xn:An] is a finite map, called the {b hypotheses}, from distinct
     strings [x1,...,xn] to corresponding formulas [A1, ..., An], and [B] is a formula,
     called the {b consequent}.
      
     A judgement is derivable if it can be generated inductively using the inference
     rules, listed below.

     A kernel is an implementation of an abstract data type [t] which
     represents judgments, together with functions for constructing and
     destructing values of type [t]. Because [t] is abstract the only way
     to generate and inspect its values is by applying the provided functions.

     A kernel is {b correct} if every value of type [t] which can be
     computed represents a derivable judgement, and it is {b complete} if every
     derivable judgment can be represented as a value of type [t].
   *)

  (** [conclusion jdg] gives the conclusion of judgement [jdg] *)
  val conclusion : t -> Formula.t
                          
  (** [context jdg] gives the context of judgement [jdg] *)
  val context : t -> Context.t
end

module Entrust (Trusted : KERNEL) (K : PREKERNEL) : KERNEL with type t = Trusted.t * K.t =
struct
  type t = Trusted.t * K.t

  exception Error

  let conclusion (jdg, _) = Trusted.conclusion jdg

  let context (jdg, _) = Trusted.context jdg

  let hypo x ctx = 
    try
      (Trusted.hypo x ctx, K.hypo x ctx)
    with
    | Trusted.Error -> raise Error
    | K.Error -> raise Error

  let cut x (jdg1,d1) (jdg2,d2) =
    try
      (Trusted.cut x jdg1 jdg2, K.cut x d1 d2)
    with
    | Trusted.Error -> raise Error
    | K.Error -> raise Error

  let true_intro ctx =
    try
      (Trusted.true_intro ctx, K.true_intro ctx)
    with
    | Trusted.Error -> raise Error
    | K.Error -> raise Error
  
  let and_intro (jdg1,d1) (jdg2,d2) =
    try
      (Trusted.and_intro jdg1 jdg2, K.and_intro d1 d2)
    with
    | Trusted.Error -> raise Error
    | K.Error -> raise Error

  let and_elim1 (jdg, d) =
    try
      (Trusted.and_elim1 jdg, K.and_elim1 d)
    with
    | Trusted.Error -> raise Error
    | K.Error -> raise Error

  let and_elim2 (jdg, d) =
    try
      (Trusted.and_elim2 jdg, K.and_elim2 d)
    with
    | Trusted.Error -> raise Error
    | K.Error -> raise Error

  let imply_intro x (jdg,d) =
    try
      (Trusted.imply_intro x jdg, K.imply_intro x d)
    with
    | Trusted.Error -> raise Error
    | K.Error -> raise Error

  let imply_elim (jdg1,d1) (jdg2,d2) =
    try
      (Trusted.imply_elim jdg1 jdg2, K.imply_elim d1 d2)
    with
    | Trusted.Error -> raise Error
    | K.Error -> raise Error

  let weaken x a (jdg,d) =
    try
      (Trusted.weaken x a jdg, K.weaken x a d)
    with
    | Trusted.Error -> raise Error
    | K.Error -> raise Error

  let print (jdg,d) ppf = Format.fprintf ppf "%t@\n%t" (Trusted.print jdg) (K.print d)

end

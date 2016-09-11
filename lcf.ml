(** The signature of a pre-kernel. *)
module type TRANSFORMATION =
  sig
    type t
    exception Error

    val hypo : string -> Judgement.t -> t
    val cut : string -> (Judgement.t * t) -> (Judgement.t * t) -> Judgement.t -> t
    val true_intro : Judgement.t -> t
    val and_intro : (Judgement.t * t) -> (Judgement.t * t) -> Judgement.t -> t
    val and_elim1 : (Judgement.t * t) -> Judgement.t -> t
    val and_elim2 : (Judgement.t * t) -> Judgement.t -> t
    val imply_intro : string -> (Judgement.t * t) -> Judgement.t -> t
    val imply_elim : (Judgement.t * t) -> (Judgement.t * t) -> Judgement.t -> t
    val weaken : string -> Formula.t -> (Judgement.t * t) -> Judgement.t -> t
    val print : t -> Format.formatter -> unit
  end

(** A kernel is a transformation from which we can actually extract the judgment. *)
module type KERNEL =
sig
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


  (** The abstract type of judgments *)
  type t

  (** [conclusion x] gives the judgement encoded in [x]. *)
  val judgement : t -> Judgement.t

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

module Apply (F : TRANSFORMATION) (K : KERNEL) : KERNEL with type t = K.t * F.t =
struct
  type t = K.t * F.t

  exception Error

  let judgement (jdg, _) = K.judgement jdg

  let hypo x ctx = 
    try
      let u = K.hypo x ctx in
      let jdg = K.judgement u in
      (u, F.hypo x jdg)
    with
    | K.Error -> raise Error
    | F.Error -> raise Error

  let cut x (u1,v1) (u2,v2) =
    try
      let u = K.cut x u1 u2 in
      let jdg1 = K.judgement u1
      and jdg2 = K.judgement u2
      and jdg = K.judgement u in
      (u, F.cut x (jdg1,v1) (jdg2,v2) jdg)
    with
    | K.Error -> raise Error
    | F.Error -> raise Error

  let true_intro ctx =
    try
      let u = K.true_intro ctx in
      let jdg = K.judgement u in
      (u, F.true_intro jdg)
    with
    | K.Error -> raise Error
    | F.Error -> raise Error
  
  let and_intro (u1,v1) (u2,v2) =
    try
      let u = K.and_intro u1 u2 in
      let jdg1 = K.judgement u1
      and jdg2 = K.judgement u2
      and jdg = K.judgement u in
      (u, F.and_intro (jdg1,v1) (jdg2,v2) jdg)
    with
    | K.Error -> raise Error
    | F.Error -> raise Error

  let and_elim1 (u1,v1) =
    try
      let u = K.and_elim1 u1 in
      let jdg1 = K.judgement u1
      and jdg = K.judgement u in
      (u, F.and_elim1 (jdg1,v1) jdg)
    with
    | K.Error -> raise Error
    | F.Error -> raise Error

  let and_elim2 (u1,v1) =
    try
      let u = K.and_elim2 u1 in
      let jdg1 = K.judgement u1
      and jdg = K.judgement u in
      (u, F.and_elim2 (jdg1,v1) jdg)
    with
    | K.Error -> raise Error
    | F.Error -> raise Error

  let imply_intro x (u1,v1) =
    try
      let u = K.imply_intro x u1 in
      let jdg1 = K.judgement u1
      and jdg = K.judgement u in
      (u, F.imply_intro x (jdg1,v1) jdg)
    with
    | K.Error -> raise Error
    | F.Error -> raise Error

  let imply_elim (u1,v1) (u2,v2) =
    try
      let u = K.imply_elim u1 u2 in
      let jdg1 = K.judgement u1
      and jdg2 = K.judgement u2
      and jdg = K.judgement u in
      (u, F.imply_elim (jdg1,v1) (jdg2,v2) jdg)
    with
    | K.Error -> raise Error
    | F.Error -> raise Error

  let weaken x a (u1,v1) =
    try
      let u = K.weaken x a u1 in
      let jdg1 = K.judgement u1
      and jdg = K.judgement u in
      (u, F.weaken x a (jdg1,v1) jdg)
    with
    | K.Error -> raise Error
    | F.Error -> raise Error

  let print jdg ppf = Format.fprintf ppf "%t@\n%t" (K.print (fst jdg)) (F.print (snd jdg))

end

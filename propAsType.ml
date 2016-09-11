type t = Stlc.term

exception Error

let hypo x hyps = Stlc.Var x

let cut x e1 e2 = Stlc.Let (x, e1, e2)

let true_intro _ = Stlc.UnitTerm

let and_intro e1 e2 = Stlc.Pair (e1, e2)

let and_elim1 e = Stlc.Fst e

let and_elim2 e = Stlc.Snd e

let imply_intro x e = Stlc.Lambda (x, e)

let imply_elim e1 e2 = Stlc.Apply (e1, e2)

let weaken _ _ e = e

let print e ppf =
  Format.fprintf ppf "Realizer:@\n  @[<hov>%t@]\n" (Stlc.print_term e) ;
  Format.fprintf ppf "Normal form:@\n  @[<hov>%t@]" (Stlc.print_term (Stlc.normalize [] e)) ;




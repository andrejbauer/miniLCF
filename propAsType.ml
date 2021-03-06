type t = Stlc.term

exception Error

let hypo x hyps = Stlc.Var x

let cut x (_,e1) (_,e2) _ = Stlc.Let (x, e1, e2)

let true_intro _ = Stlc.UnitTerm

let and_intro (_,e1) (_,e2) _ = Stlc.Pair (e1, e2)

let and_elim1 (_,e) _ = Stlc.Fst e

let and_elim2 (_,e) _ = Stlc.Snd e

let imply_intro x (_,e) _ = Stlc.Lambda (x, e)

let imply_elim (_,e1) (_,e2) _ = Stlc.Apply (e1, e2)

let weaken _ _ (_,e) _ = e

let print e ppf =
  Format.fprintf ppf "Realizer:@\n  @[<hov>%t@]\n" (Stlc.print_term e) ;
  Format.fprintf ppf "Normal form:@\n  @[<hov>%t@]" (Stlc.print_term (Stlc.normalize [] e)) ;




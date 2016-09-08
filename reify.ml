module Make (K : Lcf.KERNEL) : Lcf.KERNEL =
struct
  type ty = Formula.t

  type term =
    | Var of string
    | Let of string * term * term
    | Unit
    | Pair of term * term
    | Fst of term
    | Snd of term
    | Lambda of string * term
    | Apply of term * term

  type t = Derivation of term * K.t

exception Error

let consequent (Derivation (_, s)) = K.consequent s
let hypotheses (Derivation (_, s)) = K.hypotheses s

let hypo h gamma =
  Derivation (Var h, K.hypo h gamma)

let cut h (Derivation (e1, s1)) (Derivation (e2, s2)) =
  Derivation (Let (h, e1, e2), K.cut h s1 s2)

let true_intro gamma =
  Derivation (Unit, K.true_intro gamma)

let and_intro (Derivation (e1, s1)) (Derivation (e2, s2)) =
  Derivation (Pair (e1, e2), K.and_intro s1 s2)

let and_elim1 (Derivation (e, s)) =
  Derivation (Fst e, K.and_elim1 s)

let and_elim2 (Derivation (e, s)) =
  Derivation (Snd e, K.and_elim2 s)

let imply_intro h (Derivation (e, s)) =
  Derivation (Lambda (h, e), K.imply_intro h s)

let imply_elim (Derivation (e1, s1)) (Derivation (e2, s2)) =
  Derivation (Apply (e1, e2), K.imply_elim s1 s2)

let weaken h frml (Derivation (e, s)) =
  Derivation (e, K.weaken h frml s)

let rec print_term e ppf =
  match e with
  | Var x -> Format.fprintf ppf "%s" x
  | Let (x, e1, e2) -> Format.fprintf ppf "(let@ %s = %t in@ %t)" x (print_term e1) (print_term e2)
  | Unit -> Format.fprintf ppf "()"
  | Pair (e1, e2) -> Format.fprintf ppf "(%t,@ %t)" (print_term e1) (print_term e2)
  | Fst e -> Format.fprintf ppf "(fst %t)" (print_term e)
  | Snd e -> Format.fprintf ppf "(snd %t)" (print_term e)
  | Lambda (x, e) -> Format.fprintf ppf "(lambda %s,@ %t)" x (print_term e)
  | Apply (e1, e2) -> Format.fprintf ppf "(%t@ %t)" (print_term e1) (print_term e2)

let print (Derivation (e, s)) ppf =
  Format.fprintf ppf "@[<hov>%t@] ::@\n@[<hov>%t@]"
                 (print_term e)
                 (K.print s)
end

type t =
  | Atom of string
  | True
  | And of t * t
  | Imply of t * t

let to_string =
  let rec simple = function
    | Atom a -> a
    | True -> "True"
    | (And _ | Imply _) as frm -> "(" ^ implication frm ^ ")"

  and conjunct = function
    | And (frm1, frm2) -> conjunct frm1 ^ " ∧ " ^ simple frm2
    | (Atom _ | True | Imply _) as frm -> simple frm

  and implication = function
    | Imply (frm1, frm2) -> conjunct frm1 ^ " → " ^ implication frm2
    | (Atom _ | True | And _) as frm -> conjunct frm

  in
  implication

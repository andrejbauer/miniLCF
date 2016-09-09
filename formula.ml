type t =
  | Atom of string    (** atomic proposition *)
  | True              (** truth *)
  | And of t * t      (** conjunction *)
  | Imply of t * t    (** implication *)

let print =
  let rec simple frml ppf =
    match frml with
    | Atom a -> Format.fprintf ppf "%s" a
    | True -> Format.fprintf ppf "True"
    | (And _ | Imply _) as frml -> Format.fprintf ppf "(%t)" (implication frml)

  and conjunct frml ppf =
    match frml with
    | And (frml1, frml2) -> Format.fprintf ppf "%t ∧@ %t" (conjunct frml1) (conjunct frml2)
    | (Atom _ | True | Imply _) as frml -> simple frml ppf

  and implication frml ppf =
    match frml with
    | Imply (frml1, frml2) -> Format.fprintf ppf "%t@ →@ %t" (conjunct frml1) (implication frml2)
    | (Atom _ | True | And _) as frml -> conjunct frml ppf

  in
  implication

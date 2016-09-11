type rule =
  | Hypo of string * Context.t
  | Cut of string * t * t
  | True_intro of Context.t
  | And_intro of t * t
  | And_elim1 of t
  | And_elim2 of t
  | Imply_intro of string * t
  | Imply_elim of t * t
  | Weaken of string * Formula.t * t

and t = { step : rule ; conclusion : Judgement.t }

exception Error

let conclude r j = { step = r ; conclusion = j }

let hypo x jdg = conclude (Hypo (x, fst jdg)) jdg

let cut x (_,d1) (_,d2) = conclude (Cut (x, d1, d2))

let true_intro jdg = conclude (True_intro (fst jdg)) jdg

let and_intro (_,d1) (_,d2) = conclude (And_intro (d1, d2))

let and_elim1 (_,d) = conclude (And_elim1 d)

let and_elim2 (_,d) = conclude (And_elim2 d)

let imply_intro x (_,d) = conclude (Imply_intro (x, d))

let imply_elim (_,d1) (_,d2) = conclude (Imply_elim (d1, d2))

let weaken x a (_,d) = conclude (Weaken (x, a, d))

let print d ppf =
  let k = ref 0 in
  let rec pr d =
    let print_line rule refs = 
      incr k ;
      Format.fprintf ppf "(%d) @[<hov>%t\t\t by %s (%t)@]@]@\n"
                     !k
                     (Judgement.print d.conclusion)
                     rule
                     (fun ppf -> Format.pp_print_list
                                   ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
                                   (fun ppf j -> Format.fprintf ppf "%d" j)
                                   ppf
                                   refs)
    in
    match d.step with

    | Hypo (x, _) -> 
       print_line ("HYP-" ^ x) []

    | Cut (x, d1, d2) ->
       pr d1 ;
       let k1 = !k in
       pr d2 ;
       let k2 = !k in
       print_line ("CUT-" ^ x) [k1; k2]

    | True_intro _ ->
       print_line "TRUE" []

    | And_intro (d1, d2) ->
       pr d1 ;
       let k1 = !k in
       pr d2 ;
       let k2 = !k in
       print_line "AND-INTRO" [k1; k2]

    | And_elim1 d ->
       pr d ;
       let j = !k in
       print_line "AND-ELIM-L" [j]

    | And_elim2 d ->
       pr d ;
       let j = !k in
       print_line "AND-ELIM-R" [j]

    | Imply_intro (x, d) ->
       pr d ;
       let j = !k in
       print_line ("IMPLY-INTRO-" ^ x) [j]

    | Imply_elim (d1, d2) ->
       pr d1 ;
       let k1 = !k in
       pr d2 ;
       let k2 = !k in
       print_line "IMPLY-ELIM" [k1; k2]

    | Weaken (x, a, d) ->
       pr d ;
       let j = !k in
       print_line ("WEAKEN-" ^ x) [j]
  in
  Format.fprintf ppf "Proof:@\n";
  pr d ;
  Format.fprintf ppf "QED@\n"

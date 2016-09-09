type t =
  | Hypo of string * Context.t
  | Cut of string * t * t
  | True_intro of Context.t
  | And_intro of t * t
  | And_elim1 of t
  | And_elim2 of t
  | Imply_intro of string * t
  | Imply_elim of t * t
  | Weaken of string * Formula.t * t

exception Error

let rec conclusion = function
  | Hypo (h, gamma) ->
     begin
       match Context.lookup h gamma with
       | Some a -> a
       | None -> raise Error
     end

  | Cut (h, d1, d2) -> conclusion d2

  | True_intro _ -> Formula.True

  | And_intro (d1, d2) ->
     Formula.And (conclusion d1, conclusion d2)

  | And_elim1 d ->
     begin
       match conclusion d with
       | Formula.And (a, _) -> a
       | _ -> raise Error
     end

  | And_elim2 d ->
     begin
       match conclusion d with
       | Formula.And (_, b) -> b
       | _ -> raise Error
     end

  | Imply_intro (h, d) ->
     begin
       match Context.lookup h (context d) with
       | Some a -> Formula.Imply (a, conclusion d)
       | None -> raise Error
     end
        
  | Imply_elim (d1, d2) ->
     begin
       match conclusion d1 with
       | Formula.Imply (_, b) -> b
       | _ -> raise Error
     end

  | Weaken (_, _, d) ->
     conclusion d

and context : t -> Context. t = function
  | Hypo (_, gamma) -> gamma
  | Cut (_, d1, d2) -> context d1
  | True_intro gamma -> gamma
  | And_intro (d1, _) -> context d1
  | And_elim1 d -> context d
  | And_elim2 d -> context d
  | Imply_intro (h, d) ->
     begin 
       match Context.extract h (context d) with
       | Some (_, d') -> d'
       | None -> raise Error
     end
  | Imply_elim (d1, _) -> context d1
  | Weaken (x,a,d) -> Context.extend (x,a) (context d)

let hypo (h : string) gamma = Hypo (h, gamma)

let cut h jdg1 jdg2 = Cut (h, jdg1, jdg2)

let true_intro gamma = True_intro gamma

let and_intro jdg1 jdg2 = And_intro (jdg1, jdg2)

let and_elim1 jdg = And_elim1 jdg

let and_elim2 jdg = And_elim2 jdg

let imply_intro h jdg = Imply_intro (h, jdg)

let imply_elim jdg1 jdg2 = Imply_elim (jdg1, jdg2)

let weaken h frml jdg = Weaken (h, frml, jdg)

let print d ppf =
  let k = ref 0 in
  let rec pr d =
    let jdg = (context d, conclusion d) in
    let print_line rule refs = 
      incr k ;
      Format.fprintf ppf "(%d) @[<hov>%t\t\t by %s (%t)@]@]@\n"
                     !k
                     (Judgement.print jdg)
                     rule
                     (fun ppf -> Format.pp_print_list
                                   ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
                                   (fun ppf j -> Format.fprintf ppf "%d" j)
                                   ppf
                                   refs)
    in
    match d with

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

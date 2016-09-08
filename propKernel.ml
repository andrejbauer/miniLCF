type t = (string * Formula.t) list * Formula.t

exception Error

let hypotheses (gamma, _) = gamma

let consequent (_, a) = a

let print_context gamma ppf =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ " )
    (fun ppf (h,a) -> Format.fprintf ppf "%s: @[<h>%t@]" h (Formula.print a))
    ppf
    gamma

let print (gamma, a) ppf =
  Format.fprintf ppf "%t@ ⊢ %t" (print_context gamma) (Formula.print a)

let check b =
  if b then () else raise Error

let lookup h ctx =
  try
    List.assoc h ctx
  with Not_found -> raise Error

let extract h ctx =
  let rec fold delta = function
    | [] -> raise Error
    | (h',a) :: gamma when h = h' -> a, delta @ gamma
    | g :: gamma -> fold (g :: delta) gamma
  in
  fold [] ctx

let rec is_context = function
  | [] -> true
  | (h,_) :: lst -> not (List.mem_assoc h lst) && is_context lst

let rec equal_context ctx1 ctx2 =
  let rec contains ctx2 = function
    | [] -> true
    | (h1,a1)::ctx1 ->
       check (lookup h1 ctx2 = a1) ;
       contains ctx2 ctx1
  in
  contains ctx1 ctx2 && contains ctx2 ctx1

let hypo h gamma =
  check (is_context gamma) ;
  (gamma, lookup h gamma)

let cut h (gamma, a) (delta, b) =
  let c, eta = extract h delta in
  check (equal_context gamma eta) ;
  check (c = a) ;
  (gamma, b)

let true_intro gamma = 
  check (is_context gamma) ;
  (gamma, Formula.True)

let and_intro (gamma, a) (delta, b) =
  check (equal_context gamma delta) ;
  (gamma, Formula.And (a, b))

let and_elim1 (gamma, a) =
  match a with
  | Formula.And (b, _) -> (gamma, b)
  | _ -> raise Error

let and_elim2 (gamma, a) =
  match a with
  | Formula.And (_, c) -> (gamma, c)
  | _ -> raise Error

let imply_intro h (gamma, a) =
  let b, delta = extract h gamma in
  (delta, Formula.Imply (b, a))

let imply_elim (gamma, a) (delta, b) =
  check (equal_context gamma delta) ;
  match a with
  | Formula.Imply (c, d) when c = b -> (gamma, d)
  | _ -> raise Error

let weaken h b (gamma, a) =
  if List.mem_assoc h gamma
  then raise Error
  else ((h, b) :: gamma, a)

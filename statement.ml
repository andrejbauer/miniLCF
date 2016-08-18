type t =
  | Statement of (string * Formula.t) list * Formula.t

exception Error

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

let hypothesis h gamma =
  check (is_context gamma) ;
  Statement (gamma, lookup h gamma)

let cut h (Statement (gamma, a)) (Statement (delta, b)) =
  check (lookup h delta = a) ;
  Statement (gamma, b)

let true_intro gamma = 
  check (is_context gamma) ;
  Statement (gamma, Formula.True)

let and_intro (Statement (gamma, a)) (Statement (delta, b)) =
  check (equal_context gamma delta) ;
  Statement (gamma, Formula.And (a, b))

let and_elim1 (Statement (gamma, a)) =
  match a with
  | Formula.And (b, _) -> Statement (gamma, b)
  | _ -> raise Error

let and_elim2 (Statement (gamma, a)) =
  match a with
  | Formula.And (_, c) -> Statement (gamma, c)
  | _ -> raise Error

let imply_intro h (Statement (gamma, a)) =
  let b, delta = extract h gamma in
  Statement (delta, Formula.Imply (b, a))

let imply_elim (Statement (gamma, a)) (Statement (delta, b)) =
  check (equal_context gamma delta) ;
  match a with
  | Formula.Imply (c, d) when c = b -> Statement (gamma, d)
  | _ -> raise Error

let weaken h b (Statement (gamma, a)) =
  if List.mem_assoc h gamma
  then raise Error
  else Statement ((h, b) :: gamma, a)

let to_string (Statement (gamma, a)) =
  String.concat ", " (List.map (fun (h,a) -> h ^ ":" ^ Formula.to_string a) gamma) ^ 
  " ⊢ " ^ Formula.to_string a

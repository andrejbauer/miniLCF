type t =
  | Statement of Formula.t list * Formula.t

exception Error

let check b =
  if b then () else raise Error

let hypothesis n gamma =
  check (0 <= n && n < List.length gamma) ;
  Statement (gamma, List.nth gamma n)

let true_intro gamma = 
  Statement (gamma, Formula.True)

let and_intro (Statement (gamma, a)) (Statement (delta, b)) =
  check (gamma = delta) ;
  Statement (gamma, Formula.And (a, b))

let and_elim1 (Statement (gamma, a)) =
  match a with
  | Formula.And (b, _) -> Statement (gamma, b)
  | _ -> raise Error

let and_elim2 (Statement (gamma, a)) =
  match a with
  | Formula.And (_, c) -> Statement (gamma, c)
  | _ -> raise Error

let imply_intro (Statement (gamma, a)) =
  match gamma with
  | b :: delta -> Statement (delta, Formula.Imply (b, a))
  | [] -> raise Error

let imply_elim (Statement (gamma, a)) (Statement (delta, b)) =
  check (gamma = delta) ;
  match a with
  | Formula.Imply (c, d) when c = b -> Statement (gamma, d)
  | _ -> raise Error

let weaken b (Statement (gamma, a)) =
  Statement (b :: gamma, a)

let rec list_remove k lst =
  match lst with
  | [] -> raise Error
  | (x :: lst') ->
     if k = 0 then lst' else x :: (list_remove (k-1) lst')

let list_exchange i j lst =
  try
    let x = List.nth lst i
    and y = List.nth lst j in
    let rec exchange k lst =
      match lst with
      | [] -> []
      | lst when k > i && k > j -> lst
      | z :: lst' ->
         let z' = (if k = i then x
                   else if k = j then y
                   else z) in
         z' :: (exchange (k+1) lst')
    in
    exchange 0 lst
  with Failure _ -> raise Error

let contract i j (Statement (gamma, a)) =
  let n = List.length gamma in
  check (0 <= i && i < n) ;
  check (0 <= j && j < n) ;
  check (List.nth gamma i = List.nth gamma j) ;
  let delta = list_remove j gamma in
  Statement (delta, a)

let exchange i j (Statement (gamma, a)) =
  let delta = list_exchange i j gamma in
  Statement (delta, a)

let to_string (Statement (gamma, a)) =
  String.concat ", " (List.map Formula.to_string gamma) ^ " ⊢ " ^ Formula.to_string a

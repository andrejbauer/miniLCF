type t = Judgement.t

exception Error

let judgement (jdg : t) = jdg

let print jdg ppf = Judgement.print jdg ppf

let check b =
  if b then () else raise Error

let lookup h ctx =
  try
    List.assoc h ctx
  with Not_found -> raise Error

let hypo h gamma =
  match Context.lookup h gamma with
  | Some a -> (gamma, a)
  | None -> raise Error

let cut h (gamma, a) (delta, b) =
  match Context.extract h delta with
  | Some (c, eta) ->
     check (Context.equal gamma eta) ;
     check (c = a) ;
     (gamma, b)
  | None -> raise Error

let true_intro gamma = 
  (gamma, Formula.True)

let and_intro (gamma, a) (delta, b) =
  check (Context.equal gamma delta) ;
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
  match Context.extract h gamma with
  | Some (b, delta) -> (delta, Formula.Imply (b, a))
  | None -> raise Error

let imply_elim (gamma, a) (delta, b) =
  check (Context.equal gamma delta) ;
  match a with
  | Formula.Imply (c, d) when c = b -> (gamma, d)
  | _ -> raise Error

let weaken h b (gamma, a) =
  try
    (Context.extend (h, b) gamma, a)
  with
  | Context.Invalid_context -> raise Error

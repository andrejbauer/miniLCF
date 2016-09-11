type t = (string * Formula.t) list

exception Invalid_context 

let fold = List.fold_left

let empty = []

let is_empty = function
  | [] -> true
  | _::_ -> false

let extend (x,a) gamma =
  if List.mem_assoc x gamma
  then raise Invalid_context
  else ((x,a) :: gamma)

let make lst =
  List.fold_right extend lst empty

let extract h gamma =
  let rec fold delta = function
    | [] -> None
    | (h',a) :: gamma when h = h' -> Some (a, delta @ gamma)
    | g :: gamma -> fold (g :: delta) gamma
  in
  fold [] gamma

let rec find a = function
  | [] -> None
  | (x,b) :: _ when a = b -> Some x
  | _ :: ctx -> find a ctx

let rec lookup x = function
  | [] -> None
  | (y,a) :: _ when x = y -> Some a
  | _ :: ctx -> lookup x ctx

let rec equal ctx1 ctx2 =
  let rec contains ctx2 = function
    | [] -> true
    | (h1,a1)::ctx1 -> (lookup h1 ctx2 = Some a1) && contains ctx2 ctx1
  in
  contains ctx1 ctx2 && contains ctx2 ctx1

let print ?(vertical=false) gamma ppf =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf (if vertical then "@\n" else ",@ "))
    (fun ppf (x,a) -> Format.fprintf ppf "%s:%t" x (Formula.print a))
    ppf
    gamma


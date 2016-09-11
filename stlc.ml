(* A simply typed lambda calculus with let-binding. *)

type ty =
  | Primitive of string
  | UnitTy
  | Product of ty * ty
  | Arrow of ty * ty

type term =
  | Var of string
  | UnitTerm
  | Pair of term * term
  | Fst of term
  | Snd of term
  | Lambda of string * term
  | Apply of term * term
  | Let of string * term * term

type context = (string * ty) list

let (empty : context) = []

let rec lookup x = function
  | [] -> None
  | (y,a) :: _ when x = y -> Some a
  | _ :: ctx -> lookup x ctx

exception Type_error


(** [type_check ctx e a] checks that [e] has type [a] in context [ctx]. *)
let rec type_check ctx e a =
  match e with
  | (Var _ | UnitTerm | Pair _ | Fst _ | Snd _ | Apply _) as e ->
     if type_infer ctx e <> a then raise Type_error

  | Lambda (x, e) ->
     begin
       match a with
       | Arrow (b, c) -> type_check ((x,b)::ctx) e c
       | _ -> raise Type_error
     end

  | Let (x, e1, e2) ->
     let b = type_infer ctx e1 in
     type_check ((x,b)::ctx) e2 a

(** [type_infer ctx e] infers the type of [e] *)
and type_infer ctx = function
  | Var x ->
     begin
       try
         List.assoc x ctx
       with Not_found -> raise Type_error
     end

  | UnitTerm -> UnitTy

  | Pair (e1, e2) ->
     Product (type_infer ctx e1, type_infer ctx e2)

  | Fst e ->
     begin
       match type_infer ctx e with
       | Product (a, _) -> a
       | _ -> raise Type_error
     end

  | Snd e ->
     begin
       match type_infer ctx e with
       | Product (a, _) -> a
       | _ -> raise Type_error
     end

  | Lambda (x, e) -> raise Type_error

  | Apply (e1, e2) ->
     begin
       match type_infer ctx e1 with
       | Arrow (b, c) -> type_check ctx e2 b ; c
       | _ -> raise Type_error
     end

  | Let (x, e1, e2) ->
     let a = type_infer ctx e1 in
     type_infer ((x,a) :: ctx) e2

let rec normalize env = function

  | Var x ->
     begin
       try
         normalize env (List.assoc x env)
       with Not_found -> Var x
     end

  | UnitTerm ->
     UnitTerm

  | Pair (e1, e2) ->
     Pair (normalize env e1, normalize env e2)

  | Fst e ->
     begin
       match normalize env e with
       | Pair (v, _) -> v
       | v -> Fst v
     end

  | Snd e ->
     begin
       match normalize env e with
       | Pair (_, v) -> v
       | v -> Snd v
     end

  | Lambda (x, e) ->
     Lambda (x, normalize env e)

  | Apply (e1, e2) ->
     begin
       let v2 = normalize env e2 in
       match normalize env e1 with
       | Lambda (x, e) ->
          normalize ((x,v2)::env) e
       | v1 -> Apply (v1, v2)
     end

  | Let(x, e1, e2) ->
     let v1 = normalize env e1 in
     normalize ((x,v1) :: env) e2



let print_ty =
  let rec simple a ppf =
    match a with
    | Primitive p -> Format.fprintf ppf "%s" p
    | UnitTy -> Format.fprintf ppf "unit"
    | (Product _ | Arrow _) as a -> Format.fprintf ppf "(%t)" (arrow a)

  and product a ppf =
    match a with
    | Product (a1, a2) -> Format.fprintf ppf "%t × %t" (simple a1) (simple a2)
    | (Primitive _ | UnitTy | Arrow _) as a -> simple a ppf

  and arrow a ppf =
    match a with
    | Arrow (a1, a2) -> Format.fprintf ppf "%t → %t" (product a1) (arrow a2)
    | (Primitive _ | UnitTy | Product _) as a -> product a ppf

  in
  arrow

let print_term e ppf =
  let rec simple e ppf =
    match e with
    | Var x -> Format.fprintf ppf "%s" x
    | UnitTerm -> Format.fprintf ppf "()"
    | Pair (e1, e2) -> Format.fprintf ppf "(%t,@ %t)" (lambda e1) (lambda e2)
    | (Fst _ | Snd _ | Lambda _ | Apply _ | Let _) as e -> Format.fprintf ppf "(%t)" (lambda e)

  and apply e ppf =
    match e with
    | Fst e -> Format.fprintf ppf "fst@ %t" (simple e)
    | Snd e -> Format.fprintf ppf "snd@ %t" (simple e)
    | Apply (e1, e2) -> Format.fprintf ppf "%t@ %t" (apply e1) (simple e2)
    | (Var _ | UnitTerm | Pair _ | Lambda _ | Let _) as e -> simple e ppf

  and lambdas xs e ppf =
    match e with
    | Lambda (x, e) -> lambdas (x::xs) e ppf
    | _ ->
       let xs = List.rev xs in
       Format.fprintf ppf "λ %t . %t"
                      (fun ppf -> Format.pp_print_list
                                    ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
                                    (fun ppf x -> Format.fprintf ppf "%s" x)
                                    ppf
                                    xs)
                      (lambda e)

  and lambda e ppf =
    match e with
    | Lambda _ -> lambdas [] e ppf
    | Let (x, e1, e2) -> Format.fprintf ppf "let %s =@ %t in@ %t" x (apply e1) (lambda e2)
    | (Var _ | UnitTerm | Pair _ | Fst _ | Snd _ | Apply _) as e -> apply e ppf

  in
  Format.fprintf ppf "@[<hov>%t@]" (lambda e)

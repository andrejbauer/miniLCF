type goal = (string * Formula.t) list * Formula.t

exception InternalTacticError

type proof =
  | Failure of string
  | Goal of goal
  | Branch of proof list * (Statement.t list -> Statement.t)

let fail msg = Failure msg

type tactic = goal -> proof

let success s = 
  Branch ([], (function [] -> s | _ -> assert false))

let string_of_goal (gamma, a) =
  String.concat "\n" (List.map (fun (h,b) -> h ^ " : " ^ Formula.to_string b) (List.rev gamma)) ^
  "\n-------------------\n" ^
  Formula.to_string a

let rec string_of_proof = function
  | Failure msg -> msg
  | Goal g -> string_of_goal g
  | Branch (prfs, _) ->
     String.concat "\n*******\n" (List.map string_of_proof prfs)

let split_list n lst =
  let rec split us vs n =
    match n, vs with
    | 0, vs -> (List.rev us, vs)
    | n, (v :: vs) -> split (v :: us) vs (n - 1)
    | _, [] -> assert false
  in
  split [] lst n

let rec ap (t : tactic) = function
  | Failure msg -> Failure msg
  | Goal gl -> t gl
  | Branch (ps, g) ->
     let rec fold qs g = function
       | [] -> Branch (qs, g)
       | p :: ps ->
          begin
            match ap t p with
            | Failure msg -> Failure msg
            | Goal gl -> Goal gl
            | Branch (ps', f) ->
               let n = List.length ps' in
               let g lst = 
                 let lst1, lst2 = split_list n lst in
                 g (f lst1 :: lst2)
               in
               fold (ps' @ qs) g ps
          end
     in
     fold [] g ps


let ( ** ) t1 t2 : tactic =
  fun gl -> ap t2 (ap t1 (Goal gl))
    
let find p =
  let rec fnd = function
    | [] -> None
    | (h, x) :: xs when p h x -> Some (h, x)
    | _ :: xs -> fnd xs
  in
  fnd

let assumption (gamma, a) =
  match find (fun _ b -> b = a) gamma with
  | Some (h, _) -> success (Statement.hypothesis h gamma)
  | None -> fail "cannot find the assumption"

let exact h (gamma, a) =
  match find (fun h' _ -> h' = h) gamma with
  | Some (h, b) when b = a -> success (Statement.hypothesis h gamma)
  | Some _ -> fail "hypothesis mismatch"
  | None -> fail "no such hypothesis"
       
let k = ref 0 

let intro h (gamma, a) =
  match a with
  | Formula.Imply (b, c) ->
     Branch ([Goal ((h, b) :: gamma, c)],
             (function
               | [s] -> Statement.imply_intro h s
               | _ -> assert false))
  | _ -> fail "not an implication"

let split (gamma, a) =
  match a with
  | Formula.And (b, c) ->
     Branch (
         [Goal (gamma, b); Goal (gamma, c)],
         (function
           | [s1; s2] -> Statement.and_intro s1 s2
           | lst -> raise InternalTacticError))
  | _ -> fail "not a conjunction"

let extract p lst =
  let rec extr = function
  | [] -> (None, [])
  | x :: xs when p x -> (Some x, xs)
  | x :: xs -> let (y, ys) = extr xs in (y, x :: xs)
  in
  extr lst

let apply h (gamma, a) =
  match find (fun h' _ -> h = h') gamma with
  | Some (h, Formula.Imply (b, c)) when c = a ->
     let s1 = Statement.hypothesis h gamma in
     Branch (
         [Goal (gamma, b)],
         (function
           | [s2] -> Statement.imply_elim s1 s2
           | _ -> assert false)
       )
  | Some _ -> fail "invalid apply"
  | None -> fail "no such hypothesis"

let attempt (t : tactic) (gl : goal) =
  match t gl with
  | Failure _ -> Goal gl
  | (Goal _ | Branch _) as gl -> gl

let rec repeat (t : tactic) (gl : goal) =
  match t gl with
  | Failure _ -> Goal gl
  | (Goal _ | Branch _) as gl -> ap (repeat t) gl

let intros = 
  let intr =
    let k = ref 0 in
    fun gl -> incr k ; intro ("H" ^ string_of_int !k) gl
  in
  repeat intr

let destruct h h1 h2 (gamma, a) =
  match find (fun h' _ -> h' = h) gamma with
  | None -> fail "no such hypothesis"
  | Some (h, Formula.And (b, c)) -> 
     Branch ([Goal ((h1, b) :: (h2, c) :: gamma, a)],
           (function
             | _ -> raise InternalTacticError
           )
)
  | Some _ -> fail "cannot destruct"

let theorem frm (t : tactic)  = 
  match ap t (Goal ([], frm)) with
  | Branch ([], f) ->
     print_endline ("Theorem: " ^ Statement.to_string (f []))
  | prf -> print_endline (string_of_proof prf)


type goal = (string * Formula.t) list * Formula.t

exception InternalTacticError

type proof =
  | Failure of string
  | Goal of goal
  | Branch of proof list * (Statement.t list -> Statement.t)

let fail msg = Failure msg

let extract h ctx =
  let rec fold delta = function
    | [] -> None
    | (h',a) :: gamma when h = h' -> Some (a, delta @ gamma)
    | g :: gamma -> fold (g :: delta) gamma
  in
  fold [] ctx

let rec lookup h = function
  | [] -> None
  | (h',a) :: gamma when h' = h -> Some a
  | _ :: gamma -> lookup h gamma

let success s = 
  Branch ([], (function [] -> s | _ -> assert false))

let string_of_goal (gamma, a) =
  String.concat "\n" (List.map (fun (h,b) -> h ^ " : " ^ Formula.to_string b) gamma) ^
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

let rec ap t = function
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


let rec aps ts = function
  | Failure msg -> Failure msg
  | Goal gl ->
     begin
       match ts with
       | [t] -> t gl
       | _ -> fail "expected one tactic"
     end
  | Branch (ps, g) ->
     let rec fold qs g ps ts =
       match ps, ts with
       | [], [] -> Branch (qs, g)
       | [], _::_ -> fail "too many tactics"
       | _::_, [] -> fail "too few tactics"
       | p :: ps, t :: ts ->
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
               fold (ps' @ qs) g ps ts
          end
     in
     fold [] g ps ts


let ( ** ) t1 t2 gl = ap t2 (ap t1 (Goal gl))

let ( ^^ ) t1 t2s gl = aps t2s (ap t1 (Goal gl))
      
let rec assumption (gamma, a) = 
  let rec find = function
    | [] -> fail "cannot find the assumption"
    | (h, b) :: _ when b = a -> success (Statement.hypothesis h gamma)
    | _ :: delta -> find delta
  in
  find gamma

let exact h (gamma, a) =
  match lookup h gamma with
  | Some b when b = a -> success (Statement.hypothesis h gamma)
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

let apply h (gamma, a) =
  match lookup h gamma with
  | Some (Formula.Imply (b, c)) when c = a ->
     let s1 = Statement.hypothesis h gamma in
     Branch (
         [Goal (gamma, b)],
         (function
           | [s2] -> Statement.imply_elim s1 s2
           | _ -> assert false)
       )
  | Some _ -> fail "invalid apply"
  | None -> fail "no such hypothesis"

let attempt t gl =
  match t gl with
  | Failure _ -> Goal gl
  | (Goal _ | Branch _) as gl -> gl

let rec repeat t gl =
  match t gl with
  | Failure _ -> Goal gl
  | Goal _ as gl -> gl
  | Branch (prfs, f) -> Branch (List.map (ap (repeat t)) prfs, f)

let intros = 
  let intr =
    let k = ref 0 in
    fun gl -> incr k ; intro ("H" ^ string_of_int !k) gl
  in
  repeat intr

let destruct h h1 h2 (gamma, a) =
  match extract h gamma with
  | None -> fail "no such hypothesis"
  | Some (Formula.And (b, c), delta) -> 
     Branch ([Goal ((h1, b) :: (h2, c) :: delta, a)],
           (function
             | [s] -> (* h1:b, h2:c, delta |- a *)
                let s = Statement.weaken h (Formula.And (b, c)) s in (* h:b/\c, h1:b, h2:c, delta |- a *)
                let t = Statement.hypothesis h gamma in (* h: b /\ c, delta |- b /\ c *)
                let t1 = Statement.and_elim1 t (* h : b /\ c, delta |- b *)
                and t2 = Statement.weaken h1 b (Statement.and_elim2 t) in (* h1:b, h:b/\c, delta |- c *)
                Statement.cut h1 t1 (Statement.cut h2 t2 s) 
             | _ -> raise InternalTacticError
           ))
  | Some _ -> fail "cannot destruct"

let theorem frm t  = 
  match ap t (Goal ([], frm)) with
  | Branch ([], f) ->
     begin
       match f [] with
       | Statement.Statement ([], frm') as s when frm = frm' ->
          print_endline ("Theorem: " ^ Statement.to_string s)
       | s -> print_endline ("We proved " ^ Statement.to_string s ^ " instead of " ^ Formula.to_string frm)
     end
  | prf -> print_endline (string_of_proof prf)


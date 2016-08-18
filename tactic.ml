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

let branch ps g =
  let rec fold = function
  | [] -> [], (function [] -> [] | _::_ -> raise InternalTacticError)
  | (Branch ([], h)) :: ps ->
     let s = h [] in
     let ps, r = fold ps in
     ps, (fun lst -> s :: r lst)
  | p :: ps ->
     let ps, r = fold ps in
     (p :: ps), (function (s :: lst) -> s :: (r lst) | [] -> raise InternalTacticError)
  in
  let ps, r = fold ps in
  Branch (ps, fun lst -> g (r lst))

let rec ap t = function
  | Failure msg -> Failure msg
  | Goal gl -> t gl
  | Branch (ps, g) -> branch (List.map (ap t) ps) g

let aps ts gl =
  let rec aps = function
    | ts, Failure msg -> ts, Failure msg
    | t::ts , Goal gl -> ts, t gl
    | [], Goal gl -> [], fail "too few tactics"
    | ts, Branch (ps, g) ->
       let rec fold ts qs = function
         | [] -> ts, branch (List.rev qs) g
         | p :: ps ->
            let ts, q = aps (ts, p) in
            fold ts (q :: qs) ps
       in
       fold ts [] ps
  in
  match aps (ts, gl) with
  | [], p -> p
  | _::_, _ -> fail "too many tactics"

let idtac gl = Goal gl

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
  let rec collect bs = function
    | c when c = a ->
       Branch (List.rev bs,
               List.fold_left (fun s t -> Statement.imply_elim s t) (Statement.hypothesis h gamma))
    | Formula.Imply (b, c) -> collect (Goal (gamma, b) :: bs) c
    | _ -> fail "cannot apply"
  in
  match lookup h gamma with
  | Some c -> collect [] c
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

let rec first ts gl =
  match ts with
  | [] -> fail "no applicable tactic"
  | t :: ts ->
    begin
      match t gl with
      | Failure _ -> first ts gl
      | (Goal _ | Branch _) as prf -> prf
    end 
  
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


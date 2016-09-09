module Make (K : Lcf.KERNEL) =
struct

  type goal = Judgement.t

  exception InternalError of string

  type state =
    | Failure of string
    | Goal of goal
    | Branch of state list * (K.t list -> K.t)

  let fail msg = Failure msg

  let success s = 
    Branch ([], (function [] -> s | _ -> assert false))

  let print_goal = Judgement.print ~vertical:true

  let print_state prf ppf =
    let rec print ks prf ppf =
      match prf with
      | Failure msg -> Format.fprintf ppf "Failure: %s" msg
      | Goal g -> print_goal g ppf
      | Branch (prfs, _) ->
         List.iteri 
           (fun k prf -> 
             let ks = ks ^ "." ^ string_of_int k in
             Format.fprintf ppf "GOAL %s:@\n%t@\n@\n" ks (print ks prf))
           prfs
    in
    print "" prf ppf

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
      | [] -> [], (function [] -> [] | _::_ -> raise (InternalError "branch (1)"))
      | (Branch ([], h)) :: ps ->
         let s = h [] in
         let ps, r = fold ps in
         ps, (fun lst -> s :: r lst)
      | p :: ps ->
         let ps, r = fold ps in
         (p :: ps), (function (s :: lst) -> s :: (r lst) | [] -> raise (InternalError "branch (2)"))
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
                             
  let rec assumption (ctx, a) =
    match Context.find a ctx with
    | None -> fail "cannot find the assumption"
    | Some h -> success (K.hypo h ctx)

  let exact h (ctx, a) =
    match Context.lookup h ctx with
    | Some b when b = a -> success (K.hypo h ctx)
    | Some _ -> fail "hypothesis mismatch"
    | None -> fail "no such hypothesis"
                   
  let intro h (gamma, a) =
    match a with
    | Formula.Imply (b, c) ->
       Branch ([Goal (Context.extend (h, b) gamma, c)],
               (function
                 | [s] -> K.imply_intro h s
                 | _ -> assert false))
    | _ -> fail "not an implication"

  let split (gamma, a) =
    match a with
    | Formula.And (b, c) ->
       Branch (
           [Goal (gamma, b); Goal (gamma, c)],
           (function
             | [s1; s2] -> K.and_intro s1 s2
             | lst -> raise (InternalError "split")))
    | _ -> fail "not a conjunction"

  let apply h (ctx, a) =
    let rec collect bs = function
      | c when c = a ->
         Branch (List.rev bs,
                 List.fold_left (fun s t -> K.imply_elim s t) (K.hypo h ctx))
      | Formula.Imply (b, c) -> collect (Goal (ctx, b) :: bs) c
      | _ -> fail "cannot apply"
    in
    match Context.lookup h ctx with
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
    match Context.extract h gamma with
    | None -> fail "no such hypothesis"
    | Some (Formula.And (b, c), delta) -> 
       Branch ([Goal (Context.extend (h1, b) (Context.extend (h2, c) delta), a)],
               (function
                 | [s] -> (* h1:b, h2:c, delta |- a *)
                    let s = K.weaken h (Formula.And (b, c)) s in (* h:b/\c, h1:b, h2:c, delta |- a *)
                    let t = K.hypo h gamma in (* h: b /\ c, delta |- b /\ c *)
                    let t1 = K.and_elim1 t (* h : b /\ c, delta |- b *)
                    and t2 = K.weaken h1 b (K.and_elim2 t) in (* h1:b, h:b/\c, delta |- c *)
                    K.cut h1 t1 (K.cut h2 t2 s) 
                 | _ -> raise (InternalError "destruct")
               ))
    | Some _ -> fail "cannot destruct"

  let theorem frml tctc = 
    match ap tctc (Goal (Context.empty, frml)) with
    | Branch ([], f) ->
      begin
        let s = f [] in
        match K.context s, K.conclusion s with
        | (ctx, frml') when Context.is_empty ctx && frml = frml' -> 
           Format.printf "Theorem:@\n@[<hov>%t@]@." (K.print s)
        | _ -> raise (InternalError "theorem")
      end
    | prf -> Format.printf "Remaining goals:@\n%t" (print_state prf)

end

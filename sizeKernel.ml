type t = int
exception Error
let hypo _ _ = 1
let cut _ (_,d1) (_,d2) _ = d1 + d2 + 1
let true_intro _ = 1
let and_intro (_,d1) (_,d2) _ = d1 + d2 + 1
let and_elim1 (_,d) _ = d + 1
let and_elim2 (_,d) _ = d + 1
let imply_intro _ (_,d) _ = d + 1
let imply_elim (_,d1) (_,d2) _ = d1 + d2
let weaken _ _ (_,d) _ = d + 1
let print k ppf = Format.fprintf ppf "size %d" k

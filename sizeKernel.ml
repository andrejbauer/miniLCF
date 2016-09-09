type t = int
exception Error
let hypo _ _ = 1
let cut _ d1 d2 = d1 + d2 + 1
let true_intro _ = 1
let and_intro d1 d2 = d1 + d2 + 1
let and_elim1 d = d + 1
let and_elim2 d = d + 1
let imply_intro _ d = d + 1
let imply_elim d1 d2 = d1 + d2
let weaken _ _ d = d + 1
let print k ppf = Format.fprintf ppf "size %d" k

type t = Context.t * Formula.t

let print ?(vertical=false) (gamma, a) ppf =
  if vertical then
    Format.fprintf ppf "@[<v>%t@]@\n-----------------@\n@[<h>%t@]"
                   (Context.print gamma)
                   (Formula.print a)                   
  else
    Format.fprintf ppf "@[<hov>%t@ ⊢ %t@]"
                   (Context.print gamma)
                   (Formula.print a)

let sequence sep print_entry lst ppf =
  let rec print lst ppf =
    match lst with
    | [] -> Format.fprintf ppf ""
    | [x] -> Format.fprintf ppf "%t" (print_entry x)
    | x :: lst -> Format.fprintf ppf "%t%s%t" (print_entry x) sep (print lst)
  in
  print lst ppf

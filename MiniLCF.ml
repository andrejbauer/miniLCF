open Formula
open Statement
open Tactic

let formula str = Parser.formula Lexer.token (Lexing.from_string str)

let p = hypothesis "H" [("H", formula {| A /\ B |})] ;;

let q = and_elim2 p ;;

(* print_endline (Statement.to_string q) ;; *)

let p = formula {| (C -> D) -> True -> (A -> B) |} ;;

(* print_endline (Formula.to_string p) ;; *)

let g = ([("H1", formula {|A -> B|} );
          ("H2", formula {| B |} )],
         formula {| A /\ B /\ A |}) ;;

Tactic.theorem (formula {| A -> B -> A /\ B |}) (intros ** split ** assumption) ;;
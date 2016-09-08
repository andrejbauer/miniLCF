let formula str = Parser.formula Lexer.token (Lexing.from_string str)

let p = Statement.hypothesis "H" [("H", formula {| A /\ B |})] ;;

let q = Statement.and_elim2 p ;;

(* print_endline (Statement.to_string q) ;; *)

let p = formula {| (C -> D) -> True -> (A -> B) |} ;;

(* print_endline (Formula.to_string p) ;; *)

let g = ([("H1", formula {|A -> B|} );
          ("H2", formula {| B |} )],
         formula {| A /\ B /\ A |}) ;;

module T = Tactic.Make (Reify.Make (Statement))

open T ;;

theorem (formula {| A -> B -> A |}) (intros ** assumption) ;;

theorem (formula {| A -> B -> A /\ B |}) (intros ** split ** assumption) ;;

theorem (formula {| A /\ B -> B /\ A |})
               (intro "H" **
                destruct "H" "H1" "H2" **
                split ** assumption) ;;

theorem (formula {| (A -> A) /\ (C /\ D -> C) |})
               (split ^^
                [intros ** assumption ;
                 intro "H" ** destruct "H" "H1" "H2" ** exact "H1"]) ;;

let auto = repeat (intros ** repeat split) ** attempt assumption ;;
                        
theorem (formula {| X -> A -> X /\ ((C -> A) /\ (B -> X)) |}) auto ;;

theorem (formula {| (A -> B -> C) -> (A -> B) -> (A -> C) |})
               (intro "J" **
                intro "K" **
                intro "L" **
                apply "J" ^^ [assumption; apply "K" ** assumption]) ;;
                        

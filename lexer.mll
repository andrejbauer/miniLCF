{
  open Parser
  open Lexing
}

let atom = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9' '\'']*

rule token = parse
  | [' ' '\t']      { token lexbuf }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "/\\"           { AND }
  | "->"            { IMPLY }
  | "True"          { TRUE }
  | atom            { ATOM (lexeme lexbuf) }
  | eof             { EOF }

{
}

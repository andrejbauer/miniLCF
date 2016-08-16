%{
%}

%token AND IMPLY TRUE
%token LPAREN RPAREN
%token <string> ATOM
%token EOF

%start formula
%type <Formula.t> formula

%%

formula:
    | implication EOF            { $1 }

implication:
    | conjunct                   { $1 }
    | conjunct IMPLY implication { Formula.Imply ($1, $3) }

conjunct:
    | simple                     { $1 }
    | conjunct AND simple        { Formula.And ($1, $3) }

simple:
    | ATOM                       { Formula.Atom $1 }
    | TRUE                       { Formula.True }
    | LPAREN implication RPAREN  { $2 }

%%

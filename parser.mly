%{

%}

%token EOF
%token LBRACE, RBRACE
%token LPAR, RPAR
%token FUN, MAIN
%token <string> IDENT

%start file
%type <unit> file

%%

file:
| EOF { () }
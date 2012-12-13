%{
(* parse.mly
   Parser for... something.


*)

open Backend


%}

%token LPAREN RPAREN LBRACK RBRACK BANG AT COMMA EOF
%token NULL
%token <int> INT
%token <string> SYMBOL
/*%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <bool> BOOLEAN
*/

%type <Backend.syntree list> main
%start main

%%

main:  
	  /* EMPTY */ 
	  	{[]}
	| exprlist
	  	{$1}
	;


exprlist:
          expr
                { [$1] }
        | expr exprlist
                { $1 :: $2 }
        ;


expr:
          value
                { Value( $1 ) }
        | setexpr
                { $1 }
        | funcallexpr
                { $1 }
        | matchexpr
                { $1 }
        ;

value:
          INT
                { Int( $1 ) }
        | SYMBOL
                { Symbol( $1 ) }
        | LBRACK exprlist RBRACK
                { Tuple( List.length $2, $2 ) }
        | NULL
                { Null }
        ;

/*
valuelist:
          value
                { [$1] }
        | value valuelist
                { $1 :: $2 }
        ;
*/

funcallexpr:
          LPAREN expr expr RPAREN
                { Apply( $2, $3 ) }
        ;

setexpr:
          LPAREN BANG expr expr RPAREN
                { Set( $3, $4 ) }

matchexpr:
          LPAREN AT expr expr RPAREN
                { Match( $3, $4 ) }

%%


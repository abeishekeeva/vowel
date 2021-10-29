
%token PLUS MINUS TIMES DIVIDE ASSIGN STATE IF THEN ELSE EOF 
             MODULO LSTHN GRTRTHN INCR DECR NEGATE AND OR GTEQT LTEQT
             LPAREN RPAREN LCURLY RCURLY LBRACK RBRACK FOR PRINT RETURN    
             STRUCT BOOL INT SEMICOLON UNION INTERSECTION
             STRING ARRAY CONTINUE BREAK VOID NEGATIVE
%token <int> LITERAL
%token <string> ID

%left STATE
%right IF 
%right OR AND
%left UNION INTERSECTION
%right ASSIGN LSTHN GRTRTHN GTEQT LTEQT
%left PLUS MINUS INCR DECR
%left TIMES DIVIDE MODULO

%start expr
%type <Ast.expr> expr

%%
 
expr:
  expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| expr STATE  expr { Binop($1, Fin, $3) }
| expr MODULO   expr { Binop($1, Modl, $3)}
| expr INCR   expr { Binop($1, Incr, $3)}
| expr DECR  expr { Binop($1, Decr, $3)}
| NEGATE expr        {Binop($2, Negate)}
| NEGATIVE expr    {Binop($2, Negative)}
| expr LSTHN expr {Binop($1, Lsthn, $3)}
| expr LTEQT expr {Binop ($1, Lteqt, $3)}
| expr GTEQT expr {Binop($1, Gteqt, $3)}
| expr GRTRTHN expr {Binop($1, Grtrthn, $3)}
| expr AND expr {Binop($1, And, $3)}
| expr OR expr {Binop($1, Or, $3)}

| IF expr THEN expr ELSE expr { Cond(If, $2, Then, $4, Else, $6) }
| ID ASSIGN expr { Assignment($1, Ass, $3) } 
| ID               { Id($1)  }
| LITERAL          { Lit($1) }










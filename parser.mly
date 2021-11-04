
%token      INT STRING BOOL VOID ARRAY                                  /* literals         */
            PLUS MINUS TIMES DIVIDE MODULO                              /* arithmetic ops   */
            COMPEQUAL COMPNOTEQUAL LESSTHAN GREATERTHAN GTEQT LTEQT     /* comparison ops   */
            UNION INTERSECTION                                          /* set ops          */
            IF THEN ELSE FOR WHILE CONTINUE BREAK SEMICOLON RETURN EOF  /* control flow     */ 
            LPAREN RPAREN LCURLY RCURLY LBRACK RBRACK                   /* parens, brackets */
            ASSIGN INCREMENT DECREMENT                                  /* assignment ops   */
            AND OR NEGATE                                               /* boolean ops      */
            PRINT COMMA                                                 /* misc             */   
%token NEW            
%token <int> INTL
%token <string> IDL
%token <bool> BOOLL

%left SEMICOLON
%right ASSIGN INCREMENT DECREMENT
%right IF THEN ELSE
%left OR
%left AND
%right NEGATE
%left COMPEQUAL COMPNOTEQUAL
%left LESSTHAN GREATERTHAN GTEQT LTEQT
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left UNION INTERSECTION
%left LPAREN RPAREN LCURLY RCURLY LBRACK RBRACK

%start expr
%type <Ast.expr> expr

%%
typ:
    VOID {Void}
    |INT {Int}
    |STRING {String}
    |typ LBRACK RBRACK {Array($1)}

make_arrayL:
    {[]}
    |LCURLY args RCURLY {List.rev $2}



expr:
    /* Literals                             */
    INTL                      { Int($1)            }   
    | IDL                     { Id($1)         }
    | BOOLL                  { Bool($1)           }
    /* Arithmetic Operators                     */
    | expr PLUS   expr       { Binop($1, Add, $3) }
    | expr MINUS  expr       { Binop($1, Sub, $3) }
    | expr TIMES  expr       { Binop($1, Mul, $3) }
    | expr DIVIDE expr       { Binop($1, Div, $3) }
    | expr MODULO expr       { Binop($1, Mod, $3) }
    | MINUS expr %prec TIMES { Unop(Negative, $2) }
    /* Comparison Operators                 */
    | expr COMPEQUAL expr    { Binop($1, Compeq, $3) }
    | expr COMPNOTEQUAL expr { Binop($1, Compnoteq, $3) }
    | expr LESSTHAN expr     { Binop($1, Lessthan, $3) }
    | expr GREATERTHAN expr  { Binop($1, Greaterthan, $3) }
    | expr GTEQT expr        { Binop($1, Gteqt, $3) }
    | expr LTEQT  expr       { Binop($1, Lteqt, $3) }
    /* Set Operators                        */
    | expr UNION expr        { Binop($1, Union, $3) }
    | expr INTERSECTION expr { Binop($1, Intersect, $3) }
    /* Parens and brackets */
    | LPAREN expr RPAREN     { $2 }
    | LCURLY expr RCURLY     { $2 }
    | LBRACK expr RBRACK     {$2}
    /* Assignment Operators   */
    | IDL ASSIGN expr         { Assign($1, $3) }
    | IDL INCREMENT expr      { Increment($1, $3) }
    | IDL DECREMENT expr      { Decrement($1, $3) }
    /* Boolean Operators  */
    | expr AND expr          { Bool($1, And, $3) }
    | expr OR expr           { Bool($1, Or, $3) }
    | NEGATE expr            {  Unop(Negate, $2)   } 
    /* Arrays */
    |NEW typ LBRACK expr RBRACK make_arrayL {ArrayL($2,$4,$6)}


args:
    expr {[$1]}
    |args COMMA expr {$3 :: $1}


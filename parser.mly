
%token      INT STRING BOOL VOID ARRAY STRUCT                         /* literals         */
            PLUS MINUS TIMES DIVIDE MODULO                              /* arithmetic ops   */
            COMPEQUAL COMPNOTEQUAL LESSTHAN GREATERTHAN GTEQT LTEQT     /* comparison ops   */
            UNION INTERSECTION                                          /* set ops          */
            IF THEN ELSE FOR WHILE CONTINUE BREAK SEMICOLON RETURN EOF  /* control flow     */ 
            LPAREN RPAREN LCURLY RCURLY LBRACK RBRACK                   /* parens, brackets */
            ASSIGN INCREMENT DECREMENT                                  /* assignment ops   */
            AND OR NEGATE                                               /* boolean ops      */
            PRINT COMMA                                                 /* misc             */             
%token <int> INTL
%token <string> ID
%token <bool> BOOLL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%left SEMICOLON
%right ASSIGN INCREMENT DECREMENT
%right IF THEN 
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
  | INT     { Int}
  | STRING  { String}
  | BOOL    { Bool   }
  | STRUCT ID { Struct($2) }


make_arrayL:
    {[]}
    |LCURLY args RCURLY {List.rev $2}

declare_statements:
  /* nothing */ { ([], []) }
  | declare_statements vdecl { (($2 :: fst $1), snd $1) }
  | declare_statements fdecl { (fst $1, ($2 :: snd $1)) }

fdecl: 
  | typ ID LPAREN formals_opt RPAREN LCURLY vdecl_list stmt_list RCURLY {
        { 
          typ = $1; 
          fname = $2; 
          formals = List.rev $4;
          locals = List.rev $7; 
          body = List.rev $8 
        } 
    }


formals_opt: 
  /* nothing */ { [] }
  | formal_list { $1 }

formal_list: 
  typ ID { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

vdecl_list: 
  /* nothing */ { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl: 
  typ ID SEMICOLON { ($1, $2) }

sdecl:
    STRUCT ID LCURLY vdecl_list RCURLY SEMICOLON {{struct_name = $2; members = List.rev $4}}

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMICOLON                               { Expr $1               }
  | RETURN expr_opt SEMICOLON                    { Return $2             }
  | LCURLY stmt_list RCURLY                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMICOLON expr SEMICOLON expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }


expr:
    /* Literals                             */
    INTL                      { Int($1)            }   
    | ID                     { Id($1)         }
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
    | ID DECREMENT expr      { Decrement($1, $3) }
    /* Slicing */
    | expr LPAREN expr RPAREN     { $1, $3 }
    /* Boolean Operators  */
    | expr AND expr          { Bool($1, And, $3) }
    | expr OR expr           { Bool($1, Or, $3) }
    | NEGATE expr            {  Unop(Negate, $2)   } 
    /*Struct 177*/
    |STRUCT ID            { NewStruct($2) }
    /* Arrays */
    |typ LBRACK expr RBRACK make_arrayL {ArrayL($1,$3,$5)}
    /* Conditional */
    | IF LPAREN expr RPAREN LCURLY expr RCURLY                         {Conditional($3, $6)}
    | IF LPAREN expr RPAREN LCURLY expr RCURLY ELSE LCURLY expr RCURLY {ConditionalEl($3, $6, $10)};
    /* Loops */
    | WHILE LPAREN expr RPAREN LCURLY expr RCURLY                      {WLoop($3, $6)}
    | FOR LPAREN for_loop_init RPAREN LCURLY expr RCURLY               {FLoop($3, $6)}
    
increment: 
    ID INCREMENT expr      { Increment($1, $3) }

args:
    expr {[$1]}
    |args COMMA expr {$3 :: $1}

for_loop_init:
      vdecl SEMICOLON comparison SEMICOLON increment {FLoop_init($1, $3, $5)}



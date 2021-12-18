type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULUS
  | ASSIGN
  | INCR
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | DECREMENT
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | VOID
  | STRING
  | ARRAY
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | SLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "vowelparse.mly"
open Ast
# 50 "vowelparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* COMMA *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
  268 (* DIVIDE *);
  269 (* MODULUS *);
  270 (* ASSIGN *);
  271 (* INCR *);
  272 (* NOT *);
  273 (* EQ *);
  274 (* NEQ *);
  275 (* LT *);
  276 (* LEQ *);
  277 (* GT *);
  278 (* GEQ *);
  279 (* AND *);
  280 (* OR *);
  281 (* DECREMENT *);
  282 (* RETURN *);
  283 (* IF *);
  284 (* ELSE *);
  285 (* FOR *);
  286 (* WHILE *);
  287 (* INT *);
  288 (* BOOL *);
  289 (* FLOAT *);
  290 (* VOID *);
  291 (* STRING *);
  292 (* ARRAY *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  293 (* LITERAL *);
  294 (* BLIT *);
  295 (* ID *);
  296 (* FLIT *);
  297 (* SLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\005\000\007\000\
\007\000\003\000\008\000\008\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\012\000\012\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\002\000\000\000\
\002\000\003\000\000\000\002\000\002\000\003\000\003\000\005\000\
\007\000\009\000\005\000\000\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\003\000\003\000\004\000\003\000\003\000\004\000\003\000\
\006\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\062\000\000\000\010\000\011\000\012\000\013\000\
\014\000\001\000\003\000\004\000\000\000\015\000\000\000\018\000\
\000\000\000\000\000\000\000\000\008\000\000\000\000\000\016\000\
\000\000\000\000\009\000\017\000\000\000\000\000\000\000\000\000\
\019\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\030\000\033\000\000\000\031\000\032\000\020\000\000\000\
\000\000\000\000\000\000\000\000\048\000\049\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\021\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\053\000\023\000\
\056\000\000\000\022\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\037\000\038\000\
\039\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\052\000\000\000\000\000\
\000\000\027\000\000\000\000\000\000\000\000\000\025\000\000\000\
\000\000\026\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\013\000\019\000\026\000\030\000\
\020\000\047\000\048\000\056\000\087\000\052\000"

let yysindex = "\018\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\235\254\000\000\047\255\000\000\
\111\255\024\255\021\255\045\255\000\000\051\255\111\255\000\000\
\036\255\111\255\000\000\000\000\041\255\052\255\058\255\162\255\
\000\000\000\000\162\255\162\255\162\255\162\255\082\255\086\255\
\093\255\000\000\000\000\008\255\000\000\000\000\000\000\251\255\
\183\000\092\255\005\001\116\255\000\000\000\000\005\001\064\255\
\162\255\162\255\162\255\162\255\162\255\162\255\162\255\162\255\
\000\000\162\255\162\255\162\255\162\255\162\255\162\255\162\255\
\162\255\162\255\162\255\162\255\162\255\162\255\000\000\000\000\
\000\000\162\255\000\000\205\000\106\255\227\000\114\255\101\255\
\245\000\005\001\005\001\005\001\088\255\088\255\000\000\000\000\
\000\000\050\001\050\001\244\255\244\255\244\255\244\255\036\001\
\021\001\005\001\155\255\162\255\155\255\000\000\099\255\097\255\
\028\000\000\000\162\255\155\255\162\255\005\001\000\000\125\255\
\155\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\131\255\000\000\000\000\132\255\000\000\000\000\000\000\000\000\
\000\000\110\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\137\255\000\000\000\000\
\000\000\000\000\000\000\203\255\000\000\000\000\000\000\000\000\
\000\000\000\000\005\255\000\000\000\000\000\000\102\255\000\000\
\000\000\137\255\000\000\138\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\159\255\
\000\000\066\255\166\255\019\000\052\000\076\000\000\000\000\000\
\000\000\164\000\167\000\100\000\108\000\132\000\140\000\063\255\
\103\255\044\255\000\000\000\000\000\000\000\000\227\255\150\255\
\000\000\000\000\000\000\000\000\160\255\072\001\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\127\000\000\000\250\255\000\000\000\000\125\000\
\000\000\156\255\224\255\200\255\000\000\110\000"

let yytablesize = 592
let yytable = "\049\000\
\010\000\085\000\051\000\053\000\054\000\055\000\112\000\060\000\
\114\000\060\000\018\000\060\000\060\000\061\000\014\000\119\000\
\025\000\015\000\001\000\029\000\122\000\062\000\063\000\022\000\
\084\000\055\000\086\000\051\000\089\000\090\000\091\000\092\000\
\064\000\093\000\094\000\095\000\096\000\097\000\098\000\099\000\
\100\000\101\000\102\000\103\000\104\000\105\000\061\000\016\000\
\017\000\106\000\061\000\061\000\023\000\032\000\024\000\033\000\
\034\000\035\000\016\000\014\000\120\000\036\000\021\000\046\000\
\083\000\046\000\050\000\037\000\050\000\046\000\046\000\014\000\
\050\000\050\000\027\000\113\000\014\000\038\000\039\000\031\000\
\040\000\041\000\118\000\057\000\055\000\046\000\046\000\058\000\
\042\000\043\000\044\000\045\000\046\000\032\000\059\000\033\000\
\080\000\035\000\068\000\069\000\070\000\036\000\029\000\047\000\
\029\000\047\000\108\000\037\000\082\000\047\000\047\000\019\000\
\115\000\019\000\019\000\019\000\110\000\038\000\039\000\019\000\
\040\000\041\000\081\000\082\000\116\000\019\000\047\000\121\000\
\042\000\043\000\044\000\045\000\046\000\006\000\007\000\019\000\
\019\000\028\000\019\000\019\000\058\000\005\000\006\000\007\000\
\008\000\009\000\019\000\019\000\019\000\019\000\019\000\024\000\
\028\000\024\000\024\000\024\000\032\000\050\000\033\000\024\000\
\035\000\059\000\028\000\032\000\036\000\024\000\051\000\035\000\
\051\000\088\000\037\000\036\000\051\000\051\000\000\000\024\000\
\024\000\037\000\024\000\024\000\038\000\039\000\000\000\040\000\
\041\000\000\000\024\000\024\000\024\000\024\000\024\000\042\000\
\043\000\044\000\045\000\046\000\000\000\000\000\042\000\043\000\
\044\000\045\000\046\000\034\000\000\000\034\000\000\000\000\000\
\000\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\000\000\000\000\000\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\055\000\000\000\055\000\000\000\000\000\
\000\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
\000\000\000\000\000\000\055\000\055\000\055\000\055\000\055\000\
\055\000\055\000\055\000\065\000\066\000\067\000\068\000\069\000\
\070\000\000\000\000\000\066\000\067\000\068\000\069\000\070\000\
\000\000\000\000\000\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\054\000\000\000\054\000\000\000\000\000\
\000\000\054\000\054\000\000\000\117\000\000\000\000\000\005\000\
\006\000\007\000\008\000\009\000\066\000\067\000\068\000\069\000\
\070\000\000\000\000\000\000\000\071\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\035\000\000\000\035\000\000\000\
\000\000\000\000\035\000\035\000\035\000\035\000\000\000\000\000\
\000\000\000\000\000\000\000\000\035\000\035\000\035\000\035\000\
\035\000\035\000\035\000\035\000\036\000\000\000\036\000\000\000\
\000\000\000\000\036\000\036\000\036\000\036\000\000\000\000\000\
\000\000\000\000\000\000\000\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\042\000\000\000\042\000\000\000\
\000\000\000\000\042\000\042\000\043\000\000\000\043\000\000\000\
\000\000\000\000\043\000\043\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\043\000\043\000\043\000\043\000\
\043\000\043\000\043\000\043\000\044\000\000\000\044\000\000\000\
\000\000\000\000\044\000\044\000\045\000\000\000\045\000\000\000\
\000\000\000\000\045\000\045\000\044\000\044\000\044\000\044\000\
\044\000\044\000\044\000\044\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\040\000\000\000\040\000\041\000\
\000\000\041\000\040\000\040\000\000\000\041\000\041\000\000\000\
\000\000\000\000\000\000\000\000\040\000\040\000\000\000\041\000\
\041\000\079\000\040\000\040\000\000\000\041\000\041\000\066\000\
\067\000\068\000\069\000\070\000\000\000\000\000\000\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\107\000\
\000\000\000\000\000\000\000\000\000\000\066\000\067\000\068\000\
\069\000\070\000\000\000\000\000\000\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\109\000\000\000\000\000\
\000\000\000\000\000\000\066\000\067\000\068\000\069\000\070\000\
\000\000\000\000\000\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\111\000\000\000\066\000\067\000\068\000\
\069\000\070\000\000\000\000\000\000\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\066\000\067\000\068\000\
\069\000\070\000\000\000\000\000\000\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\078\000\066\000\067\000\068\000\
\069\000\070\000\000\000\000\000\000\000\071\000\072\000\073\000\
\074\000\075\000\076\000\077\000\066\000\067\000\068\000\069\000\
\070\000\000\000\000\000\000\000\071\000\072\000\073\000\074\000\
\075\000\076\000\066\000\067\000\068\000\069\000\070\000\000\000\
\000\000\000\000\000\000\000\000\073\000\074\000\075\000\076\000\
\057\000\000\000\057\000\000\000\000\000\000\000\057\000\057\000"

let yycheck = "\032\000\
\000\000\058\000\035\000\036\000\037\000\038\000\107\000\003\001\
\109\000\002\001\017\000\007\001\008\001\006\001\036\001\116\000\
\023\000\039\001\001\000\026\000\121\000\014\001\015\001\003\001\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\025\001\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\077\000\078\000\003\001\001\001\
\002\001\082\000\007\001\008\001\008\001\002\001\004\001\004\001\
\005\001\006\001\001\001\036\001\117\000\010\001\039\001\001\001\
\001\001\003\001\001\001\016\001\003\001\007\001\008\001\036\001\
\007\001\008\001\039\001\108\000\036\001\026\001\027\001\039\001\
\029\001\030\001\115\000\002\001\117\000\023\001\024\001\002\001\
\037\001\038\001\039\001\040\001\041\001\002\001\002\001\004\001\
\005\001\006\001\011\001\012\001\013\001\010\001\001\001\001\001\
\003\001\003\001\001\001\016\001\008\001\007\001\008\001\002\001\
\014\001\004\001\005\001\006\001\003\001\026\001\027\001\010\001\
\029\001\030\001\007\001\008\001\028\001\016\001\024\001\003\001\
\037\001\038\001\039\001\040\001\041\001\003\001\003\001\026\001\
\027\001\001\001\029\001\030\001\003\001\031\001\032\001\033\001\
\034\001\035\001\037\001\038\001\039\001\040\001\041\001\002\001\
\026\000\004\001\005\001\006\001\002\001\033\000\004\001\010\001\
\006\001\003\001\003\001\002\001\010\001\016\001\001\001\006\001\
\003\001\060\000\016\001\010\001\007\001\008\001\255\255\026\001\
\027\001\016\001\029\001\030\001\026\001\027\001\255\255\029\001\
\030\001\255\255\037\001\038\001\039\001\040\001\041\001\037\001\
\038\001\039\001\040\001\041\001\255\255\255\255\037\001\038\001\
\039\001\040\001\041\001\001\001\255\255\003\001\255\255\255\255\
\255\255\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\255\255\255\255\255\255\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\001\001\255\255\003\001\255\255\255\255\
\255\255\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\255\255\255\255\255\255\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\001\001\009\001\010\001\011\001\012\001\
\013\001\255\255\255\255\009\001\010\001\011\001\012\001\013\001\
\255\255\255\255\255\255\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\001\001\255\255\003\001\255\255\255\255\
\255\255\007\001\008\001\255\255\001\001\255\255\255\255\031\001\
\032\001\033\001\034\001\035\001\009\001\010\001\011\001\012\001\
\013\001\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\001\001\255\255\003\001\255\255\
\255\255\255\255\007\001\008\001\009\001\010\001\255\255\255\255\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\001\001\255\255\003\001\255\255\
\255\255\255\255\007\001\008\001\009\001\010\001\255\255\255\255\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\001\001\255\255\003\001\255\255\
\255\255\255\255\007\001\008\001\001\001\255\255\003\001\255\255\
\255\255\255\255\007\001\008\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\001\001\255\255\003\001\255\255\
\255\255\255\255\007\001\008\001\001\001\255\255\003\001\255\255\
\255\255\255\255\007\001\008\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\001\001\255\255\003\001\001\001\
\255\255\003\001\007\001\008\001\255\255\007\001\008\001\255\255\
\255\255\255\255\255\255\255\255\017\001\018\001\255\255\017\001\
\018\001\003\001\023\001\024\001\255\255\023\001\024\001\009\001\
\010\001\011\001\012\001\013\001\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\003\001\
\255\255\255\255\255\255\255\255\255\255\009\001\010\001\011\001\
\012\001\013\001\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\003\001\255\255\255\255\
\255\255\255\255\255\255\009\001\010\001\011\001\012\001\013\001\
\255\255\255\255\255\255\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\007\001\255\255\009\001\010\001\011\001\
\012\001\013\001\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\009\001\010\001\011\001\
\012\001\013\001\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\009\001\010\001\011\001\
\012\001\013\001\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\009\001\010\001\011\001\012\001\
\013\001\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\009\001\010\001\011\001\012\001\013\001\255\255\
\255\255\255\255\255\255\255\255\019\001\020\001\021\001\022\001\
\001\001\255\255\003\001\255\255\255\255\255\255\007\001\008\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACKET\000\
  RBRACKET\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MODULUS\000\
  ASSIGN\000\
  INCR\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  DECREMENT\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  FLOAT\000\
  VOID\000\
  STRING\000\
  ARRAY\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  ID\000\
  FLIT\000\
  SLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 36 "vowelparse.mly"
            ( _1 )
# 389 "vowelparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "vowelparse.mly"
                 ( ([], [])               )
# 395 "vowelparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 40 "vowelparse.mly"
               ( ((_2 :: fst _1), snd _1) )
# 403 "vowelparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 41 "vowelparse.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 411 "vowelparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 45 "vowelparse.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 426 "vowelparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "vowelparse.mly"
                  ( [] )
# 432 "vowelparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 53 "vowelparse.mly"
                  ( _1 )
# 439 "vowelparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "vowelparse.mly"
                             ( [(_1,_2)]     )
# 447 "vowelparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "vowelparse.mly"
                             ( (_3,_4) :: _1 )
# 456 "vowelparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "vowelparse.mly"
           ( Int   )
# 462 "vowelparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "vowelparse.mly"
           ( Bool  )
# 468 "vowelparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "vowelparse.mly"
           ( Float )
# 474 "vowelparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "vowelparse.mly"
           ( Void  )
# 480 "vowelparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "vowelparse.mly"
           ( String )
# 486 "vowelparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 65 "vowelparse.mly"
              ( Arr(_1, 0) )
# 493 "vowelparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "vowelparse.mly"
                     ( [] )
# 499 "vowelparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 69 "vowelparse.mly"
                     ( _2 :: _1 )
# 507 "vowelparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 72 "vowelparse.mly"
                ( (_1, _2) )
# 515 "vowelparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "vowelparse.mly"
                   ( [] )
# 521 "vowelparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 76 "vowelparse.mly"
                   ( _2 :: _1 )
# 529 "vowelparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 79 "vowelparse.mly"
                                            ( Expr _1               )
# 536 "vowelparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 80 "vowelparse.mly"
                                            ( Return _2             )
# 543 "vowelparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 81 "vowelparse.mly"
                                            ( Block(List.rev _2)    )
# 550 "vowelparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "vowelparse.mly"
                                            ( If(_3, _5, Block([])) )
# 558 "vowelparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "vowelparse.mly"
                                            ( If(_3, _5, _7)        )
# 567 "vowelparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 85 "vowelparse.mly"
                                            ( For(_3, _5, _7, _9)   )
# 577 "vowelparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 86 "vowelparse.mly"
                                            ( While(_3, _5)         )
# 585 "vowelparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "vowelparse.mly"
                  ( Noexpr )
# 591 "vowelparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "vowelparse.mly"
                  ( _1 )
# 598 "vowelparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 93 "vowelparse.mly"
                     ( Literal(_1)            )
# 605 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "vowelparse.mly"
                    ( Fliteral(_1)           )
# 612 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "vowelparse.mly"
                     ( STRliteral(_1)         )
# 619 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 96 "vowelparse.mly"
                     ( BoolLit(_1)            )
# 626 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "vowelparse.mly"
                     ( Id(_1)                 )
# 633 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "vowelparse.mly"
                     ( Binop(_1, Add,   _3)   )
# 641 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "vowelparse.mly"
                     ( Binop(_1, Sub,   _3)   )
# 649 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "vowelparse.mly"
                     ( Binop(_1, Mult,  _3)   )
# 657 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "vowelparse.mly"
                     ( Binop(_1, Div,   _3)   )
# 665 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "vowelparse.mly"
                      ( Binop(_1, Mod,   _3)   )
# 673 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "vowelparse.mly"
                     ( Binop(_1, Equal, _3)   )
# 681 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "vowelparse.mly"
                     ( Binop(_1, Neq,   _3)   )
# 689 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "vowelparse.mly"
                     ( Binop(_1, Less,  _3)   )
# 697 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "vowelparse.mly"
                     ( Binop(_1, Leq,   _3)   )
# 705 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "vowelparse.mly"
                     ( Binop(_1, Greater, _3) )
# 713 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "vowelparse.mly"
                     ( Binop(_1, Geq,   _3)   )
# 721 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "vowelparse.mly"
                     ( Binop(_1, And,   _3)   )
# 729 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "vowelparse.mly"
                     ( Binop(_1, Or,    _3)   )
# 737 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "vowelparse.mly"
                         ( Unop(Neg, _2)      )
# 744 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "vowelparse.mly"
                     ( Unop(Not, _2)          )
# 751 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "vowelparse.mly"
                     ( Assign(_1, _3)         )
# 759 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "vowelparse.mly"
                     ( Incr(_1, _3)           )
# 767 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 115 "vowelparse.mly"
                              ( Call(_1, _3)  )
# 775 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 116 "vowelparse.mly"
                       ( _2                   )
# 782 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "vowelparse.mly"
                      ( Decrement(_1, _3)     )
# 790 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 119 "vowelparse.mly"
                              ( ArrayAccess(_1, _3) )
# 798 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'args_list) in
    Obj.repr(
# 120 "vowelparse.mly"
                                ( ArrayLit(_2) )
# 805 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "vowelparse.mly"
                                          ( ArrAssign(_1, _3, _6) )
# 814 "vowelparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "vowelparse.mly"
                  ( [] )
# 820 "vowelparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 128 "vowelparse.mly"
               ( List.rev _1 )
# 827 "vowelparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "vowelparse.mly"
                            ( [_1] )
# 834 "vowelparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "vowelparse.mly"
                         ( _3 :: _1 )
# 842 "vowelparse.ml"
               : 'args_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)

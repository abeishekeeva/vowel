type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
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
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | SLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "microcparse.mly"
open Ast
# 46 "microcparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* ASSIGN *);
  268 (* INCR *);
  269 (* NOT *);
  270 (* EQ *);
  271 (* NEQ *);
  272 (* LT *);
  273 (* LEQ *);
  274 (* GT *);
  275 (* GEQ *);
  276 (* AND *);
  277 (* OR *);
  278 (* DECREMENT *);
  279 (* RETURN *);
  280 (* IF *);
  281 (* ELSE *);
  282 (* FOR *);
  283 (* WHILE *);
  284 (* INT *);
  285 (* BOOL *);
  286 (* FLOAT *);
  287 (* VOID *);
  288 (* STRING *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  289 (* LITERAL *);
  290 (* BLIT *);
  291 (* ID *);
  292 (* FLIT *);
  293 (* SLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\007\000\007\000\
\003\000\008\000\008\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\012\000\012\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\013\000\013\000\014\000\014\000\
\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\000\000\002\000\
\003\000\000\000\002\000\002\000\003\000\003\000\005\000\007\000\
\009\000\005\000\000\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\003\000\
\003\000\004\000\003\000\003\000\000\000\001\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\002\000\000\000\057\000\000\000\010\000\011\000\012\000\013\000\
\014\000\001\000\003\000\004\000\000\000\000\000\017\000\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\015\000\000\000\
\000\000\009\000\016\000\000\000\000\000\000\000\000\000\018\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\032\000\000\000\030\000\031\000\019\000\000\000\000\000\000\000\
\046\000\047\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\020\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\051\000\022\000\021\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\036\000\037\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\050\000\000\000\000\000\000\000\026\000\
\000\000\000\000\000\000\024\000\000\000\000\000\025\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\013\000\018\000\025\000\029\000\
\019\000\045\000\046\000\052\000\080\000\081\000"

let yysindex = "\006\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\235\254\064\255\000\000\207\255\
\241\254\043\255\046\255\000\000\056\255\207\255\000\000\038\255\
\207\255\000\000\000\000\040\255\045\255\083\255\181\255\000\000\
\000\000\181\255\181\255\181\255\095\255\096\255\097\255\000\000\
\000\000\007\255\000\000\000\000\000\000\212\255\134\000\081\255\
\000\000\000\000\187\000\100\255\181\255\181\255\181\255\181\255\
\181\255\181\255\181\255\000\000\181\255\181\255\181\255\181\255\
\181\255\181\255\181\255\181\255\181\255\181\255\181\255\181\255\
\000\000\000\000\000\000\153\000\111\255\172\000\187\000\110\255\
\113\255\187\000\187\000\187\000\078\255\078\255\000\000\000\000\
\229\000\229\000\119\255\119\255\119\255\119\255\216\000\202\000\
\151\255\181\255\151\255\000\000\181\255\105\255\233\255\000\000\
\187\000\151\255\181\255\000\000\117\255\151\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\120\255\
\000\000\000\000\137\255\000\000\000\000\000\000\000\000\000\000\
\098\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\136\255\000\000\000\000\000\000\000\000\
\000\000\189\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\041\255\000\000\000\000\136\255\000\000\138\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\048\255\000\000\
\141\255\010\255\058\255\090\255\254\255\020\000\000\000\000\000\
\042\255\125\000\041\000\062\000\083\000\104\000\089\255\142\255\
\000\000\000\000\000\000\000\000\000\000\134\255\000\000\000\000\
\071\255\000\000\143\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\124\000\000\000\246\255\000\000\000\000\118\000\
\000\000\167\255\225\255\204\255\000\000\000\000"

let yytablesize = 504
let yytable = "\047\000\
\010\000\077\000\049\000\050\000\051\000\017\000\001\000\102\000\
\056\000\104\000\048\000\024\000\048\000\014\000\028\000\048\000\
\108\000\057\000\058\000\020\000\111\000\076\000\051\000\078\000\
\079\000\082\000\083\000\084\000\059\000\085\000\086\000\087\000\
\088\000\089\000\090\000\091\000\092\000\093\000\094\000\095\000\
\096\000\028\000\038\000\028\000\038\000\021\000\031\000\038\000\
\032\000\033\000\055\000\022\000\034\000\055\000\109\000\038\000\
\038\000\035\000\049\000\023\000\049\000\038\000\038\000\049\000\
\015\000\016\000\103\000\036\000\037\000\105\000\038\000\039\000\
\026\000\056\000\030\000\051\000\056\000\040\000\041\000\042\000\
\043\000\044\000\031\000\015\000\032\000\074\000\063\000\064\000\
\034\000\044\000\052\000\044\000\052\000\035\000\044\000\052\000\
\053\000\054\000\055\000\018\000\075\000\018\000\018\000\036\000\
\037\000\018\000\038\000\039\000\044\000\044\000\018\000\098\000\
\100\000\040\000\041\000\042\000\043\000\044\000\101\000\110\000\
\018\000\018\000\006\000\018\000\018\000\061\000\062\000\063\000\
\064\000\106\000\018\000\018\000\018\000\018\000\018\000\023\000\
\027\000\023\000\023\000\007\000\053\000\023\000\045\000\054\000\
\045\000\027\000\023\000\045\000\027\000\048\000\000\000\000\000\
\031\000\000\000\032\000\000\000\023\000\023\000\034\000\023\000\
\023\000\000\000\045\000\035\000\000\000\000\000\023\000\023\000\
\023\000\023\000\023\000\000\000\000\000\036\000\037\000\000\000\
\038\000\039\000\000\000\000\000\000\000\000\000\031\000\040\000\
\041\000\042\000\043\000\044\000\034\000\033\000\000\000\033\000\
\000\000\035\000\033\000\033\000\033\000\033\000\033\000\000\000\
\000\000\000\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\000\000\000\000\060\000\040\000\041\000\042\000\
\043\000\044\000\061\000\062\000\063\000\064\000\000\000\000\000\
\000\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\072\000\107\000\005\000\006\000\007\000\008\000\009\000\061\000\
\062\000\063\000\064\000\000\000\000\000\000\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\072\000\034\000\000\000\
\034\000\000\000\000\000\034\000\034\000\034\000\000\000\000\000\
\000\000\000\000\000\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\000\000\035\000\000\000\035\000\000\000\
\000\000\035\000\035\000\035\000\005\000\006\000\007\000\008\000\
\009\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
\035\000\040\000\000\000\040\000\000\000\000\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\041\000\000\000\
\041\000\000\000\000\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\042\000\000\000\042\000\000\000\000\000\
\042\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\043\000\000\000\043\000\000\000\000\000\043\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\043\000\043\000\043\000\
\043\000\043\000\043\000\043\000\043\000\039\000\000\000\039\000\
\000\000\000\000\039\000\000\000\000\000\000\000\000\000\000\000\
\073\000\000\000\039\000\039\000\061\000\062\000\063\000\064\000\
\039\000\039\000\000\000\065\000\066\000\067\000\068\000\069\000\
\070\000\071\000\072\000\097\000\000\000\000\000\000\000\061\000\
\062\000\063\000\064\000\000\000\000\000\000\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\072\000\099\000\000\000\
\000\000\000\000\061\000\062\000\063\000\064\000\000\000\000\000\
\000\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\072\000\061\000\062\000\063\000\064\000\000\000\000\000\000\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\061\000\062\000\063\000\064\000\000\000\000\000\000\000\065\000\
\066\000\067\000\068\000\069\000\070\000\071\000\061\000\062\000\
\063\000\064\000\000\000\000\000\000\000\065\000\066\000\067\000\
\068\000\069\000\070\000\061\000\062\000\063\000\064\000\000\000\
\000\000\000\000\000\000\000\000\067\000\068\000\069\000\070\000"

let yycheck = "\031\000\
\000\000\054\000\034\000\035\000\036\000\016\000\001\000\097\000\
\002\001\099\000\001\001\022\000\003\001\035\001\025\000\006\001\
\106\000\011\001\012\001\035\001\110\000\053\000\054\000\055\000\
\056\000\057\000\058\000\059\000\022\001\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\072\000\001\001\001\001\003\001\003\001\003\001\002\001\006\001\
\004\001\005\001\003\001\006\001\008\001\006\001\107\000\014\001\
\015\001\013\001\001\001\004\001\003\001\020\001\021\001\006\001\
\001\001\002\001\098\000\023\001\024\001\101\000\026\001\027\001\
\035\001\003\001\035\001\107\000\006\001\033\001\034\001\035\001\
\036\001\037\001\002\001\001\001\004\001\005\001\009\001\010\001\
\008\001\001\001\001\001\003\001\003\001\013\001\006\001\006\001\
\002\001\002\001\002\001\002\001\001\001\004\001\005\001\023\001\
\024\001\008\001\026\001\027\001\020\001\021\001\013\001\001\001\
\003\001\033\001\034\001\035\001\036\001\037\001\006\001\003\001\
\023\001\024\001\003\001\026\001\027\001\007\001\008\001\009\001\
\010\001\025\001\033\001\034\001\035\001\036\001\037\001\002\001\
\001\001\004\001\005\001\003\001\003\001\008\001\001\001\003\001\
\003\001\003\001\013\001\006\001\025\000\032\000\255\255\255\255\
\002\001\255\255\004\001\255\255\023\001\024\001\008\001\026\001\
\027\001\255\255\021\001\013\001\255\255\255\255\033\001\034\001\
\035\001\036\001\037\001\255\255\255\255\023\001\024\001\255\255\
\026\001\027\001\255\255\255\255\255\255\255\255\002\001\033\001\
\034\001\035\001\036\001\037\001\008\001\001\001\255\255\003\001\
\255\255\013\001\006\001\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\255\255\255\255\001\001\033\001\034\001\035\001\
\036\001\037\001\007\001\008\001\009\001\010\001\255\255\255\255\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\001\001\028\001\029\001\030\001\031\001\032\001\007\001\
\008\001\009\001\010\001\255\255\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\001\001\255\255\
\003\001\255\255\255\255\006\001\007\001\008\001\255\255\255\255\
\255\255\255\255\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\255\255\001\001\255\255\003\001\255\255\
\255\255\006\001\007\001\008\001\028\001\029\001\030\001\031\001\
\032\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\001\001\255\255\003\001\255\255\255\255\006\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\001\001\255\255\003\001\255\255\255\255\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\001\001\255\255\003\001\255\255\255\255\006\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\001\001\255\255\003\001\
\255\255\255\255\006\001\255\255\255\255\255\255\255\255\255\255\
\003\001\255\255\014\001\015\001\007\001\008\001\009\001\010\001\
\020\001\021\001\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\003\001\255\255\255\255\255\255\007\001\
\008\001\009\001\010\001\255\255\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\003\001\255\255\
\255\255\255\255\007\001\008\001\009\001\010\001\255\255\255\255\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\007\001\008\001\009\001\010\001\255\255\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\007\001\008\001\
\009\001\010\001\255\255\255\255\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\255\255\255\255\016\001\017\001\018\001\019\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
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
# 35 "microcparse.mly"
            ( _1 )
# 349 "microcparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "microcparse.mly"
                 ( ([], [])               )
# 355 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 39 "microcparse.mly"
               ( ((_2 :: fst _1), snd _1) )
# 363 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 40 "microcparse.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 371 "microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 44 "microcparse.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 386 "microcparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "microcparse.mly"
                  ( [] )
# 392 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 52 "microcparse.mly"
                  ( _1 )
# 399 "microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "microcparse.mly"
                             ( [(_1,_2)]     )
# 407 "microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "microcparse.mly"
                             ( (_3,_4) :: _1 )
# 416 "microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "microcparse.mly"
           ( Int   )
# 422 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "microcparse.mly"
           ( Bool  )
# 428 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "microcparse.mly"
           ( Float )
# 434 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "microcparse.mly"
           ( Void  )
# 440 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "microcparse.mly"
           ( String )
# 446 "microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "microcparse.mly"
                     ( [] )
# 452 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 67 "microcparse.mly"
                     ( _2 :: _1 )
# 460 "microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 70 "microcparse.mly"
               ( (_1, _2) )
# 468 "microcparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "microcparse.mly"
                   ( [] )
# 474 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 74 "microcparse.mly"
                   ( _2 :: _1 )
# 482 "microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "microcparse.mly"
                                            ( Expr _1               )
# 489 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 78 "microcparse.mly"
                                            ( Return _2             )
# 496 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 79 "microcparse.mly"
                                            ( Block(List.rev _2)    )
# 503 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "microcparse.mly"
                                            ( If(_3, _5, Block([])) )
# 511 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 81 "microcparse.mly"
                                            ( If(_3, _5, _7)        )
# 520 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "microcparse.mly"
                                            ( For(_3, _5, _7, _9)   )
# 530 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 84 "microcparse.mly"
                                            ( While(_3, _5)         )
# 538 "microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "microcparse.mly"
                  ( Noexpr )
# 544 "microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "microcparse.mly"
                  ( _1 )
# 551 "microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "microcparse.mly"
                     ( Literal(_1)            )
# 558 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "microcparse.mly"
                    ( Fliteral(_1)           )
# 565 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "microcparse.mly"
                     ( STRliteral(_1)         )
# 572 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 94 "microcparse.mly"
                     ( BoolLit(_1)            )
# 579 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "microcparse.mly"
                     ( Id(_1)                 )
# 586 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "microcparse.mly"
                     ( Binop(_1, Add,   _3)   )
# 594 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "microcparse.mly"
                     ( Binop(_1, Sub,   _3)   )
# 602 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "microcparse.mly"
                     ( Binop(_1, Mult,  _3)   )
# 610 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "microcparse.mly"
                     ( Binop(_1, Div,   _3)   )
# 618 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "microcparse.mly"
                     ( Binop(_1, Equal, _3)   )
# 626 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "microcparse.mly"
                     ( Binop(_1, Neq,   _3)   )
# 634 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "microcparse.mly"
                     ( Binop(_1, Less,  _3)   )
# 642 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "microcparse.mly"
                     ( Binop(_1, Leq,   _3)   )
# 650 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "microcparse.mly"
                     ( Binop(_1, Greater, _3) )
# 658 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "microcparse.mly"
                     ( Binop(_1, Geq,   _3)   )
# 666 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "microcparse.mly"
                     ( Binop(_1, And,   _3)   )
# 674 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "microcparse.mly"
                     ( Binop(_1, Or,    _3)   )
# 682 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "microcparse.mly"
                         ( Unop(Neg, _2)      )
# 689 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "microcparse.mly"
                     ( Unop(Not, _2)          )
# 696 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "microcparse.mly"
                     ( Assign(_1, _3)         )
# 704 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "microcparse.mly"
                     ( Incr(_1, _3)           )
# 712 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 112 "microcparse.mly"
                              ( Call(_1, _3)  )
# 720 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 113 "microcparse.mly"
                       ( _2                   )
# 727 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "microcparse.mly"
                      ( Decrement(_1, _3) )
# 735 "microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "microcparse.mly"
                  ( [] )
# 741 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 118 "microcparse.mly"
               ( List.rev _1 )
# 748 "microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "microcparse.mly"
                            ( [_1] )
# 755 "microcparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "microcparse.mly"
                         ( _3 :: _1 )
# 763 "microcparse.ml"
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

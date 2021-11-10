{ open Parser 

  let unescape s = Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}

  
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let string = '"' (ascii | escape)* '"'


rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| "/*"       { comment lexbuf }
| "bool"     { BOOL }
| "int"      { INT }
| "true"     { BOOL(true)}
| "false"    { BOOL(false)}
| "string"   { STRING }
| "int"      { INT }
| "array"    { ARRAY }
| "void"     { VOID }
| "struct"   { STRUCT }
| '+'        { PLUS }
| '-'        { MINUS }
| '*'        { TIMES }
| '/'        { DIVIDE }
| '%'        { MODULO }
| "=="       { COMPEQUAL }
| "!="       { COMPNOTEQUAL }
| '>'        { GREATERTHAN }
| '<'        { LESSTHAN }
| ">="       { GTEQT }
| "<="       { LTEQT }
| '|'        { UNION} 
| '&'        { INTERSECTION }
| "if"       { IF }
| "then"     { THEN }
| "else"     { ELSE }
| "for"      { FOR }
| "while"    { WHILE }
| "continue" { CONTINUE } 
| "break"    { BREAK }
| ';'        { SEMICOLON }
| "return"   { RETURN }
| ['0'-'9']+ as lit        { INTL(int_of_string lit)  }
| ['a'-'z']+ as variable   { ID(variable)             }
| string     as str_val    { STRING(unescape str_val) }
| '='        { ASSIGN }
| "+="       { INCREMENT }
| "-="       { DECREMENT }
| "and"      { AND }
| "or"       { OR }
| '!'        { NEGATE}
| '('        { LPAREN }
| ')'        { RPAREN }
| '}'        { LCURLY }
| '{'        { RCURLY }
| '['        { LBRACK }
| ']'        { RBRACK }
| "print"    { PRINT }
| ','        { COMMA }
| eof        { EOF }
| _ as ch { raise (Failure("illegal character " ^ Char.escaped ch)) }


and comment = parse
"*/" { token lexbuf }
| _  { comment lexbuf }

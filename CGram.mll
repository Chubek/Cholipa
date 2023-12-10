{
open Printf
open Parser

let count () =
  let rec count_chars str col =
    match str with
    | '\n' :: _ -> col := 0
    | '\t' :: _ -> col := !col + (8 - (!col mod 8))
    | _ :: tail -> col := !col + 1; count_chars tail col
    | [] -> ()
  in
  count_chars (Lexing.lexeme lexbuf) (ref 0);


let comment () =
  let rec consume_comment () =
    match input_char lexbuf with
    | '*' -> if input_char lexbuf = '/' then ()
             else consume_comment ()
    | _ -> consume_comment ()
  in
  consume_comment ()

}

rule main = parse
  | [' ' '\t' '\n' '\r'] { count (); main lexbuf }
  | "/*"                  { comment (); main lexbuf }
  | "//"[^'\n']*          { (* Consume //-comment *) main lexbuf }
  | "auto"                { count (); AUTO }
  | "_Bool"               { count (); BOOL }
  | "break"               { count (); BREAK }
  | "case"                { count (); CASE }
  | "char"                { count (); CHAR }
  | "_Complex"            { count (); COMPLEX }
  | "const"               { count (); CONST }
  | "continue"            { count (); CONTINUE }
  | "default"             { count (); DEFAULT }
  | "do"                  { count (); DO }
  | "double"              { count (); DOUBLE }
  | "else"                { count (); ELSE }
  | "enum"                { count (); ENUM }
  | "extern"              { count (); EXTERN }
  | "float"               { count (); FLOAT }
  | "for"                 { count (); FOR }
  | "goto"                { count (); GOTO }
  | "if"                  { count (); IF }
  | "_Imaginary"          { count (); IMAGINARY }
  | "inline"              { count (); INLINE }
  | "int"                 { count (); INT }
  | "long"                { count (); LONG }
  | "register"            { count (); REGISTER }
  | "restrict"            { count (); RESTRICT }
  | "return"              { count (); RETURN }
  | "short"               { count (); SHORT }
  | "signed"              { count (); SIGNED }
  | "sizeof"              { count (); SIZEOF }
  | "static"              { count (); STATIC }
  | "struct"              { count (); STRUCT }
  | "switch"              { count (); SWITCH }
  | "typedef"             { count (); TYPEDEF }
  | "union"               { count (); UNION }
  | "unsigned"            { count (); UNSIGNED }
  | "void"                { count (); VOID }
  | "volatile"            { count (); VOLATILE }
  | "while"               { count (); WHILE }
  | {L}({L}|{D})*         { count (); check_type () }
  | "0x"{H}+{IS}?        { count (); CONSTANT }
  | "0"[0-7]*{IS}?       { count (); CONSTANT }
  | [1-9]{D}*{IS}?       { count (); CONSTANT }
  | L?'(\\.|[^\\'\n])+    { count (); CONSTANT }
  | {D}+{E}{FS}?         { count (); CONSTANT }
  | {D}*"."{D}+{E}?{FS}? { count (); CONSTANT }
  | {D}+"."{D}*{E}?{FS}? { count (); CONSTANT }
  | "0x"{H}+{P}{FS}?     { count (); CONSTANT }
  | "0x"{H}*"."{H}+{P}?{FS}? { count (); CONSTANT }
  | "0x"{H}+"."{H}*{P}?{FS}? { count (); CONSTANT }
  | L?\"(\\.|[^\\\"\n])+" { count (); STRING_LITERAL }
  | "..."                { count (); ELLIPSIS }
  | ">>="                { count (); RIGHT_ASSIGN }
  | "<<="                { count (); LEFT_ASSIGN }
  | "+="                 { count (); ADD_ASSIGN }
  | "-="                 { count (); SUB_ASSIGN }
  | "*="                 { count (); MUL_ASSIGN }
  | "/="                 { count (); DIV_ASSIGN }
  | "%="                 { count (); MOD_ASSIGN }
  | "&="                 { count (); AND_ASSIGN }
  | "^="                 { count (); XOR_ASSIGN }
  | "|="                 { count (); OR_ASSIGN }
  | ">>"                 { count (); RIGHT_OP }
  | "<<"                 { count (); LEFT_OP }
  | "++"                 { count (); INC_OP }
  | "--"                 { count (); DEC_OP }
  | "->"                 { count (); PTR_OP }
  | "&&"                 { count (); AND_OP }
  | "||"                { count (); OR_OP }
  | "<="                 { count (); LE_OP }
  | ">="                 { count (); GE_OP }
  | "=="                 { count (); EQ_OP }
  | "!="                 { count (); NE_OP }
  | ";"                  { count (); ';' }
  | ("{"|"<%")           { count (); '{' }
  | ("}"|"%>")           { count (); '}' }
  | ","                  { count (); ',' }
  | ":"                  { count (); ':' }
  | "="                  { count (); '=' }
  | "("                  { count (); '(' }
  | ")"                  { count (); ')' }
  | ("["|"<:")           { count (); '[' }
  | ("]"|":>")           { count (); ']' }
  | "."                  { count (); '.' }
  | "&"                  { count (); '&' }
  | "!"                  { count (); '!' }
  | "~"                  { count (); '~' }
  | "-"                  { count (); '-' }
  | "+"                  { count (); '+' }
  | "*"                  { count (); '*' }
  | "/"                  { count (); '/' }
  | "%"                  { count (); '%' }
  | "<"                  { count (); '<' }
  | ">"                  { count (); '>' }
  | "^"                  { count (); '^' }
  | "|"                  { count (); '|' }
  | "?"                  { count (); '?' }
  | [ \t\v\n\f]          { count (); }
  | .                    { (* Unmatched characters *) count (); }
  | eof                  { count (); }
 



{
        open Lexing
        open Ast
        open Parser
                open Char

        exception LexingError of string
        let newline lexbuf =
                let pos = lexbuf.lex_curr_p in
                lexbuf.lex_curr_p <- {
                        pos with pos_lnum = pos.pos_lnum + 1;
                        pos_bol = pos.pos_cnum
                }

        let string_of_token token =
        match token with
        | LAMBDA -> "LAMBDA "
        | CHAN -> "CHAN "
        | ARROW -> "ARROW "
        | DOT -> "DOT "
        | COMMA -> "COMMA "
        | LPARENS -> "LPARENS "
        | RPARENS -> "RPARENS "
        | PARA -> "PARA "
        | LSB -> "LSB "
        | RSB -> "RSB "
        | GT -> "GT "
        | LT -> "LT "
        | IDENT id -> "IDENT(" ^ id ^ ") "
        | EOF -> "EOF "
        | TYPE -> "TYPE "
        | COLON -> "COLON "
        | CIRCUMDEPTH d -> "CIRCUMDEPTH(" ^ (string_of_int d) ^ ") "
}

let lowercaseLetter = [ 'a' - 'z' ]
let uppercaseLetter = [ 'A' - 'Z' ]
let letter =  lowercaseLetter | uppercaseLetter
let digit = [ '0' - '9' ]
let ident = (letter | digit) ( letter | '_' | '\'' | digit )* 
let integer = digit+
let car = digit | letter | ' ' | '!' | '#' | '$' | '%' | '&' | ''' | '('
| ')' | '*' | '+' | '-' | ',' | '.' | '/' | ':' | ';' | '<' | '=' | '>'
| '?' | '@' | '[' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~' 
| "\\\\" | "\\\"" | "\\n" | "\\t"
let space = [' ' '\t']	

rule token = parse
| '\n'          { newline lexbuf ; token lexbuf }
| "\\"          { LAMBDA }
| "#"           { CHAN }
| "->"          { ARROW }
| "."           { DOT }
| ","           { COMMA }
| "("           { LPARENS }
| ")"           { RPARENS }
| "||"          { PARA }
| "["           { LSB }
| "]"           { RSB }
| ">"           { GT }
| "<"           { LT }
| "type"        { TYPE }
| ':'           { COLON }
| '^' (integer as i) { 
        try (CIRCUMDEPTH (int_of_string i))
        with
        | _ -> raise (LexingError ("Can not get int of string: "^ i))
}
| (ident as id) { IDENT id }
| space+        { token lexbuf }
| eof           { EOF }


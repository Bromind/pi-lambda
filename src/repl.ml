open Config
open Ast
open Lexing
open Format
open Interp
open Typer

let error_loc pos =
  let line = pos.pos_lnum in
  let col = pos.pos_cnum-pos.pos_bol + 1 in
  eprintf "On line %d, characters %d-%d:\n" line (col-1) col
 
let invite = function _ -> 
        (
                output_string stdout "\n> ";
                flush stdout
        )
let read_input = 
        function _ -> 
                (
                        let line = Pervasives.read_line () in
                        line
                )

let eval s = 
        let buf = Lexing.from_string s in
        let p = Parser.file Lexer.token buf in 
        let tast = type_pi_lambda_expr p in
        let red_p = reduce p in
        let t_red_ast = type_pi_lambda_expr red_p in
        t_red_ast
        
let print tast: unit =
        output_string stdout ("\n" ^ (string_of_tast tast) ^ "\n");
        flush stdout

let rec loop (f: unit -> unit) = 
        f (); loop f

let rep: unit -> unit = 
        function _ ->
        (
                try 
                        invite ();
                        let s = read_input () in
                        let ast = eval s in
                        print ast
                with
                | Parser.Error ->
                        eprintf "Syntax error\n@?"
                | Interp.ApplicationError(loc, t1, t2) ->
                        print_string ("Can not apply term\n\n" ^ (string_of_ast t2) ^"\n to the term\n\n"^(string_of_ast t1))
        )

let () = 
        output_string stdout ("Welcome to πλ-t REPL version " ^ version ^ " - " ^ version_name ^"\n");
        flush stdout;
        try 
                loop rep ;
        with
        | End_of_file -> exit 1;

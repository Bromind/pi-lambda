open Arg
open Pi_lambda_types
open Config
open Ast
open Lexing
open Format
open Interp
open Typer

let filename = ref "main.pil"
let typing_only = ref false

let error_loc pos =
  let line = pos.pos_lnum in
  let col = pos.pos_cnum-pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !filename line (col-1) col

let string_of_loc (bpos, epos) = 
        "File \"" ^ !filename
        ^ "\", at line " ^ (string_of_int bpos.pos_lnum)
        ^ "-" ^ (string_of_int epos.pos_lnum)
        ^ ", character " ^ (string_of_int (bpos.pos_cnum - 1))
        ^ "-" ^ (string_of_int (epos.pos_cnum - 1)) ^ ": "
 
let print_version () = print_string ("Version " ^ version ^ " - " ^ version_name ^ "\n"); exit 0

let unknown_arg s = output_string stderr ("Unknown argument: "^s); exit 1

let usage_msg = "Usage: ./inter [option]"

let args = 
        [
                ("--version", Unit(print_version), "\tPrint version number and exit.");
                ("--input", String(function s -> filename := s), "\tSpecify the input file.");
                ("--type-only", Unit(fun _ -> typing_only := true), "\tStop the compilation process after the typing phase.")
        ]


let () = 
        Arg.parse args unknown_arg usage_msg;
        let f =
                try open_in !filename 
                with 
                | Sys_error s -> output_string stderr (s^"\n"); exit 1
        in
        let buf = Lexing.from_channel f in
        try
        let p = Parser.file Lexer.token buf in 
        close_in f;
        let typed_p = type_pi_lambda_expr p in
        if not !typing_only
        then
                let reduced = type_pi_lambda_expr (reduce p) in
                let term_string = term_string_of_tast reduced in
                output_string stdout (term_string^"\n\t: "^(print_type reduced.typ)^"\n")
        else
                output_string stdout (print_type (type_val typed_p.typ))
        with
        | Parser.Error ->
                error_loc (Lexing.lexeme_start_p buf);
                eprintf "Syntax error\n@?"; exit 1
        | Typer.UnificationError (t1, t2, loc) ->
                        let msg =
                                begin match loc with
                                | Some loc -> string_of_loc loc
                                | None -> "At unknown position, "
                                end ^
                                "can not unify the types "
                                ^ (print_type (type_val t1))
                                ^ " and "
                                ^ (print_type (type_val t2))
                                ^ "\n"
                        in
                        prerr_endline msg; exit 2
        | Typer.ChannelLeakError (name_chan, depth_chan, name_leak, depth_leak, loc) -> 
                        let msg = 
                                begin match loc with
                                | Some loc -> string_of_loc loc
                                | None -> "At unknown position, "
                                end ^
                                begin match name_chan with
                                | Some n -> "channel " ^ n
                                | None -> "unknown channel"
                                end
                                ^ " at depth " ^ (string_of_int depth_chan) ^ " leaks " ^
                                begin match name_leak with
                                | Some n -> "the identifier " ^ n
                                | None -> "an unknown identifer"
                                end
                                ^ " at depth " ^ (string_of_int depth_leak)
                        in
                        prerr_endline msg; exit 3
        | Interp.ApplicationError(loc, t1, t2) ->
                        print_string ("Can not apply term\n\n" ^ (string_of_ast t2) ^"\n to the term\n\n"^(string_of_ast t1)); exit 4;
        | Ast.PatternMatchingNotImplemented -> print_string "Pattern matching is not yet implemented. Please keep waiting, our best technicians are working on the problem right now.\n"; exit 5;
        | _ -> exit 5;

open Constructor
open Identifier

type loc = Lexing.position * Lexing.position

type expr = {
        exp: expr_tree;
        loc: loc
}
and expr_tree =
| E_lambda of ident * expr
| E_ident of ident
| E_app of expr * expr
| E_para of expr list
| E_match of expr * (expr * expr) list
| E_chan of ident * expr
| E_send of ident * expr * expr
| E_deliver of ident * ident * expr
| E_type of ident * constructor list * expr

let rec depth_shift (depth: bool list) = 
match depth with
| [] -> "+- "
| true::tl -> "|  " ^ (depth_shift tl)
| false::tl -> "   " ^ (depth_shift tl)

exception PatternMatchingNotImplemented
let rec string_of_ast_aux (depth: bool list ) expr = 
        let prefix = depth_shift depth in
        let string_of_sub_ast = string_of_ast_aux (depth @ [true] ) in 
        let string_of_sub_ast_last = string_of_ast_aux (depth @ [false]) in
        let sub_tree_string = 
                match expr.exp with
                | E_lambda (i, e) -> 
                                "lambda\n" 
                                ^ prefix ^ i ^ "\n" 
                                ^ prefix ^ string_of_sub_ast_last e 
                | E_ident i ->
                                "ident\n"
                                ^ prefix ^ i ^ "\n"
                | E_app (e1, e2) ->
                                "application\n"
                                ^ prefix ^ string_of_sub_ast e1 
                                ^ prefix ^ string_of_sub_ast_last e2 
                | E_para el ->
                                let rec print_list l = 
                                match l with
                                | e :: [] -> prefix ^ string_of_sub_ast_last e
                                | e :: lt -> prefix ^ string_of_sub_ast e ^ (print_list lt)
                                | [] -> ""
                                in
                                "parallel\n"
                                ^ print_list el
                | E_chan (i, e) -> 
                                "new channel\n" 
                                ^ prefix ^ i ^ "\n" 
                                ^ prefix ^ string_of_sub_ast_last e 
                | E_send (i, e1, e2) ->
                                "send\n"
                                ^ prefix ^ i ^ "\n"
                                ^ prefix ^ string_of_sub_ast e1
                                ^ prefix ^ string_of_sub_ast_last e2
                | E_deliver (c, v, e) ->
                                "deliver\n"
                                ^ prefix ^ c ^ "\n"
                                ^ prefix ^ v ^ "\n"
                                ^ prefix ^ string_of_sub_ast_last e
                | E_type (type_name, constructors, prog) -> 
                                let rec print_list l = 
                                match l with
                                | c :: lt -> prefix ^ string_of_constructor c ^ "\n" ^ (print_list lt)
                                | [] -> ""
                                in
                                "type " ^ type_name ^ "\n"
                                ^ print_list constructors
                                ^ prefix ^ string_of_sub_ast_last prog
                | E_match _ -> raise PatternMatchingNotImplemented
        in
        sub_tree_string  

let string_of_ast expr = string_of_ast_aux [] expr

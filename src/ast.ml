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

(* Returns a list containing the free variables of ast *)
exception PatternMatchingNotImplemented
let rec free_names ast: ident list =
let filter var = fun n ->
        var <> n
in
match ast.exp with
| E_ident v -> [v]
| E_send (chan, msg, cont) ->
                chan :: (free_names msg)@(free_names cont)
| E_deliver (chan, var, cont) ->
                let cont_filtered = List.filter (filter var) (free_names cont) in
                chan :: cont_filtered
| E_chan (i, t)
| E_type (i, _, t)
| E_lambda (i, t) ->
                List.filter (filter i) (free_names t)
| E_app (t1, t2) ->
                (free_names t1) @ (free_names t2)
| E_para tl ->
                let para_free_names = List.map free_names tl in
                List.flatten para_free_names
| E_match _ -> raise PatternMatchingNotImplemented

let rec depth_shift (depth: bool list) = 
match depth with
| [] -> "+- "
| true::tl -> "|  " ^ (depth_shift tl)
| false::tl -> "   " ^ (depth_shift tl)

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

(** Display a term correspondign to the given expr. Contrary to `string_of_ast`, the string returned here is a valid term (i.e. should be parsable) *)
let rec term_of_ast expr =
        match expr.exp with
        | E_lambda (i, e) ->
                        "\\" ^ i ^ "->" ^ term_of_ast e
        | E_ident (i) -> i
        | E_app (e1, e2) -> "(" ^ term_of_ast e1 ^ ") (" ^ term_of_ast e2 ^ ")"
        | E_para el ->
                        let rec print_list l =
                        match l with
                        | [] -> ""
                        | e :: [] -> term_of_ast e
                        | e :: lt -> term_of_ast e ^ " || " ^ print_list lt
                        in
                        "[ " ^ print_list el ^ " ]"
        | E_chan (i, e) ->
                        "#" ^ i ^ ". " ^ term_of_ast e
        | E_send (i, e1, e2) ->
                        i ^ "[" ^ term_of_ast e1 ^ "]." ^ term_of_ast e2
        | E_deliver (c, v, e) ->
                        c ^ ", " ^ v ^ " > " ^ term_of_ast e
        | E_type (type_name, constructors, e) ->
                        "type " ^ type_name ^ string_of_constructor_list constructors ^ ". \n" ^ term_of_ast e
        | E_match _ -> raise PatternMatchingNotImplemented

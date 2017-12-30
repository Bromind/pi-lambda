open Ast

exception TypeError of string
exception NotImplemented of string

let fresh_var_id: unit -> int = 
        let id = ref 0 in
        fun () -> incr id; !id


let fresh_chan_id: unit -> int = 
        let id = ref 0 in
        fun () -> incr id; !id

type pi_lambda_type =
| Tvar of tvar
| Tchan of chan_id * pi_lambda_type
| Tarrow of pi_lambda_type * pi_lambda_type
| Tdot of pi_lambda_type * pi_lambda_type
| Tcross of pi_lambda_type list
| Tsend_chan of pi_lambda_type * pi_lambda_type
| Tdeliver_chan of pi_lambda_type * pi_lambda_type
and chan_id = 
        {
                chan_id: int;
                mutable chan_depth: int
        }

and tvar =
        {
                var_id: int;
                mutable var_depth: int;
                mutable var_type: pi_lambda_type option
        }

let rec type_val (t: pi_lambda_type): pi_lambda_type =
match t with
| Tvar {var_type = Some t} -> type_val t
| Tvar {var_type = None} -> t
| Tchan (id, t) -> Tchan (id, type_val t)
| Tarrow (t1, t2) -> Tarrow (type_val t1, type_val t2)
| Tdot (t1, t2) -> Tdot (type_val t1, type_val t2)
| Tcross tl -> Tcross (List.map type_val tl)
| Tsend_chan (t1, t2) -> Tsend_chan (type_val t1, type_val t2)
| Tdeliver_chan (t1, t2) -> Tdeliver_chan (type_val t1, type_val t2)


let is_complexe_type t = 
match t with
| Tvar _ | Tchan _ | Tcross _-> false
| _ -> true

let rec print_type t = 
        let print_sub_type t = 
                if is_complexe_type t
                then "("^(print_type t)^")"
                else print_type t
        in
match t with
| Tvar {var_id = i; var_depth = d; var_type = Some t} -> 
                "α_"^(string_of_int i)^"^"^(string_of_int d)^" = "^(print_sub_type t)
| Tvar {var_id = i; var_depth = d; var_type = None} ->
                "α_"^(string_of_int i)^"^"^(string_of_int d)
| Tchan (i, t) -> "<"^(string_of_int i.chan_id)^"^"^(string_of_int i.chan_depth)^", "^(print_type (type_val t))^">"
| Tarrow (arg, body) -> (print_sub_type arg) ^ " → " ^ (print_sub_type body)
| Tdot (c, body) -> (print_sub_type c) ^ "." ^ (print_sub_type body)
| Tsend_chan (chan, cont) -> "send on " ^ (print_sub_type (type_val chan)) ^ " then " ^ (print_sub_type (type_val cont))  
| Tdeliver_chan (chan, cont) -> "deliver from " ^ (print_sub_type (type_val chan)) ^ " in " ^ (print_sub_type (type_val cont))
| Tcross tl -> 
        let rec print_tl tl = 
                match tl with
                | [t] -> print_type t
                | t::tl -> (print_sub_type t)^" × "^(print_tl tl)
                | [] -> "[]"
        in
        "["^(print_tl tl)^"]"

let channel_type c =
match type_val c with
| Tchan (_, t) -> t
| _ -> raise (TypeError "Try to retrieve the channel type of a non-channel type")

type typed_expr = {
        texpr: typed_expr_tree;
        loc: loc;
        typ: pi_lambda_type
}
and typed_expr_tree = 
| T_lambda of ident * typed_expr
| T_ident of ident
| T_app of typed_expr * typed_expr
| T_para of typed_expr list
| T_chan of ident * typed_expr
| T_send of ident * typed_expr * typed_expr
| T_deliver of ident * ident * typed_expr


type environment = (ident * pi_lambda_type) list

(* Adds ident `s` bound to type `t` in environment `env` *)
let add s t env = 
        (s, t)::env

exception VarNotFound of string
(* Finds the type of ident `s` in environment `env` *)
let rec find s env =
match env with
| (s_hd, t)::tl -> if s = s_hd then type_val t else find s tl
| [] -> raise (VarNotFound ("Variable not found: " ^ s))

(* Returns true iff variable v occurs in type t*)
let rec occur v t = 
match t with 
| Tvar w -> 
                begin match w.var_type with
                | None -> v.var_id = w.var_id
                | Some subt -> v.var_id = w.var_id || occur v subt
                end
| Tarrow (t1, t2) 
| Tdot (t1, t2)
| Tsend_chan (t1, t2)
| Tdeliver_chan (t1, t2) -> occur v t1 || occur v t2
| Tchan (_, t) -> occur v t
| Tcross tl -> List.fold_left (||) false (List.map (occur v) tl)


exception UnionError of string
exception UnificationError of string
let union (t1: pi_lambda_type) (t2: pi_lambda_type) = 
match type_val t1, type_val t2 with
| Tvar v1, Tvar v2 when v1.var_id = v2.var_id -> ()
| Tvar v1, _ -> 
                if occur v1 t2 
                then 
                        raise (UnificationError ("Types "^(print_type (type_val t1))^" and "^(print_type (type_val t2))^"can not be unified."))
                else v1.var_type <- Some t2
| _, Tvar v2 -> 
                if occur v2 t1 
                then 
                        raise (UnificationError ("Types "^(print_type (type_val t1))^" and "^(print_type (type_val t2))^"can not be unified."))
                else v2.var_type <- Some t1
| _ -> ()

let rec unify t1 t2: unit = 
match type_val t1, type_val t2 with
| Tarrow (targ1, tbody1), Tarrow (targ2, tbody2) -> 
                (unify targ1 targ2); (unify tbody1 tbody2)
| Tsend_chan(ch1, c1), Tsend_chan(ch2, c2) -> 
                (unify_chan_inner (channel_type ch1) (channel_type ch2)); (unify c1 c2)
| Tdeliver_chan(ch1, c1), Tdeliver_chan(ch2, c2) -> 
                (unify_chan_outer (channel_type ch1) (channel_type ch2)); (unify c1 c2)
| Tchan _, Tchan _ -> raise (UnificationError "Can not unify two `Tchan`s, inner or outer unification must be specified")
| Tvar _, _
| _, Tvar _ -> union t1 t2
| _ -> raise (NotImplemented "unification")
 
and unify_chan_inner t1 t2: unit =
match type_val t1, type_val t2 with
| Tchan (id1, c1), Tchan (id2, c2) -> 
                let inner_chan = Pervasives.max id1.chan_depth id2.chan_depth in
                id1.chan_depth <- inner_chan;
                id2.chan_depth <- inner_chan;
                unify c1 c2
| _ -> unify t1 t2

and unify_chan_outer t1 t2: unit =
match type_val t1, type_val t2 with
| Tchan (id1, c1), Tchan (id2, c2) -> 
                let outer_chan = Pervasives.min id1.chan_depth id2.chan_depth in
                id1.chan_depth <- outer_chan;
                id2.chan_depth <- outer_chan;
                unify c1 c2
| _ -> unify t1 t2

let rec type_pi_lambda_expr_aux (env: environment) (depth: int) (expr: expr): typed_expr =
match expr.exp with
| E_lambda (var, body) ->
                (* We compute body's type, knowing that var has type α *)
                let new_env = add var (Tvar {
                        var_id = fresh_var_id ();
                        var_depth = depth;
                        var_type = None}
                        ) env in
                let typed_body = type_pi_lambda_expr_aux new_env depth body in
                {
                        texpr = T_lambda (var, typed_body);
                        loc = expr.loc;
                        typ = Tarrow ((find var new_env), typed_body.typ)
                }
| E_ident var ->
                (*
                 *
                 * (var) ────────────────
                 *	 Γ, x:τ ; Δ ⊢ x:τ
                 * *)
                {
                        texpr = T_ident var;
                        loc = expr.loc;
                        typ = find var env
                }
| E_app (f, arg) ->
                (*
                 *       Γ ; Δ ⊢ u : τ' → τ   Γ ; Δ ⊢ v : τ'
                 * (app) ───────────────────────────────────
                 *              Γ ; Δ ⊢ (u) (v) : τ
                 *
                 * *)
                let typed_f = type_pi_lambda_expr_aux env depth f in
                let typed_arg = type_pi_lambda_expr_aux env depth arg in
                let result_type = Tvar {
                        var_id = fresh_var_id ();
                        var_depth = depth; 
                        var_type = None
                } in
                let _ = unify typed_f.typ (Tarrow (typed_arg.typ, result_type)) in
                {
                        texpr = T_app (typed_f, typed_arg);
                        loc = expr.loc;
                        typ = type_val result_type
                }

| E_para pl ->
                (*
                 *
                 *          Γ ; Δ ⊢ u : τ  ...  Γ ; Δ ⊢ v : τ'
                 * (para) ──────────────────────────────────────
                 *        Γ ; Δ ⊢ [u || ... || v] : τ × ... × τ'
                 *
                 * *)
                let local_w = type_pi_lambda_expr_aux env depth in
                let typed_term_list = List.map local_w pl in
                let extract_types texpr = texpr.typ in
                let type_list = List.map extract_types typed_term_list in
                {
                        texpr = T_para typed_term_list;
                        loc = expr.loc;
                        typ = Tcross type_list

                }

| E_chan (ident, body) -> 
                let chan_type = (Tchan (
                        {
                                chan_id = fresh_chan_id ();
                                chan_depth = 1 + depth
                        },
                        Tvar {
                                var_id = fresh_var_id ();
                                var_depth = depth;
                                var_type = None
                        }))
                in
                let new_env = add ident chan_type env in
                let typed_body = type_pi_lambda_expr_aux new_env (depth + 1) body in
                {
                        texpr = T_chan (ident, typed_body);
                        loc = expr.loc;
                        typ = Tdot(type_val chan_type, typed_body.typ)
                }

| E_send (chan, msg, cont) ->
                let typed_msg = type_pi_lambda_expr_aux env 0 msg in (* Reinitialise depth, so we can send closed processes, e.g. #c. c[\b->b].c *)
                let typed_cont = type_pi_lambda_expr_aux env depth cont in
                let chan_type = 
                        match find chan env with
                        | (Tchan _) as t -> t
                        | (Tvar v) as t -> (* if it is not a Tchan (i.e. a Tvar), then first unify with a channel *)
                                unify (Tchan ({chan_id = fresh_chan_id (); chan_depth = v.var_depth}, Tvar {var_id = fresh_var_id (); var_depth = v.var_depth; var_type = None})) t; t
                        | t -> t
                in
                let _ = unify_chan_inner (channel_type chan_type) typed_msg.typ in
                {
                        texpr = T_send(chan, typed_msg, typed_cont);
                        loc = expr.loc;
                        typ = Tsend_chan(type_val chan_type, type_val typed_cont.typ)
                }

| E_deliver (chan, ident, cont) -> 
                let chan_depth = 
                        match find chan env with
                        | Tchan (i, _) -> i.chan_depth
                        | Tvar v -> v.var_depth
                        | _ -> assert false (* should not happen, `chan` should be a channel or a "channel to be", i.e. a var *)
                in
                let new_env = add ident (Tvar {var_id = fresh_var_id (); var_depth = chan_depth-1; var_type = None}) env in
                let typed_cont = type_pi_lambda_expr_aux new_env depth cont in
                let ident_type = find ident new_env in
                let chan_type = 
                        match find chan env with
                        | (Tchan _) as t -> t
                        | (Tvar v) as t -> (* if it is not a Tchan (i.e. a Tvar), then first unify with a channel *)
                                unify (Tchan ({chan_id = fresh_chan_id (); chan_depth = v.var_depth}, Tvar {var_id = fresh_var_id (); var_depth = v.var_depth; var_type = None})) t; t
                        | t -> t
                in
                let _ = unify_chan_outer (channel_type chan_type) ident_type in
                {
                        texpr = T_deliver(chan, ident, typed_cont);
                        loc = expr.loc;
                        typ = Tdeliver_chan(type_val chan_type, type_val typed_cont.typ)
                }
                


exception ChannelLeakError of string

let rec innermostChannel tast = 
        let aux i t = 
                match innermostChannel t, i with
                | Some it, Some i -> Some (Pervasives.max i it)
                | _, None -> innermostChannel t
                | None, _ -> i
        in
match tast with
| Tvar v -> Some v.var_depth
| Tchan (i, t) -> aux (Some i.chan_depth) t
| Tarrow (i, t) -> innermostChannel t
| Tsend_chan (t1, t2)
| Tdeliver_chan (t1, t2)
| Tdot (t1, t2) -> aux (innermostChannel t2) t1
| Tcross tl -> List.fold_left aux None tl

let rec checkChannelLeak tast =
match tast with
| Tchan (i, t) ->
                begin match innermostChannel t with
                | None -> ()
                | Some ic -> 
                        if i.chan_depth <= ic 
                        then 
                                raise (ChannelLeakError "A channel leaks")
                        else 
                                ()
                end
| Tarrow (i, t) -> checkChannelLeak t
| Tvar _ -> ()
| Tsend_chan(t1, t2)
| Tdeliver_chan(t1, t2)
| Tdot (t1, t2) -> checkChannelLeak t1; checkChannelLeak t2
| Tcross tl -> List.iter checkChannelLeak tl

let type_pi_lambda_expr expr = 
        let tast = type_pi_lambda_expr_aux [] 0 expr in
        checkChannelLeak tast.typ; tast

let rec string_of_tast_aux (depth: bool list ) expr = 
        let prefix = depth_shift depth in
        let string_of_sub_ast = string_of_tast_aux (depth @ [true] ) in 
        let string_of_sub_ast_last = string_of_tast_aux (depth @ [false]) in
        let sub_tree_string = 
                match expr.texpr with
                | T_lambda (i, e) -> 
                                "lambda: " ^ (print_type expr.typ) ^ "\n"
                                ^ prefix ^ i ^ "\n" 
                                ^ prefix ^ string_of_sub_ast_last e 
                | T_ident i ->
                                "ident: " ^ (print_type expr.typ) ^ "\n"
                                ^ prefix ^ i ^ "\n"
                | T_app (e1, e2) ->
                                "application: " ^ (print_type expr.typ) ^ "\n"
                                ^ prefix ^ string_of_sub_ast e1 
                                ^ prefix ^ string_of_sub_ast_last e2 
                | T_para el ->
                                let rec print_list l = 
                                match l with
                                | e :: [] -> prefix ^ string_of_sub_ast_last e
                                | e :: lt -> prefix ^ string_of_sub_ast e ^ (print_list lt)
                                | [] -> ""
                                in
                                "parallel: " ^ (print_type expr.typ) ^ "\n"
                                ^ print_list el
                | T_chan (i, e) -> 
                                "new channel: " ^ (print_type expr.typ) ^ "\n" 
                                ^ prefix ^ i ^ "\n" 
                                ^ prefix ^ string_of_sub_ast_last e 
                | T_send (i, e1, e2) ->
                                "send: " ^ (print_type expr.typ) ^ "\n"
                                ^ prefix ^ i ^ "\n"
                                ^ prefix ^ string_of_sub_ast e1
                                ^ prefix ^ string_of_sub_ast_last e2
                | T_deliver (c, v, e) ->
                                "deliver: " ^ (print_type expr.typ) ^ "\n"
                                ^ prefix ^ c ^ "\n"
                                ^ prefix ^ v ^ "\n"
                                ^ prefix ^ string_of_sub_ast_last e
        in
        sub_tree_string  

let string_of_tast texpr = string_of_tast_aux [] texpr

let rec term_string_of_tast (t: typed_expr) =
match t.texpr with
| T_lambda (i, t) -> "\\" ^ i ^ " -> " ^ (term_string_of_tast t)
| T_ident i -> i
| T_app (t1, t2) -> "(" ^ term_string_of_tast t1 ^ ") (" ^ term_string_of_tast t2 ^ ")"
| T_para el -> 
                let concat_para s1 s2 = " || " ^ s1 ^ s2 in
                let elem_s = List.fold_left concat_para "" (List.map term_string_of_tast el) in
                "[ " ^ elem_s ^ " ]"
| T_chan (i, t) -> "#" ^ i ^ ". " ^ (term_string_of_tast t)
| T_send (i, t1, t2) -> i ^ "[" ^ (term_string_of_tast t1) ^ "]." ^ (term_string_of_tast t2)
| T_deliver (c, v, t) -> c ^ ", " ^ v ^ "> " ^ (term_string_of_tast t)


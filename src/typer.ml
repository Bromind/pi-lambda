open Ast
open Pi_lambda_types
open Identifier
open Constructor

exception NotImplemented of string
(* Expected type (i.e. type of matched term), found type *)
exception PatternTypeError of pi_lambda_type * pi_lambda_type

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
| T_type of ident * constructor list * typed_expr
| T_match of typed_expr * (typed_expr * typed_expr) list


type environment = (ident * pi_lambda_type) list

(* Adds ident `s` bound to type `t` in environment `env` *)
let add s t env = 
        (s, t)::env

exception VarNotFound of string
(* Finds the type of ident `s` in environment `env` *)
(* TODO Utiliser filter *)
let rec find s env =
match env with
| (s_hd, t)::tl -> if s = s_hd then type_val t else find s tl
| [] -> raise (VarNotFound ("Variable not found: " ^ s))

exception UnionError of string
exception UnificationError of pi_lambda_type * pi_lambda_type * loc option
let union (t1: pi_lambda_type) (t2: pi_lambda_type) = 
match type_val t1, type_val t2 with
| Tvar v1, Tvar v2 when v1.var_id = v2.var_id -> ()
| Tvar v1, _ -> 
                if occur v1 t2 
                then 
                        raise (UnificationError (t1, t2, None))
                else v1.var_type <- Some t2
| _, Tvar v2 -> 
                if occur v2 t1 
                then 
                        raise (UnificationError (t1, t2, None))
                else v2.var_type <- Some t1
| _ -> ()

let rec unify t1 t2: unit = 
match type_val t1, type_val t2 with
| Tsend_chan(ch1, c1), Tsend_chan(ch2, c2) -> 
                (unify_inner (channel_type ch1) (channel_type ch2)); (unify c1 c2)
| Tdeliver_chan(ch1, c1), Tdeliver_chan(ch2, c2) -> 
                (unify_outer (channel_type ch1) (channel_type ch2)); (unify c1 c2)
| Tchan _, Tchan _ -> raise (UnificationError (t1, t2, None))
| Tvar _, _
| _, Tvar _ -> union t1 t2
| Tname n1, Tname n2 when n1 = n2 -> ()
| Tname n1, Tname n2 -> raise (UnificationError (t1, t2, None))
| _ -> raise (NotImplemented "unification")
 
(* The most restricted channed *)
and unify_inner t1 t2: unit =
match type_val t1, type_val t2 with
| Tvar v1, Tvar v2 when v1.var_id = v2.var_id -> ()
| Tvar v1, Tvar v2 -> 
                let inner_var = Pervasives.max v1.var_depth v2.var_depth in
                v1.var_depth <- inner_var;
                v2.var_depth <- inner_var;
| Tchan (id1, c1), Tchan (id2, c2) -> 
                let inner_chan = Pervasives.max id1.chan_depth id2.chan_depth in
                id1.chan_depth <- inner_chan;
                id2.chan_depth <- inner_chan;
                (* Take the most general message type *)
                unify_outer c1 c2
| Tarrow (targ1, tbody1), Tarrow (targ2, tbody2) -> 
                (unify_inner targ1 targ2); (unify_outer tbody1 tbody2)
| _ -> unify t1 t2

(* The most general channel *)
and unify_outer t1 t2: unit =
match type_val t1, type_val t2 with
| Tvar v1, Tvar v2 when v1.var_id = v2.var_id -> ()
| Tvar v1, Tvar v2 -> 
                let outer_var = Pervasives.min v1.var_depth v2.var_depth in
                v1.var_depth <- outer_var;
                v2.var_depth <- outer_var;
| Tchan (id1, c1), Tchan (id2, c2) -> 
                let outer_chan = Pervasives.min id1.chan_depth id2.chan_depth in
                id1.chan_depth <- outer_chan;
                id2.chan_depth <- outer_chan;
                (* Take the more restricted message type *)
                unify_inner c1 c2
| Tarrow (targ1, tbody1), Tarrow (targ2, tbody2) -> 
                (unify_outer targ1 targ2); (unify_inner tbody1 tbody2)
| _ -> unify t1 t2

exception ChannelLeakError of ident option * depth * ident option * depth * loc option

(* Returns a list containing the free variables of tast along with their type *)
let rec free_names_types tast: (ident * pi_lambda_type) list = 
let filter var = fun n -> 
        let (var2, _) = n in
        var != var2 
in 
match tast.texpr with
| T_ident v -> [(v, tast.typ)]
| T_send (chan, msg, cont) -> 
                begin
                match tast.typ with
                | Tdot (t, _) ->
                                [(chan, t)]@(free_names_types msg)@(free_names_types cont)
                | _ -> assert false;
                end
| T_deliver (chan, var, cont) -> 
                let cont_filtered = List.filter (filter var) (free_names_types cont) in
                begin
                match tast.typ with
                | Tdot (t, _) ->
                                [(chan, t)]@cont_filtered
                | _ -> assert false;
                end
| T_chan (i, t) 
| T_type (i, _, t)
| T_lambda (i, t) ->
                List.filter (fun elem -> let (e_name, _) = elem in e_name <> i) (free_names_types t)
| T_app (t1, t2) ->
                (free_names_types t1) @ (free_names_types t2)
| T_para tl -> 
                let para_free_names_types = List.map free_names_types tl in
                List.flatten para_free_names_types
| T_match _ -> raise PatternMatchingNotImplemented

(* Returns a list containing the free variables of tast.*)
let free_names tast: ident list = 
        let typed_ident = free_names_types tast in
        List.map (function | (ident, typ) -> ident) typed_ident


let innermostChannel tast = 
        let rec innerChannels tast = 
        match tast with
        | Tvar v -> [(v.var_id, v.var_depth)]
        | Tchan (i, t) -> [(i.chan_id, i.chan_depth)] @ (innerChannels t)
        | Tarrow (targ, t_res) -> (innerChannels targ) @ (innerChannels t_res)
        | Tsend_chan (t1, t2) 
        | Tdeliver_chan (t1, t2) -> (innerChannels t1) @ (innerChannels t2)
        | Tdot (t1, t2) -> 
                        let t1_l = innerChannels t1 in
                        let t2_l = innerChannels t2 in
                        List.filter (fun elem -> List.exists (fun elem_t1 -> elem_t1 = elem) t1_l) t2_l
        | Tcross tl -> List.flatten(List.map innerChannels tl)
        | Tname _ -> []
        in
        let rec innermost tl = 
        match tl with
        | [] -> None
        | (_, depth)::b ->
                begin match innermost b with
                | None -> Some depth
                | Some db -> Some (Pervasives.max db depth)
                end
        in
        innermost (innerChannels tast)


let rec checkChannelLeak tast =
match tast with
| Tchan (i, t) ->
                begin match innermostChannel t with
                | None -> ()
                | Some ic -> 
                        if i.chan_depth <= ic 
                        then 
                                raise (ChannelLeakError (None, i.chan_depth, None, ic, None))
                        else 
                                ()
                end
| Tarrow (i, t) -> checkChannelLeak t
| Tvar _ -> ()
| Tsend_chan(t1, t2)
| Tdeliver_chan(t1, t2)
| Tdot (t1, t2) -> checkChannelLeak t1; checkChannelLeak t2
| Tcross tl -> List.iter checkChannelLeak tl
| Tname _ -> ()

exception ErrorConstructorNotWellTyped of constructor list
let rec type_pi_lambda_expr_aux (env: environment) (depth: int) (expr: expr): typed_expr =
match expr.exp with
| E_lambda (var, body) ->
                (* We compute body's type, knowing that var has type α *)
                let new_env = add var (Tvar {
                        var_id = fresh_var_id None ();
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
                        var_id = fresh_var_id None ();
                        var_depth = depth; 
                        var_type = None
                } in
                begin
                try
                        unify_outer typed_f.typ (Tarrow (typed_arg.typ, result_type));
                        {
                                texpr = T_app (typed_f, typed_arg);
                                loc = expr.loc;
                                typ = type_val result_type
                        }
                with
                | UnificationError (t1, t2, None) ->
                                raise (UnificationError (t1, t2, Some expr.loc))
                end

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
                                chan_id = fresh_chan_id None ();
                                chan_depth = 1 + depth
                        },
                        Tvar {
                                var_id = fresh_var_id None ();
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
                let typed_msg = type_pi_lambda_expr_aux env depth msg in (* Reinitialise depth, so we can send closed processes, e.g. #c. c[\b->b].c *)
                
                (*  e.g. #c. c[\b->b].c *)
                let free_names_msg = free_names typed_msg in
                let most_inner_free_name, most_inner_free_name_depth = 
                        List.fold_left 
                                (fun (a, b) (a', b') ->
                                        if Pervasives.max b b' = b
                                        then (a, b)
                                        else (a', b')
                                ) 
                                ("", 0)
                                (List.map (fun n -> (n, depth_of (find n env))) free_names_msg) 
                in
                let chan_type = 
                        match find chan env with
                        | (Tchan _) as t -> t
                        | (Tvar v) as t -> (* if it is not a Tchan (i.e. a Tvar), then first unify with a channel *)
                                unify_outer (Tchan ({chan_id = fresh_chan_id None (); chan_depth = v.var_depth}, Tvar {var_id = fresh_var_id None (); var_depth = v.var_depth; var_type = None})) t; t
                        | t -> t
                in
                if (depth_of chan_type) <= most_inner_free_name_depth
                then
                        raise (ChannelLeakError (Some chan, depth_of chan_type, Some most_inner_free_name, most_inner_free_name_depth, Some expr.loc))
                else
                        let typed_cont = type_pi_lambda_expr_aux env depth cont in
                        let _ = unify_inner (channel_type chan_type) typed_msg.typ in
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
                let new_env = add ident (Tvar {var_id = fresh_var_id None (); var_depth = chan_depth-1; var_type = None}) env in
                let typed_cont = type_pi_lambda_expr_aux new_env depth cont in
                let ident_type = find ident new_env in
                let chan_type = 
                        match find chan env with
                        | (Tchan _) as t -> t
                        | (Tvar v) as t -> (* if it is not a Tchan (i.e. a Tvar), then first unify with a channel *)
                                unify_outer (Tchan ({chan_id = fresh_chan_id None (); chan_depth = v.var_depth}, Tvar {var_id = fresh_var_id None (); var_depth = v.var_depth; var_type = None})) t; t
                        | t -> t
                in
                let _ = unify_outer (channel_type chan_type) ident_type in
                {
                        texpr = T_deliver(chan, ident, typed_cont);
                        loc = expr.loc;
                        typ = Tdeliver_chan(type_val chan_type, type_val typed_cont.typ)
                }
| E_type (type_name, constructors, prog) ->
                begin
                try List.iter (fun (_, typ) -> checkChannelLeak typ) constructors;
                (* check for free type variables/free channels *)
                let new_env = 
                        List.fold_left
                        (fun env (constr_name, constr_typ) -> add constr_name constr_typ env)
                        env
                        constructors
                in
                let typed_prog = type_pi_lambda_expr_aux new_env depth prog in
                {
                        texpr = T_type(type_name, constructors, typed_prog);
                        loc = expr.loc;
                        typ = typed_prog.typ
                }
                with
                | ChannelLeakError _ -> raise (ErrorConstructorNotWellTyped constructors)
                end
| E_match (arg,  patterns) ->  
                let typed_arg = type_pi_lambda_expr_aux env depth arg in
                let type_pattern pattern = 
                        let (pat, res) = pattern in
                        let typed_pat = type_pi_lambda_expr_aux env depth pat in
                        let free_vars_pat = free_names_types typed_pat in
                        let new_env = free_vars_pat @ env in
                        let typed_res = type_pi_lambda_expr_aux new_env depth res in
                        (typed_pat, typed_res)
                in
                let typed_patterns = List.map type_pattern patterns in
                let assert_pattern_type_is_arg_type typed_pattern = 
                        let (typed_pat, _) = typed_pattern in
                        (* Generalize: typed_pat does not necessary need to 
                         * have the same type: it needs to have a 
                         * "more general" type, i.e. typ_arg.typ must be an 
                         * instance of typ_pat.typ *)
                        if typed_arg.typ <> typed_pat.typ
                        then
                                raise (PatternTypeError (typed_pat.typ, typed_arg.typ))
                in
                let returned_type = Tvar {
                        var_id = fresh_var_id None ();
                        var_depth = depth;
                        var_type = None}
                in
                let unify_all pattern = 
                        let (_, typed_ret) = pattern in
                        unify returned_type typed_ret.typ
                in
                List.iter assert_pattern_type_is_arg_type typed_patterns;
                List.iter unify_all typed_patterns;
                {
                        texpr = T_match(typed_arg, typed_patterns);
                        loc = expr.loc;
                        typ = returned_type
                }

let type_pi_lambda_expr expr = 
        let tast = type_pi_lambda_expr_aux [] 0 expr in
        (*checkChannelLeak tast.typ;*) tast

let rec string_of_tast_aux (depth: bool list ) expr = 
        let prefix = depth_shift depth in
        let string_of_sub_ast = string_of_tast_aux (depth @ [true]) in 
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
                | T_type (type_name, constructors, prog) -> 
                                let rec print_list l = 
                                match l with
                                | c :: lt -> prefix ^ string_of_constructor c ^ "\n" ^ (print_list lt)
                                | [] -> ""
                                in
                                "type " ^ type_name ^ "\n"
                                ^ print_list constructors
                                ^ prefix ^ string_of_sub_ast_last prog
                | T_match _ -> raise PatternMatchingNotImplemented
        in
        sub_tree_string  

let string_of_tast texpr = string_of_tast_aux [] texpr

let rec term_string_of_tast (t: typed_expr) =
match t.texpr with
| T_lambda (i, t) -> "\\" ^ i ^ " -> " ^ (term_string_of_tast t)
| T_ident i -> i
| T_app (t1, t2) -> "(" ^ term_string_of_tast t1 ^ ") (" ^ term_string_of_tast t2 ^ ")"
| T_para el -> 
                let concat_para s1 s2 =  s1 ^ " || " ^ s2 in
                let elem_s = List.fold_left concat_para "" (List.map term_string_of_tast el) in
                "[ " ^ elem_s ^ " ]"
| T_chan (i, t) -> "#" ^ i ^ ". " ^ (term_string_of_tast t)
| T_send (i, t1, t2) -> i ^ "[" ^ (term_string_of_tast t1) ^ "]." ^ (term_string_of_tast t2)
| T_deliver (c, v, t) -> c ^ ", " ^ v ^ "> " ^ (term_string_of_tast t)
| T_type (s, def, p) -> "type " ^ s ^ ":\n" ^ (string_of_constructor_list def) ^ ".\n" ^ (term_string_of_tast p)
| T_match _ -> raise PatternMatchingNotImplemented


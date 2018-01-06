
type depth = int

exception TypeError of string
type pi_lambda_type =
| Tvar of tvar
| Tchan of chan_id * pi_lambda_type
| Tarrow of pi_lambda_type * pi_lambda_type
| Tdot of pi_lambda_type * pi_lambda_type
| Tcross of pi_lambda_type list
| Tsend_chan of pi_lambda_type * pi_lambda_type
| Tdeliver_chan of pi_lambda_type * pi_lambda_type
| Tname of type_name
and chan_id = 
        {
                chan_id: id;
                mutable chan_depth: depth
        }

and tvar =
        {
                var_id: id;
                mutable var_depth: depth;
                mutable var_type: pi_lambda_type option
        }
and type_name = string
and id = 
| Named of string
| Anonymous of int


let counter_var_id = ref 0
let counter_chan_id = ref 0

let fresh_var_id s: unit -> id = 
match s with
| None ->
                fun () -> incr counter_var_id; Anonymous(!counter_var_id)
| Some s -> fun () -> Named s


let fresh_chan_id s: unit -> id = 
match s with
| None -> 
                fun () -> incr counter_chan_id; Anonymous(!counter_chan_id)
| Some s -> fun () -> Named s



let print_id id = 
match id with
| Named s -> s
| Anonymous i -> "α_" ^ string_of_int i

let rec type_val (t: pi_lambda_type): pi_lambda_type =
match t with
| Tvar {var_type = Some t} -> type_val t
| Tname _
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
                then "("^(print_type (type_val t))^")"
                else print_type (type_val t)
        in
match t with
| Tvar {var_id = i; var_depth = d; var_type = Some t} -> 
                (print_id i)^"^"^(string_of_int d)^" = "^(print_sub_type t)
| Tvar {var_id = i; var_depth = d; var_type = None} ->
                (print_id i)^"^"^(string_of_int d)
| Tchan (i, t) -> "<"^(print_id i.chan_id)^"^"^(string_of_int i.chan_depth)^", "^(print_type (type_val t))^">"
| Tarrow (arg, body) -> (print_sub_type arg) ^ " → " ^ (print_sub_type body)
| Tdot (c, body) -> (print_sub_type c) ^ "." ^ (print_sub_type body)
| Tsend_chan (chan, cont) -> "send on " ^ (print_sub_type (type_val chan)) ^ " then " ^ (print_sub_type (type_val cont))  
| Tdeliver_chan (chan, cont) -> "deliver from " ^ (print_sub_type (type_val chan)) ^ " in " ^ (print_sub_type (type_val cont))
| Tcross tl -> 
        let rec print_tl tl = 
                match tl with
                | [t] -> print_sub_type t
                | t::tl -> (print_sub_type t)^" × "^(print_tl tl)
                | [] -> "[]"
        in
        "["^(print_tl tl)^"]"
| Tname n -> n

let channel_type c =
match type_val c with
| Tchan (_, t) -> t
| _ -> raise (TypeError "Try to retrieve the channel type of a non-channel type")

let depth_of t = 
match t with
| Tvar {var_depth = d} -> d
| Tchan (i, t) -> i.chan_depth
| _ -> raise (TypeError "Try to retrieve the depth of a non-var/non-depth type")

(* Returns true iff variable v occurs in type t*)
let rec occur v t = 
        let occur_in_list l = 
                List.fold_left (||) false (List.map (occur v) l)
        in
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
| Tcross tl -> occur_in_list tl
| Tname _ -> false

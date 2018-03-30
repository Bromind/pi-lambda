open Pi_lambda_types
open Ast
open List
open Channel
open Event
open Constructor
open Identifier

exception RuntimeError 
exception ApplicationError of loc * expr * expr
exception NotImplementedError of loc * expr
exception SubstitutionError of loc * ident * expr

exception NoSuchChannel of string

(** Substitute ident by term1 in term2*)
let rec substitute_free_variable (ident: ident) (term1: expr) (term2: expr) : expr = 
        let subs_in = substitute_free_variable ident term1 in
        match term2.exp with
        | E_lambda (var, expr) -> (
                        if var = ident then
                                term2
                        else
                                {exp = E_lambda(var, subs_in expr); loc = term2.loc})
        | E_ident var ->
                        if var = ident then
                                term1
                        else
                                term2
        | E_app (f, arg) ->
                        ({exp = E_app(subs_in f, subs_in arg); loc = term2.loc}) 
        | E_para pl ->
                        {exp = E_para (map subs_in pl); loc = term2.loc}
        | E_chan (var, expr) -> 
                        {exp = E_chan(var, subs_in expr); loc = term2.loc}
        | E_send (chan, msg, cont) ->
                        {exp = E_send (chan, subs_in msg, subs_in cont); loc = term2.loc}
        | E_deliver (chan, var, expr) ->
                        if var = ident then
                                term2
                        else
                                {exp = E_deliver (chan, var, subs_in expr); loc = term2.loc}
        | E_type (constr_name, constr, prog) -> 
                        if constr_name = ident then
                                term2
                        else
                                {exp = E_type (ident, constr, subs_in prog); loc = term2.loc}
        | E_match _ -> raise PatternMatchingNotImplemented

let rec reduce_aux (chans: Concurrent_Channel.channel list) (constr: constructor list) (ast: expr): expr = 
        match ast.exp with
        | E_lambda (var, expr) -> ({ exp = E_lambda (var, reduce_aux chans constr expr); loc = ast.loc})
        | E_ident _ -> ast
        | E_app (f, arg) ->
                        begin
                                match (reduce_aux chans constr f).exp with
                                | E_lambda (var, expr) ->  
                                        (** Left reduction*)
                                        reduce_aux chans constr (substitute_free_variable var arg expr)
                                | _ -> {exp = E_app ((reduce_aux chans constr f), (reduce_aux chans constr arg)); loc = ast.loc}
                        end
        | E_para pl -> 
                        let reduce_process expr : expr event =
                                let chan = new_channel () in 
                                let job expr : unit= sync (send chan (reduce_aux chans constr expr)) in 
                                Thread.create job expr; 
                                receive chan
                        in
                        let events: expr event list = map reduce_process pl in
                        {exp = E_para (map (fun event -> sync event) events); loc = ast.loc}
                        
        | E_chan (var, expr) -> 
                        let new_channel = Concurrent_Channel.create_channel var in
                        let reduced_expr = reduce_aux (new_channel :: chans) constr expr in
                        {exp = E_chan (var, reduced_expr); loc = ast.loc}
        | E_send (chan, msg, cont) ->
                        let filter_rule = (fun c -> String.equal (Concurrent_Channel.name c) chan) in
                        begin try 
                                let chan = hd (filter filter_rule chans) in
                                Concurrent_Channel.push chan msg
                                with
                                | Failure _ -> raise (NoSuchChannel chan)
                        end;
                        reduce_aux chans constr cont
        | E_deliver (chan, var, expr) ->
                        let filter_rule = (fun c -> String.equal (Concurrent_Channel.name c) chan) in
                        begin try 
                                let chan = hd (filter filter_rule chans) in
                                Concurrent_Channel.pull chan 
                                with
                                | Failure _ -> raise (NoSuchChannel chan)
                        end
        | E_type (type_name, constructors, prog) -> 
                        let new_constrs = constructors @ constr in
                        { exp = E_type(type_name, constructors, reduce_aux chans new_constrs prog);
                        loc = ast.loc }
        | E_match _ -> raise PatternMatchingNotImplemented

        
let reduce (ast: expr): expr = 
        reduce_aux [] [] ast

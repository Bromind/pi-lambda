%{
        open Ast
        open Pi_lambda_types

        exception ParsingError of string

        let declared_t_vars = ref []
        let declared_c_vars = ref []
        let name_of c = 
                match c with
                | (n, _) -> n
        let type_of c = 
                match c with
                | (_, t) -> t

        (** Returns the type associated to the `name` in the list `l` and if not found, 
         * adds (name, f_not_found name) in the list *)
        let get_var_from_name_in_list name depth l f_not_found = 
                let filter elem = 
                        name_of elem = name
                in
                let sl = List.filter filter (!l) in
                match sl with
                | hd::_ -> 
                                let t = type_of hd in (* tail should be empty *)
                                if depth_of t = depth
                                then t
                                else raise (ParsingError ("Type variable " ^ name ^ " has depth " ^ (string_of_int depth) ^ " but previously had depth " ^ (string_of_int (depth_of t))))
                | [] ->
                                let new_var = f_not_found name depth in 
                                l := (name, new_var)::(!l);
                                new_var

        let get_tvar_from_name name depth = 
                let new_tvar name depth = Tvar {var_id = fresh_var_id (Some name) (); var_depth = depth; var_type = None} in 
                get_var_from_name_in_list name depth declared_t_vars new_tvar

        let get_cvar_from_name name depth chan_type= 
                let new_cvar name depth = Tchan ( {chan_id = fresh_chan_id (Some name) (); chan_depth = depth}, chan_type) in
                get_var_from_name_in_list name depth declared_t_vars new_cvar

%}

%token <string> IDENT
%token <int> CIRCUMDEPTH
%token LAMBDA CHAN DOT COMMA PARA LSB RSB GT LT EOF ARROW LPARENS RPARENS TYPE COLON 

%start file
%type <Ast.expr> file
%type <Ast.expr> program
%type <Ast.expr> parens_program
%type <Pi_lambda_types.pi_lambda_type> type_def

%%

file:
        p=program EOF
        { p }
;

program: 
| LAMBDA id=IDENT ARROW e=program
        { { exp = E_lambda(id, e); loc = ($startpos, $endpos)} }
| TYPE ; type_name = IDENT ; COLON ; f = type_def ; DOT; p = program
        { {exp = E_type (type_name, f, p); loc = ($startpos, $endpos)} }
| p_abs=parens_program p_arg = parens_program
        { { exp = E_app(p_abs, p_arg); loc = ($startpos, $endpos)} }
| LSB local_progs = separated_nonempty_list(PARA, program) RSB
        { { exp = E_para(local_progs); loc = ($startpos, $endpos)} }
| CHAN ; i = IDENT ; DOT e=program
        { { exp = E_chan(i, e); loc = ($startpos, $endpos)} }
| dest = IDENT LSB msg = program RSB DOT cont = program
        { { exp = E_send(dest, msg, cont); loc = ($startpos, $endpos)} }
| var = IDENT
        { { exp = E_ident(var); loc = ($startpos, $endpos)} }
| c = IDENT COMMA v = IDENT GT cont = program
        { { exp = E_deliver(c, v, cont); loc = ($startpos, $endpos)} }
| e = parens_program
        { e }
;


type_def:
| id = IDENT; depth = CIRCUMDEPTH
        { get_tvar_from_name id depth }
| LPARENS; t1 = type_def; ARROW; t2 = type_def; RPARENS
        { Tarrow (t1, t2) }
| LT; chan = IDENT; depth = CIRCUMDEPTH; COMMA; chan_type = type_def; GT; 
        { get_cvar_from_name chan depth chan_type }

parens_program:
        LPARENS ; p = program ; RPARENS
        { p }
;

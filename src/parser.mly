%{
        open Ast
%}

%token <string> IDENT
%token LAMBDA CHAN DOT COMMA PARA LSB RSB GT EOF ARROW LPARENS RPARENS 

%start file
%type <Ast.expr> file
%type <Ast.expr> program
%type <Ast.expr> parens_program

%%

file:
        p=program EOF
        { p }
;

program: 
| LAMBDA id=IDENT ARROW e=program
        { { exp = E_lambda(id, e); loc = ($startpos, $endpos)} }
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

parens_program:
        LPARENS ; p = program ; RPARENS
        { p }
;

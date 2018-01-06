open Pi_lambda_types
open Identifier

type constructor = (ident * pi_lambda_type) 

let string_of_constructor c = 
        let (ident, typ) = c in
        "| " ^ ident ^ ": " ^ print_type typ

let string_of_constructor_list cl = 
        List.fold_right (fun c s -> (string_of_constructor c) ^ "\n" ^ s) cl ""

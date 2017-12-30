open Ast
open List
open Mutex

type channel = {
        name: string ;
        queue: expr list ref ;
        m: Mutex.t
}

let create_channel n =
        {name = n ; queue = ref []; m = create ()}

let push (mp: channel) (message: expr): unit = 
        lock mp.m;
        let current_queue = mp.queue in
        let new_queue = !current_queue @ [ message ] in
        mp.queue := new_queue;
        unlock mp.m

let pull mp = 
        lock mp.m;
        match !(mp.queue) with
        | [] -> None
        | hd::tl -> mp.queue := tl; 
                unlock mp.m;
                Some hd

(* TODO Use Set.Make (require to impl OrderedType for message_passing) *)
type channels = channel list

exception NoSuchChannel of string

let send (chans: channels) (var: string) (msg: expr): unit = 
        let filter_rule = (fun c -> String.equal c.name var) in
        try 
                let chan = hd (filter filter_rule chans) in
                push chan msg
        with
        | Failure _ -> raise (NoSuchChannel var)

let deliver (chans: channels) (chan_name: string): expr option =
        let filter_rule = (fun c -> String.equal c.name chan_name) in
        try 
                let chan = hd (filter filter_rule chans) in
                pull chan 
        with
        | Failure _ -> raise (NoSuchChannel chan_name)


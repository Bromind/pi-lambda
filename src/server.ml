open Yojson
open Yojson.Basic.Util
open Unix
open Identifier
open Ast
open Lexing
open Parser
open Format
open Channel
open List

type environment = unit (* ident * expr list *)
type load = expr
type producer = out_channel

type task = {
        task_id: int; (* The local ID of the task *)
        server_id: int; (* The ID of the server hosting the parent task *)
        load: load; (* The term to evaluate *)
        env: environment; (* The environment (e.g.: variables, channel locations, etc.) *)
        prod: producer; (* The communication toward the parent task, to send the reduced term when finished *)
}

type server_channel = { 
        channel_owner_id: int; (* The ID of the task that opened the channel *)
        channel: Concurrent_Channel.channel;
}

type server = {
        server_id: int;
        tasks: task list;
}

(** Associate idents to terms *)
let environment_of_string s = () (* [] *)

let load_of_string s =
        let buf = Lexing.from_string s in
        let ast = Parser.file Lexer.token buf in
        ast

let task_completed_to_string task = 
        let json = `Assoc [ ("task_id", `Int task.task_id); ("load", `String (term_of_ast task.load))] in
        Yojson.Basic.pretty_to_string json

let task_of_data producer json = 
        let json_task = json |> member "task" in
        let env = json_task |> member "environment" |> to_list |> List.map to_string |> environment_of_string in
        let payload = json_task |> member "payload" |> to_string |> load_of_string in
        let task_id = json_task |> member "task_id" |> to_int in 
        let server_id = json_task |> member "server_id" |> to_int in
        {
                server_id = server_id;
                task_id = task_id;
                load = payload;
                env = env;
                prod = producer
        }
        
let consume_task task = 
        let completed_task = task in (* TODO : reduce term *)
        output_string task.prod (task_completed_to_string completed_task)

let () = 
        let addr = ADDR_INET ((inet_addr_of_string "127.0.0.1"), 8888) in
        let server = {
                server_id = 0;
                tasks = []
        } in
        let f i o = 
                let data = input_line i in
                let json = Yojson.Basic.from_string data in
                let keys = keys json in
                if exists (fun s -> s = "task") keys 
                then
                        let task = task_of_data o json in
                        consume_task task
                else if exists (fun s -> s = "send") keys
                then 
                        print_string "Send msg"
                else
                        ()
        in
        establish_server f addr


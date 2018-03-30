open Yojson
open Yojson.Basic.Util
open Unix

type environment = unit
type load = unit
type producer = out_channel

type task = {
        id: int;
        load: load;
        env: environment;
        prod: producer;
}

let environment_of_string s = 
        ()

let load_of_string s = 
        ()

let string_of_load load = ""

let task_completed_to_string task = 
        let json = `Assoc [ ("id", `Int task.id); ("load", `String (string_of_load task.load))] in
        Yojson.Basic.pretty_to_string json

let task_of_data producer data = 
        let json = Yojson.Basic.from_string data in
        let json_task = json |> member "task" in
        let env = json_task |> member "environment" |> to_list |> List.map to_string |> environment_of_string in
        let payload = json_task |> member "payload" |> to_string |> load_of_string in
        let id = json_task |> member "task_id" |> to_int in 
        {
                id = id;
                load = payload;
                env = env;
                prod = producer
        }
        
let consume_task task = 
        let completed_task = task in (* TODO : reduce term *)
        output_string task.prod (task_completed_to_string completed_task)


let () = 
        let addr = ADDR_INET ((inet_addr_of_string "127.0.0.1"), 8888) in
        let f i o = 
                let data = input_line i in
                print_string data;
                let task = task_of_data o data in
                consume_task task
        in
        establish_server f addr


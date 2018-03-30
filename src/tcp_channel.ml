open Channel
open Yojson
open Yojson.Basic.Util
open Unix
open Lexing
open Parser

module Tcp_Channel : Channel = struct 
type channel = {
        name: string;
        addr: inet_addr;
        port: int;
}

let pull chan = 
        let addr = ADDR_INET(chan.addr, chan.port) in
        let fd = socket (domain_of_sockaddr addr) SOCK_STREAM 0 in 
        connect fd addr;
        let out_chan = out_channel_of_descr fd in
        let in_chan = in_channel_of_descr fd in
        let id = 123 in (* TODO *)
        let msg = "\"pull\": {\"pull_id\": " ^ (string_of_int id) ^ ", \"channel_name\": \"" ^ chan.name ^"\"}" in
        output_string out_chan msg;
        let data = input_line in_chan in
        let json = Yojson.Basic.from_string data |> member "answer" in
        let id_receive = json |> member "pull_id" |> to_int in
        let term = json |> member "answer" |> to_string in
        let buf = Lexing.from_string term in
        let p = Parser.file Lexer.token buf in
        if id = id_receive
        then
                p
        else
                failwith "Answer id does not match request id" (* TODO *)

let push chan ast = failwith "unimplemented"
let create_channel s = 
        {
                name = s;
                addr = inet_addr_of_string "127.0.0.1";
                port = 8889;
        }

let name c = 
        c.name
end

open Yojson
open Yojson.Basic.Util
open Unix
open Tree
open Network_topology

let rec json_to_tree json = 
        let node_addr = json |> member "addr" |> to_string |> inet_addr_of_string in
        let node_port = json |> member "port" |> to_int in
        let subtrees = json |> member "children" |> to_list |> List.map json_to_tree in
        let node = {addr = node_addr; port = node_port} in 
        if List.length subtrees = 0
        then
                Leaf node
        else 
                Node (node, subtrees)

let node_list_of_filename filename = 
        let json = Yojson.Basic.from_file filename in
        let nodes = json |> member "nodes" |> to_list |> List.map json_to_tree in
        nodes



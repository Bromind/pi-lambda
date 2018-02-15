open Topology_config_parser
open Network_topology

let () = 
        let tree = node_list_of_filename "config.json" in
        let f n = 
                let str = string_of_node n in
                print_string (str ^ "\n")
        in
        let print_tree tree = Tree.iter f tree in
        List.iter print_tree tree

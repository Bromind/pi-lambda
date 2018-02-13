open Topology_config_parser
open Network_topology

let () = 
        let tree = tree_of_filename "config.json" in
        let f n = 
                let str = string_of_node n in
                print_string str
        in
        Tree.iter f tree

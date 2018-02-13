open Unix

type node = {
        addr: inet_addr;
        port: int
}

let string_of_node n = 
        (string_of_inet_addr n.addr) ^ ":" ^ (string_of_int n.port)

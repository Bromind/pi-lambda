
type 'a tree =
        | Node of 'a * 'a tree list
        | Leaf of 'a

let rec iter f t = 
match t with 
| Node (elem, children) -> 
                f elem; 
                let fp subtree = iter f subtree in
                List.iter fp children
| Leaf e -> f e

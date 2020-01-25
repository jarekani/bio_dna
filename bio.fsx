type PreNode = char * string * char 
type Node = char * string //e.g. first and middle, last in edge
type PreEdge = char
type Edge = char * Node// link form NODE A (parent) to NODE B (Node) via char
type PreVertex = Node * PreEdge list 
type Vertex = Node * Edge list
type Graph = Vertex list


let updateElement key f st = 
  st |> List.map (fun (k, v) -> if k = key then k, f v else k, v)
  
let rec eleExist key st = 
    match st with
    | [] -> false
    | [h] -> 
        match h with
        |(k,_) when k = key -> true
        |(_,_) -> false
    | h::r -> 
        match h with
        |(k,_) when k = key -> true
        |(_,_) -> eleExist key r

let TupleEquality t1 t2 = 
    match t1, t2 with
    | (k1,v1), (k2,v2) when k1=k2 && v1=v2 -> true
    | _,_ -> false


let createPreNode (input: string): PreNode =
    (input.[0],input.[1 .. input.Length - 2],input.[input.Length - 1])

let createPreEdge (input: PreNode): PreEdge =
    match input with
    | (_,_,c) -> c

let createNode (input: PreNode): Node =
    match input with
    | (f,rest,_) -> (f,rest)

let createPreVertex (input: PreNode):PreVertex =
    let n = createNode(input)
    let e = createPreEdge(input)
    (n,[e])


let clearPreVertex (input: PreVertex list): PreVertex list =
    let rec tmp (input: PreVertex list) (acc: PreVertex list):  PreVertex list =
        match input with
        | H::R -> 
            match H with
                | (N,PEL) when eleExist N acc -> 
                    let acc' = updateElement N (fun PEL_ACC -> PEL_ACC @ PEL) acc
                    tmp R acc'
                | (N,PEL) -> 
                    let acc' = (N,PEL) :: acc
                    tmp R acc'
        |  [H] -> match H with
                | (N,PEL) when eleExist N acc -> 
                    let acc' = updateElement N (fun PEL_ACC -> PEL_ACC @ PEL) acc
                    tmp [] acc'
                | (N,PEL) -> 
                    let acc' = (N,PEL) :: acc
                    tmp [] acc'
        | [] -> acc
    tmp input []

let mapEdge (node: Node) (e: PreEdge) (input: PreVertex List): Edge list =
    let rec tmp (node: Node) (e: PreEdge) (input: PreVertex List) acc =
        match input with
        | H::R -> 
            match H with
                | ((f,_) as n,_) when TupleEquality n node |> not && f = e ->   // TupleEquality n node |> not    <=>  !TupleEquality n node  <=> n != node
                    let acc' = (e,n) :: acc
                    tmp node e R acc'
                | (_,_) ->
                    tmp node e R acc
        | [H] ->
            match H with
                | ((f,_) as n,_) when TupleEquality n node |> not && f = e -> 
                    let acc' = (e,n) :: acc
                    tmp node e [] acc'
                | (_,_) ->
                    tmp node e [] acc
        | [] -> acc
    tmp node e input []

let mapPreVertex ((node,PEL): PreVertex) (input: PreVertex List): Vertex =
    let rec tmp (node: Node) (PEL: PreEdge List) (input: PreVertex List) acc = 
        match PEL with
        | H::R ->  
            let e = mapEdge node H input
            let acc' = e::acc
            tmp node R input acc'
        | [H] -> 
            let e = mapEdge node H input
            let acc' = e::acc
            tmp node [] input acc'        
        | [] -> acc
    let EL = tmp node PEL input []
    (node,List.concat EL)


let createGraph (input: string list) : Graph =
    let preVertex = input |> List.map createPreNode |> List.map createPreVertex |> clearPreVertex 
    preVertex |> List.map (fun pf -> mapPreVertex pf preVertex)

    

let input = ["AAA"; "AAC"; "ACA"; "CAC"; "CAA";"ACG"; "CGC"; "GCA"; "ACT";"CTT";"TTA";"TAA"]
//len of strings
let k = 3

//input |> List.map createPreNode |> List.map createPreVertex |> clearPreVertex
createGraph input
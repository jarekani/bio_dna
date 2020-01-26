// type PreNode = char * string * char 
type Node = string
// type PreEdge = char
type Edge = char * Node// link form NODE A (parent) to NODE B (Node) via char
// type PreVertex = Node * PreEdge list 
type Vertex = Node * Edge list
type Graph = Vertex list


// let updateElement key f st = 
//   st |> List.map (fun (k, v) -> if k = key then k, f v else k, v)
  
// let rec eleExist key st = 
//     match st with
//     | [] -> false
//     | [h] -> 
//         match h with
//         |(k,_) when k = key -> true
//         |(_,_) -> false
//     | h::r -> 
//         match h with
//         |(k,_) when k = key -> true
//         |(_,_) -> eleExist key r

// let TupleEquality t1 t2 = 
//     match t1, t2 with
//     | (k1,v1), (k2,v2) when k1=k2 && v1=v2 -> true
//     | _,_ -> false


// let createPreNode (input: string): PreNode =
//     (input.[0],input.[1 .. input.Length - 2],input.[input.Length - 1])

// let createPreEdge (input: PreNode): PreEdge =
//     match input with
//     | (_,_,c) -> c

// let createNode (input: PreNode): Node =
//     match input with
//     | (f,rest,_) -> (f,rest)

// let createPreVertex (input: PreNode):PreVertex =
//     let n = createNode(input)
//     let e = createPreEdge(input)
//     (n,[e])


// let clearPreVertex (input: PreVertex list): PreVertex list =
//     let rec tmp (input: PreVertex list) (acc: PreVertex list):  PreVertex list =
//         match input with
//         | H::R -> 
//             match H with
//                 | (N,PEL) when eleExist N acc -> 
//                     let acc' = updateElement N (fun PEL_ACC -> PEL_ACC @ PEL) acc
//                     tmp R acc'
//                 | (N,PEL) -> 
//                     let acc' = (N,PEL) :: acc
//                     tmp R acc'
//         |  [H] -> match H with
//                 | (N,PEL) when eleExist N acc -> 
//                     let acc' = updateElement N (fun PEL_ACC -> PEL_ACC @ PEL) acc
//                     tmp [] acc'
//                 | (N,PEL) -> 
//                     let acc' = (N,PEL) :: acc
//                     tmp [] acc'
//         | [] -> acc
//     tmp input []

// let mapEdge (node: Node) (e: PreEdge) (input: PreVertex List): Edge list =
//     let rec tmp (node: Node) (e: PreEdge) (input: PreVertex List) acc =
//         match input with
//         | H::R -> 
//             match H with
//                 | ((f,_) as n,_) when TupleEquality n node |> not && f = e ->   // TupleEquality n node |> not    <=>  !TupleEquality n node  <=> n != node
//                     let acc' = (e,n) :: acc
//                     tmp node e R acc'
//                 | (_,_) ->
//                     tmp node e R acc
//         | [H] ->
//             match H with
//                 | ((f,_) as n,_) when TupleEquality n node |> not && f = e -> 
//                     let acc' = (e,n) :: acc
//                     tmp node e [] acc'
//                 | (_,_) ->
//                     tmp node e [] acc
//         | [] -> acc
//     tmp node e input []

// let mapPreVertex ((node,PEL): PreVertex) (input: PreVertex List): Vertex =
//     let rec tmp (node: Node) (PEL: PreEdge List) (input: PreVertex List) acc = 
//         match PEL with
//         | H::R ->  
//             let e = mapEdge node H input
//             let acc' = e::acc
//             tmp node R input acc'
//         | [H] -> 
//             let e = mapEdge node H input
//             let acc' = e::acc
//             tmp node [] input acc'        
//         | [] -> acc
//     let EL = tmp node PEL input []
//     (node,List.concat EL)


// let createGraph (input: string list) : Graph =
//     let preVertex = input |> List.map createPreNode |> List.map createPreVertex |> clearPreVertex 
//     preVertex |> List.map (fun pf -> mapPreVertex pf preVertex)

let wordsToGraph (words:string list) :Graph = 
    words |> 
    List.map (fun s -> (s.[0 .. s.Length-2],s.[1 .. s.Length-1],s.[s.Length-1])) |> // (src,dst,e)
    List.groupBy (fun (src,_,_) -> src) |> 
    List.map (fun (n,adj) -> (n, adj |> List.map  (fun (_,dst,e) -> (e,dst))))

let rec createPossibleWords acc k = 
    let appendEveryElement l c = 
        l |> List.map (fun s -> s+c)
    match k with
    | 0 -> acc
    | x -> 
        createPossibleWords (appendEveryElement acc "A") (x-1) @ 
        createPossibleWords (appendEveryElement acc "C") (x-1) @ 
        createPossibleWords (appendEveryElement acc "G") (x-1) @ 
        createPossibleWords (appendEveryElement acc "T") (x-1)

let removeEdge src dst (graph:Graph) :Graph=
    let (node,adjacents) = graph |> List.find (fun (n,_) -> n = src) 
    let e = adjacents |> List.findIndex (fun (_,x) -> x = dst)
    let (a,_::b) = List.splitAt e adjacents
    let newAdjacents = (a@b)
    graph |> List.except [(node,adjacents)] |> List.append [(node,newAdjacents)]

let hasNoEdges (graph:Graph) =
    graph |> List.forall(fun (_,n) -> n.IsEmpty)

let getEulerianPath (graph:Graph) = 
    let edges = graph |> List.collect (fun (src,dste) -> dste |> List.collect (fun (e,dst) -> [(src,dst,e)]))
    let outEdges = edges |> List.countBy (fun (src,_,_) -> src) |> List.sort
    let inEdges = edges |> List.countBy (fun (_,dst,_) -> dst) |> List.sort
    let degs = List.map2 (fun (n,o) (_,i) -> (n,o-i)) outEdges inEdges
    let rec getPath ((path:Node list),(graph:Graph)) (curr:int) :Node list * Graph =
        if (graph |> hasNoEdges) then 
            (path |> List.rev ,graph)
        else 
            let (n,l) = graph |> List.find (fun (n,_) -> n = path.[curr])
            if (l.Length = 0) then
                if (curr = (path.Length - 1)) then
                    ([],graph) 
                else 
                    getPath (path,graph) (curr+1) 
            else 
                let (_,next) = l.[0]
                let (n,p) = path |> List.splitAt curr                  
                let newPath =  n@[next]@p
                getPath (newPath,(removeEdge path.[curr] next graph)) (curr)
    match List.sumBy (fun (_,d) -> abs(d)) degs with
    | 0 | 2 -> 
        let (startingNode,_) = outEdges |> List.maxBy (fun (_,d) -> d)
        let (result,_) = getPath ([startingNode],graph) 0
        result
    | _ -> []

let rec nodesToSequence acc (nodes:Node list) = 
    match nodes with
    | [] -> acc
    | h::t -> nodesToSequence (acc+string h.[h.Length-1]) t

let input = ["AAA"; "AAC"; "ACA"; "CAC"; "CAA";"ACG"; "CGC"; "GCA"; "ACT";"CTT";"TTA";"TAA"]
//len of strings
let k = 3

//input |> List.map createPreNode |> List.map createPreVertex |> clearPreVertex
// createGraph input

// TODO: generate all possible inputs by adding possible words to input list or removing a number of them

let graph = wordsToGraph input |> List.map (fun (a,n) -> (a,n |> List.rev))
let path = getEulerianPath graph

// TODO: generate all possible eulerian paths from given path

let sequence = nodesToSequence path.[0].[..k-3] path
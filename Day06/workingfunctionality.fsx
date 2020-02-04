[<StructuredFormatDisplay("{AsString}")>]
type Tree<'a when 'a : equality> =
    | Node of name : 'a * children : Tree<'a> list
    | Empty

    override this.ToString() =
        let sb = new System.Text.StringBuilder()
        sb.Append("\n") |> ignore
        let rec PrintTree (tree : Tree<'a>) level =
            match tree with
            | Node(value, children) ->
                sb.Append(String.replicate (level * 4) " ").Append(String.replicate 1 "└── ").Append(value).Append("\n")
                |> ignore
                for item in children do
                    PrintTree item (level + 1)
            | Empty ->
                sb.Append("None") |> ignore
                ()
        PrintTree this 0
        sb.ToString()

    member this.AsString = this.ToString()

    member this.Head : 'a option =
        match this with
        | Node(value, _) -> Some value
        | Empty -> None

    member this.Tail : Tree<'a> list =
        match this with
        | Node(_, children) -> children
        | Empty -> []

    member this.Exists(data : 'a) =
        let rec Exist (data : 'a) (tree : Tree<'a>) =
            match tree.Head with
            | Some head when head = data -> true
            | Some head -> List.fold (fun (acc : bool) (x : Tree<'a>) -> acc || Exist data x) false tree.Tail
        Exist data this

    member this.ExistsPair(data : 'a list) =
        let rec Find (d : 'a list) (i : Tree<'a>) =
            let a :: b :: _ = d
            match i.Head with
            | Some head when head = a ->
                List.fold (fun (acc : bool) (x : Tree<'a>) ->
                    match x.Head with
                    | Some tail when tail = b -> acc || true
                    | _ -> acc || false) false i.Tail
            | Some head -> List.fold (fun (acc : bool) (x : Tree<'a>) -> acc || Find d x) false i.Tail
            | _ -> false
        Find data this

    member this.Insert(data : 'a list) =
        if (this.ExistsPair data) then
            failwith "This pair already exists in this tree"
        else
            let rec Ins d t =
                let a :: b :: _ = d
                match t with
                | Node(value, _) when value = a -> Node(value, t.Tail @ [ Node(b, []) ])
                | Node(value, _) when value = b -> Node(a, [ t ])
                | Node(value, children) -> Node(value, List.map (Ins d) children)
                | Empty -> Node(a, [ Node(b, []) ])
            Ins data this

    member this.InsertNode(node : Tree<'a>) =
        let rec Ins (n : Tree<'a>) (t : Tree<'a>) =
            match t with
            | Node(value, _) when value = n.Head.Value ->
                let (concat : Tree<'a> list) = t.Tail @ n.Tail
                let grouped = List.groupBy (fun (x : Tree<'a>) -> x.Head.Value) concat

                let res =
                    List.collect (fun group ->
                        let (key : 'a), (value : Tree<'a> list) = group
                        if value.Length > 1
                        then [ List.fold (fun (acc : Tree<'a>) (elem : Tree<'a>) -> acc.InsertNode elem) Empty value ]
                        else value) grouped
                Node(value, res)
            | Node(value, _) when (n.Exists value) -> Node(n.Head.Value, t :: n.Tail)
            | Node(value, children) -> Node(value, List.map (Ins n) children)
            | Empty -> n
        Ins node this

    member this.Builder(data : 'a list list) =
        let mutable t = Empty
        let mutable data' = data
        let mutable data'' = []
        let mutable initDataLength = 0
        let mutable afterDataLength = 100
        while initDataLength <> afterDataLength do
            initDataLength <- data'.Length
            t <- List.fold (fun (acc : Tree<'a>) (x : 'a list) -> acc.Insert x) t data'
            let _, notInTree = List.partition (fun (x : 'a list) -> t.ExistsPair x) data'
            afterDataLength <- notInTree.Length
            data' <- notInTree
        (t, data')

    member this.DepthTree() =
        let rec Process value data = Node(value, List.map (fun (x : Tree<'a>) -> Process (value + 1) x) data.Tail)
        Process 0 this

    member this.PathTo(target : 'a) =
        let rec PathFinder (target : 'a) (tree : Tree<'a>) =
            match tree with
            | Node(value, _) when value = target -> Node(value, [])
            | Node(value, children) ->
                Node
                    (value,
                     [ List.fold (fun (acc : Tree<'a>) x -> PathFinder target x) Empty
                           (List.filter (fun (x : Tree<'a>) -> x.Exists target) children) ])
        PathFinder target this

    member this.Cut (l : 'a) (r : 'a) =
        if not ((this.Exists r) && (this.Exists r)) then
            failwith "One or both target nodes not found in tree"
        else
            let rec Cutter (acc : Tree<'a>) (src : 'a * 'a) =
                List.fold (fun (acc : Tree<'a>) (x : Tree<'a>) ->
                    let i, j = src
                    let leftMatch :: _ = List.filter (fun (x : Tree<'a>) -> x.Exists i) acc.Tail
                    let rightMatch :: _ = List.filter (fun (x : Tree<'a>) -> x.Exists j) acc.Tail
                    if (leftMatch = rightMatch) then Cutter leftMatch (i, j) else acc) acc acc.Tail
            Cutter this (l, r)

// TODO member this.SelectByName() = ()
// TODO member this.SelectByChild() = ()

let Split(orbits : string) =
    let words = orbits.Split(')')
    words |> Array.toList

let rec PathSum (acc : int) (tree : Tree<int>) =
    List.fold (fun (acc : int) (x : Tree<'a>) -> PathSum (acc + x.Head.Value) x) acc tree.Tail
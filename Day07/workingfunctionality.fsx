let rec Distribute e =
    function
    | [] -> [ [ e ] ]
    | x :: xs' as xs ->
        (e :: xs) :: [ for xs in Distribute e xs' -> x :: xs ]

let rec Permute =
    function
    | [] -> [ [] ]
    | e :: xs -> List.collect (Distribute e) (Permute xs)

type Status =
    | Available
    | Halted
    | Error

type Emitter(i : int list) =
    let mutable content = i

    member this.Next() =
        match content with
        | x :: y ->
            content <- y
            printfn "--> Emitter -> %A (%A)" x y
            Some x
        | [] -> None

    member this.Clear() = content <- []
    member this.Put value = content <- content @ [ value ]

    member this.Push value = new Emitter(content @ [ value ])

    member this.Remains = content
    static member (++) (a : Emitter, b : Emitter) = new Emitter(a.Remains @ b.Remains)

type Amplifier(ident : string, program : int [], phaseshift : int) =
    let mutable _status = Available
    let inner = Array.copy program
    let mutable emit = Emitter [ phaseshift ]
    let mutable _lastvalue = 0
    let mutable _position = 0

    let rec Evaluate (start : int) (lst : int []) =
        _position <- start
        if (start + 4 < lst.Length)
        then printfn "%2i =     %A" start (lst.[start..(start + 4)])
        else printfn "%2i =     %A" start (lst.[start..(lst.Length - 1)])

        // This function returns size of a digit
        let size (x : int) =
            let mutable len = 0
            let mutable tmp = 1

            while (tmp < x) do
                len <- len + 1
                tmp <- (tmp <<< 3) + (tmp <<< 1)
            len

        // This function returns a list of digits in a number
        let rec digits (x : int) (z : int) (acc : int list) =
            match z with
            | z when z > 1 -> digits (x % pown 10 (z - 1)) (z - 1) (acc @ [ x / pown 10 (z - 1) ])
            | z -> acc @ [ x ]

        // Wrapper for `digits` and `size` functions
        let split x = digits x (size x) []

        let s = List.rev (split lst.[start])

        // Eсли флаг нулевой, то возвращаем значение ячейки, на которое
        // указывает значение в ячейке. в противном случае просто возвращаем
        // значение ячейки
        let get (position : int) (flag : int) =
            match flag with
            | 0 -> lst.[lst.[position]]
            | 1 -> lst.[position]
            | _ -> -1

        match s with
        | [ 1; _; c; d ] ->
            let param1 = get (start + 1) c
            let param2 = get (start + 2) d
            let result = param1 + param2
            let target = lst.[start + 3]
            lst.[target] <- result
            printfn "%2i | ADD: %A + %A -> #%A = %A" start param1 param2 target result
            Evaluate (start + 4) lst
        | [ 1; _; c ] ->
            let param1 = get (start + 1) c
            let param2 = get (start + 2) 0
            let result = param1 + param2
            let target = lst.[start + 3]
            lst.[target] <- result
            printfn "%2i | ADD: %A + %A -> #%A = %A" start param1 param2 target result
            Evaluate (start + 4) lst
        | [ 1 ] ->
            let param1 = get (start + 1) 0
            let param2 = get (start + 2) 0
            let result = param1 + param2
            let target = lst.[start + 3]
            lst.[target] <- result
            printfn "%2i | ADD: %A + %A -> #%A = %A" start param1 param2 target result
            Evaluate (start + 4) lst
        | [ 2; _; c; d ] ->
            let param1 = get (start + 1) c
            let param2 = get (start + 2) d
            let result = param1 * param2
            let target = lst.[start + 3]
            lst.[target] <- result
            printfn "%2i | MULT: %A * %A -> #%A = %A" start param1 param2 target result
            Evaluate (start + 4) lst
        | [ 2; _; c ] ->
            let param1 = get (start + 1) c
            let param2 = get (start + 2) 0
            let result = param1 * param2
            let target = lst.[start + 3]
            lst.[target] <- result
            printfn "%2i | MULT: %A * %A -> #%A = %A" start param1 param2 target result
            Evaluate (start + 4) lst
        | [ 2 ] ->
            let param1 = get (start + 1) 0
            let param2 = get (start + 2) 0
            let result = param1 * param2
            let target = lst.[start + 3]
            lst.[target] <- result
            printfn "%2i | MULT: %A * %A -> #%A = %A" start param1 param2 target result
            Evaluate (start + 4) lst
        | [ 3 ] ->
            let target = lst.[start + 1]
            let i = emit.Next()
            match i with
            | Some x -> lst.[target] <- x
            | None -> ()
            printfn "%2i | PUT⁰: -> #%A = %A" start target (lst.[target])
            Evaluate (start + 2) lst
        | [ 4; _; c ] ->
            let target = get (start + 1) c
            _lastvalue <- target
            emit.Put target
            printfn "%2i | POP¹: -> #%A = %A" start (lst.[start + 1]) target
            _lastvalue
        | [ 4 ] ->
            let target = get (start + 1) 0
            _lastvalue <- target
            emit.Put target
            printfn "%2i | POP⁰: -> #%A = %A" start (lst.[start + 1]) target
            _position <- start + 2
            _lastvalue
        | [ 5; _; c; d ] ->
            let param1 = get (start + 1) c
            let param2 = get (start + 2) d
            printfn "   | i₁ = %A" param1
            printfn "   | i₂ = %A" param2
            if (param1 <> 0) then
                printfn "%2i | JMP²: %A != 0 -> goto #%A" start param1 param2
                Evaluate param2 lst
            else
                Evaluate (start + 3) lst
        | [ 5; _; c ] ->
            let param1 = get (start + 1) c
            let param2 = get (start + 2) 0
            printfn "   | i₁ = %A" param1
            printfn "   | i₂ = %A" param2
            if (param1 <> 0) then
                printfn "%2i | JMP¹: %A != 0 -> goto #%A" start param1 param2
                Evaluate param2 lst
            else
                Evaluate (start + 3) lst
        | [ 5 ] ->
            let param1 = get (start + 1) 0
            let param2 = get (start + 2) 0
            printfn "   | i₁ = %A" param1
            printfn "   | i₂ = %A" param2
            if (param1 <> 0) then
                printfn "%2i | JMP⁰: %A != 0 -> goto #%A" start param1 param2
                Evaluate param2 lst
            else
                Evaluate (start + 3) lst
        | [ 6; _; c; d ] ->
            let param1 = get (start + 1) c
            let param2 = get (start + 2) d
            if (param1 = 0) then
                printfn "%2i | JMP: %A = 0 -> goto #%A" start param1 param2
                Evaluate param2 lst
            else
                Evaluate (start + 3) lst
        | [ 6; _; c ] ->
            let param1 = get (start + 1) c
            let param2 = get (start + 2) 0
            if (param1 = 0) then
                printfn "%2i | JMP: %A = 0 -> goto #%A" start param1 param2
                Evaluate param2 lst
            else
                Evaluate (start + 3) lst
        | [ 6 ] ->
            let param1 = get (start + 1) 0
            let param2 = get (start + 2) 0
            if (param1 = 0) then
                printfn "%2i | JMP: %A = 0 -> goto #%A" start param1 param2
                Evaluate param2 lst
            else
                Evaluate (start + 3) lst
        | [ 7; _; c; d ] ->
            if (get (start + 1) c) < (get (start + 2) d) then
                lst.[lst.[start + 3]] <- 1
                Evaluate (start + 4) lst
            else
                lst.[lst.[start + 3]] <- 0
                Evaluate (start + 4) lst
        | [ 7; _; c ] ->
            if (get (start + 1) c) < (get (start + 2) 0) then
                lst.[lst.[start + 3]] <- 1
                Evaluate (start + 4) lst
            else
                lst.[lst.[start + 3]] <- 0
                Evaluate (start + 4) lst
        | [ 7 ] ->
            if (get (start + 1) 0) < (get (start + 2) 0) then
                lst.[lst.[start + 3]] <- 1
                Evaluate (start + 4) lst
            else
                lst.[lst.[start + 3]] <- 0
                Evaluate (start + 4) lst
        | [ 8; _; c; d ] ->
            if (get (start + 1) c) = (get (start + 2) d) then
                lst.[lst.[start + 3]] <- 1
                Evaluate (start + 4) lst
            else
                lst.[lst.[start + 3]] <- 0
                Evaluate (start + 4) lst
        | [ 8; _; c ] ->
            if (get (start + 1) c) = (get (start + 2) 0) then
                lst.[lst.[start + 3]] <- 1
                Evaluate (start + 4) lst
            else
                lst.[lst.[start + 3]] <- 0
                Evaluate (start + 4) lst
        | [ 8 ] ->
            if (get (start + 1) 0) = (get (start + 2) 0) then
                lst.[lst.[start + 3]] <- 1
                Evaluate (start + 4) lst
            else
                lst.[lst.[start + 3]] <- 0
                Evaluate (start + 4) lst
        | [ 9; 9 ] ->
            _status <- Halted
            printfn "System %A halted with last value of %A" ident _lastvalue
            printfn "Emitter status: %A" (emit.Remains)
            99
        | z ->
            _status <- Error
            printfn "System %A error with %A" ident z
            -99

    member this.Status = _status

    member this.Run(input : Emitter) =
        printfn "AMP %A <- %A" ident (input.Remains)
        match _status with
        | Available ->
            let init = Emitter [ phaseshift ]
            if _position = 0 then emit <- init ++ input else emit <- input
            Evaluate _position inner |> ignore
            emit
        | Halted -> input
        | Error -> Emitter []


let AmpChain (x : int list) (program : int []) =

    let counter = Emitter x

    let a1 = Amplifier ("A1", program)
    let a2 = Amplifier ("A2", program)
    let a3 = Amplifier ("A3", program)
    let a4 = Amplifier ("A4", program)
    let a5 = Amplifier ("A5", program)

    a1.Run(counter.Put 0)
    |> a2.Run
    |> a3.Run
    |> a4.Run
    |> a5.Run

let AmpChainFeedback (x : int list) (program : int []) =

    let mutable counter = Emitter x
    let mutable initial = 0
    let mutable t = Emitter []

    let a1 = Amplifier("A1f", program, (counter.Next()).Value)
    let a2 = Amplifier("A2f", program, (counter.Next()).Value)
    let a3 = Amplifier("A3f", program, (counter.Next()).Value)
    let a4 = Amplifier("A4f", program, (counter.Next()).Value)
    let a5 = Amplifier("A5f", program, (counter.Next()).Value)

    while (a5.Status = Available) do
        t <-
            (a1.Run(Emitter [ initial ])
             |> a2.Run
             |> a3.Run
             |> a4.Run
             |> a5.Run)
        printfn "t:= %A" (t.Remains)
        match t.Remains with
        | x :: _ -> initial <- x
        | _ -> initial <- 0

    t

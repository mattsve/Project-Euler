namespace Euler

module Problem14 =

    let sequences = Dict.empty()
    
    let rec dictSequence n =
        match n with 
        | 1L -> [1L]
        | _  ->
            match Dict.tryFind n sequences with
            | Some (s) -> s
            | None ->
                let newN =
                    if (n % 2L = 0L) then
                        n / 2L
                    else
                        3L * n + 1L
                let s = n::dictSequence newN
                Dict.add n s sequences |> ignore
                s
    
    let sequence n =
        let generator num =
            match num with
            | 0L                   -> None
            | 1L                   -> Some (1L, 0L)
            | _ when num % 2L = 0L -> let newNum = num / 2L
                                      Some (num, newNum)
            | _                    -> let newNum = 3L * num + 1L
                                      Some (num, newNum)
        Seq.unfold generator n
    
    let numbers = [for i in 1L .. 999999L do yield (i, Seq.length (dictSequence i))]
             
    let result =
        numbers |> List.maxBy snd |> fst
        
    let printResult =
        printfn "Problem 14: %A" result
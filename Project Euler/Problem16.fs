namespace Euler

module Problem16 =
    let digits = 
        bigint.Pow (2I, 1000) |> string |> String.toChars

    let result =
        List.map (fun x -> bigint.Parse(x.ToString())) digits |>
        List.fold (+) 0I

    let printResult =
        printfn "Problem 16: %A" result


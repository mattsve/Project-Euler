namespace Euler

module Problem17 =
    
    let toString a =
        let rec internalToString n =
            let toBase m =
                match m with
                | 1 -> "One"
                | 2 -> "Two"
                | 3 -> "Three"
                | 4 -> "Four"
                | 5 -> "Five"
                | 6 -> "Six"
                | 7 -> "Seven"
                | 8 -> "Eight"
                | 9 -> "Nine"
                | 10 -> "Ten"
                | 11 -> "Eleven"
                | 12 -> "Twelve"
                | 13 -> "Thirteen"
                | 14 -> "Fourteen"
                | 15 -> "Fifteen"
                | 16 -> "Sixteen"
                | 17 -> "Seventeen"
                | 18 -> "Eighteen"
                | 19 -> "Nineteen"
                | 20 -> "Twenty"
                | 30 -> "Thirty"
                | 40 -> "Fourty"
                | 50 -> "Fifty"
                | 60 -> "Sixty"
                | 70 -> "Seventy"
                | 80 -> "Eighty"
                | 90 -> "Ninety"
                | 100 -> "Hundred"
                | 1000 -> "Thousand"
                | _  -> ""
            let baseStrings = 
                if n / 1000 > 0 then
                    toBase (n / 1000) :: toBase 1000 :: internalToString (n % 1000)
                elif n / 100 > 0 then
                    toBase (n / 100) :: toBase 100 :: internalToString (n % 100)
                elif n > 19 then
                    toBase ((n / 10) * 10) :: internalToString (n % 10)
                else
                    [toBase n]
            baseStrings
        let reversedResult =
            let rec anding str =
                match str with
                | ""::tail     -> anding tail
                | [s1]         -> [s1]
                | [s1; s2]     -> [s1; s2]
                | [s1; s2; s3] -> [s1; "and"; s2; s3]
                | s1::s2::tail -> s1::s2::"and"::tail
                | _            -> []
            anding (List.rev (internalToString a))
        List.rev reversedResult


    let result =
        [for i in 1..1000 do yield toString i] |>
        List.concat |>
        List.map (fun (x : string) -> x.Length) |>
        List.fold (+) 0


    let printResult =
        printfn "Problem 17: %A" result
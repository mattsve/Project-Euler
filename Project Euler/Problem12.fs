namespace Euler

module Problem12 =
    
    let triangleNumbers =
        Seq.unfold (fun (t, n) -> Some (t + n, (t + n, n + 1L))) (0L, 1L) 

    let findFactors n =
        let rec inner fact num =
            if fact = 1L then [1L]
            elif num % fact = 0L then fact :: num / fact :: (inner (fact - 1L) num)
            else (inner (fact - 1L) num)
        inner (int64 (sqrt (float n))) n |>
        Seq.distinct

    let result =
        triangleNumbers |>
        Seq.find (fun x -> Seq.length (findFactors x) > 500)

    

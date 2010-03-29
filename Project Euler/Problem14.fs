namespace Euler

module Problem14 =
    
    let rec sequence =
        let f n =
            match n with
            | 1L -> [1L]
            | _  -> let newN =
                        if (n % 2L = 0L) then
                            n / 2L
                        else
                            3L * n + 1L
                    n::sequence newN
        f |> Memoization.memoize
            
    let numbers = [for i in 1L .. 999999L do yield (i, List.length (sequence i))]
             
    let result =
        numbers |> List.maxBy snd |> fst
        
    
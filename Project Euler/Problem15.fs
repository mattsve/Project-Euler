namespace Euler

module Problem15 =
    
    let rec numberOfRoutes =
        let f (x, y) =
            match (x, y) with
            | _ when x = 0 || y = 0 -> 1L
            | _                     -> numberOfRoutes (x - 1, y) +
                                       numberOfRoutes (x, y - 1)
        f |> Memoization.memoize

    let result =
        numberOfRoutes (20, 20)
        
    
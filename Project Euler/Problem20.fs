namespace Euler

module Problem20 =

    let rec calculate n =
        match n with
        | _ when n = 1I  -> 1I
        | _              -> n * (calculate (n - 1I))

    let result =
        calculate 100I |>
        Math.digits |>
        List.fold (+) 0I    
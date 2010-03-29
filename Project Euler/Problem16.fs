namespace Euler

module Problem16 =
    let result =
        bigint.Pow (2I, 1000) |> 
        Math.digits |>
        List.fold (+) 0I

    
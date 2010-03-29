namespace Euler

module String =
    let toChars (s : string) =
        s.ToCharArray()  |> Array.toList

module Char =
    let toNumber (c : char) =
        int c - int '0'

module Math =

    let rec digits n =
        match n with
        | _ when n < 10I -> [n]
        | _              -> digits (n / 10I) @ digits (n % 10I)

module Dict =
    open System.Collections.Generic

    let empty() = new Dictionary<_, _>()

    let add key value (dict : #IDictionary<'a, 'b>) =
        dict.[key] <- value; dict

    let remove (key : 'a) (dict : #IDictionary<'a, 'b>) =
        dict.Remove(key) |> ignore; dict

    let tryFind key (dict : #IDictionary<'a, 'b>) =
        match dict.TryGetValue(key) with
        | true, value -> Some (value)
        | false, _    -> None
        
module Memoization =
    let memoize (func : 'a->'b) =
        let cache = Dict.empty()
        fun n ->
            match Dict.tryFind n cache with
            | Some (result) -> result
            | None          -> let result = func n
                               Dict.add n result cache |> ignore
                               result
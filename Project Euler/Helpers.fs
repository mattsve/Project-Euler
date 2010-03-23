namespace Euler

module String =
    let toChars (s : string) =
        s.ToCharArray()  |> Array.toList

module Char =
    let toNumber (c : char) =
        int c - int '0'

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
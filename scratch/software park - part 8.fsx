open System
open System.Text.RegularExpressions

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|IsValidEmail|_|) input =
    match input with
    | ParseRegex ".*?@(.*)" [ _ ]-> Some input
    | _ -> None

let checkEmail email =
    match email with
    | IsValidEmail _ -> Ok email
    | _ -> Error "Not a valid email"

let result = checkEmail "bob@bob.com"
printfn "Result: %A" result
let result' = checkEmail "bobbob"
printfn "Result: %A" result'

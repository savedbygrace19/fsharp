open System

let tryParseDateTime (str : string) =
    match DateTime.TryParse str with
    | true, result -> Some result
    | _ -> None

let nullObj:string = null
let nullPri = Nullable<int>()

let fromNullObj = Option.ofObj nullObj
let fromNullPri = Option.ofNullable nullPri

let toNullObj = Option.toObj fromNullObj
let toNullPri = Option.toNullable fromNullPri

let result = Option.defaultValue "------" fromNullObj
let unknown = Option.defaultValue "defaultValue"
let result2 = unknown fromNullObj

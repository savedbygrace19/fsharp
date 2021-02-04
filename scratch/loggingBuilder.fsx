type LoggingBuilder() =
    let log p = printfn "expression is %A" p

    member this.Bind(x, f) = 
        log x
        f x

    member this.Return(x) = 
        x

let logger = LoggingBuilder()

let loggedWorkflow =
    logger {
        let! x = 42
        let! y = 43
        let! z = x + y
        return z
    }

let divideBy bottom top =
    if bottom = 0 then
        None
    else 
        Some(top / bottom)

type MaybeBuilder() =
    member this.Bind(x, f) = 
        match x with
        | None -> None
        | Some a -> f a

    member this.Return(x) = 
        Some x
   
let maybe = MaybeBuilder()

let divideByWorkflow init x y z = 
    maybe {
        let! a = init |> divideBy x
        let! b = a |> divideBy y
        let! c = b |> divideBy z
        return c
    }    

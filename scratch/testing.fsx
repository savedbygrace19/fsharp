type BankAccount =
    | Checking
    | Savings

(*
    bind takes a switch function (one that normally takes a single input and returns a success/failure
    type such as Result) and lifts it to a two-track function.
*)
let bind switchFn twoTrackInput =
    match twoTrackInput with
    | Ok success -> switchFn success
    | Error failure -> Error failure

(*
    Map, then, takes a single-track function (one that neither accepts nor returns a success/failure
    type such as Result) and lifts it to a two-track function.
*)
let map singleTrackFn twoTrackInput =
    match twoTrackInput with
    | Ok success -> Ok (singleTrackFn success)
    | Error failure -> Error failure

let printThings x y =
    printfn "x=%i y=%i" x y

let printTwoParameters x  =
    let subFunction y = 
        printfn "%A %A" x y
    subFunction               

let printXBakedIn = printThings 1
printXBakedIn 2

let x = 6

// create a function
let printHello() = printfn "hello"

let pipeInto (someExpression,lambda) =
   match someExpression with
    | None -> 
        None
    | Some x -> 
        x |> lambda

module strAddMod =
    let strToInt (str : string) =
        printfn ("%A") str
        str |> int

    type SomethingBuilder() =
        member this.Bind(m, f) = f m
        member this.Return(a) = a

    let somethingBuilt = SomethingBuilder()

    let stringAddWorkflow x y z = 
        somethingBuilt {
            let! a = strToInt x
            printfn "a = %A" a
            let! b = strToInt y
            let! c = strToInt z
            return a + b + c
        }    

    let stringAddWorkflow2 x y z =
        let a = strToInt x
        let b = strToInt y
        let c = strToInt z
        a + b + c

    let strAdd (str : string) i =
        str |> int |> (+) i

    let (>>=) m f = f m

    let good = strToInt "1" >>= strAdd "2" >>= strAdd "3"

let log p = printfn "expression is %A" p

let loggedWorkflow = 
    let x = 42
    log x
    let y = 43
    log y
    let z = x + y
    log z
    //return
    z

type LoggingBuilder() =
    let log p = printfn "expression is %A" p

    member this.Bind(x, f) = 
        log x
        f x

    member this.Return(x) = 
        x

let logger = LoggingBuilder()

let loggedWorkflow2 = 
    logger
        {
        let! x = 42
        let! y = 43
        let! z = x + y
        return z
        }

module DbResultWorkflow =
    type DbResult<'a> = 
        | Success of 'a
        | Error of string

    let getCustomerId name =
        if (name = "") 
        then Error "getCustomerId failed"
        else Success "Cust42"

    let getLastOrderForCustomer custId =
        if (custId = "") 
        then Error "getLastOrderForCustomer failed"
        else Success "Order123"

    let getLastProductForOrder orderId =
        if (orderId  = "") 
        then Error "getLastProductForOrder failed"
        else Success "Product456"

    type DbResultBuilder() =
        member this.Bind(m,f) = 
            match m with
            | Success success -> 
                printfn "\tSuccessful: %A" success
                f success
            | Error _ -> m

        member this.Return(x) = Success x

    let dbResultBuilder = DbResultBuilder()

    let product =
        dbResultBuilder {
            let! id = getCustomerId "Alice"
            let! lastOrder = getLastOrderForCustomer id
            let! lastProduct = getLastProductForOrder lastOrder
            return lastProduct
        }

    let (>>=) m f =
        match m with
        | Success success -> 
            printfn "\tSuccessful: %A" success
            f success
        | Error _ -> m

    let product2 = getCustomerId "Alice" >>= getLastOrderForCustomer >>= getLastProductForOrder

module DbWorkflow2 =
    type DbResult<'a> = 
        | Success of 'a
        | Error of string

    type CustomerId =  CustomerId of string
    type OrderId =  OrderId of int
    type ProductId =  ProductId of string

    let getCustomerId name =
        if (name = "") 
        then Error "getCustomerId failed"
        else Success (CustomerId "Cust42")

    let getLastOrderForCustomer (CustomerId custId) =
        if (custId = "") 
        then Error "getLastOrderForCustomer failed"
        else Success (OrderId 123)

    let getLastProductForOrder (OrderId orderId) =
        if (orderId  = 0) 
        then Error "getLastProductForOrder failed"
        else Success (ProductId "Product456")

    type DbResultBuilder() =
        member this.Bind(m, f) = 
            match m with
            | Error e -> Error e
            | Success a -> 
                printfn "\tSuccessful: %A" a
                f a

        member this.Return(x) = Success x

    let dbresult = DbResultBuilder()

    let product = 
        dbresult {
            let! custId = getCustomerId "Alice"
            let! orderId = getLastOrderForCustomer custId
            let! productId = getLastProductForOrder orderId 
            printfn "Product is %A" productId
            return productId
        }

module TraceExample =
    type TraceBuilder() =
        member __.Bind(m, f) = 
            match m with 
            | None -> 
                printfn "Binding with None. Exiting."
            | Some a -> 
                printfn "Binding with Some(%A). Continuing" a
            Option.bind f m

        member __.Return(x) = 
            printfn "Returning a unwrapped %A as an option" x
            Some x

        member __.ReturnFrom(m) = 
            printfn "Returning an option (%A) directly" m
            m

        member __.Yield(x) = 
            printfn "Yield an unwrapped %A as an option" x
            Some x

        member __.YieldFrom(m) = 
            printfn "Yield an option (%A) directly" m
            m

    // make an instance of the workflow 
    let trace = TraceBuilder()

    trace { 
        return 1
        } |> printfn "Result 1: %A" 

    trace { 
        return! Some 2
        } |> printfn "Result 2: %A" 

    trace { 
        let! x = Some 1
        printfn "x is %A" x
        let turnip = Some 1
        printfn "turnip is %A" turnip
        let! y = Some 2
        return x + y
        } |> printfn "Result 3: %A" 

    trace { 
        let! x = None
        let! y = Some 1
        return x + y
        } |> printfn "Result 4: %A" 

    trace { 
        do! Some (printfn "...expression that returns unit")
        do! Some (printfn "...another expression that returns unit")
        let! x = Some (1)
        return x
        } |> printfn "Result from do: %A" 

    trace { 
        yield 1
    } |> printfn "Result for yield: %A" 
    
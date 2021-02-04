open System

let tryDivide numerator denominator =
    match denominator with
    | 0 -> failwith "can't divide by zero"
    | _ -> numerator / denominator

let tryDivide2 (x:decimal) (y:decimal) = // decimal -> decimal -> decimal
    try
        x/y 
    with
    | :? DivideByZeroException as ex -> raise ex

let tryDivide3 numerator denominator =
    match denominator with
    | 0 -> Error "can't divide by zero"
    | _ -> Ok (numerator / denominator)

let tryDivide4 (x:decimal) (y:decimal) = // decimal -> decimal -> Result<decimal,exn>
    try
        Ok (x/y) 
    with
    | :? DivideByZeroException as ex -> Error ex

type Customer = {
    Id : int
    IsVip : bool
    Credit : decimal
}

let getPurchases customer = // Customer -> Result<(Customer * decimal),exn>
    try
        // Imagine this function is fetching data from a Database
        let purchases = if customer.Id % 2 = 0 then (customer, 120M) else (customer, 80M)
        Ok purchases
    with
    | ex -> Error ex

let tryPromoteToVip purchases = // Customer * decimal -> Customer
    let customer, amount = purchases
    if amount > 100M then { customer with IsVip = true }
    else customer

let increaseCreditIfVip customer = // Customer -> Result<Customer,exn>
    try
        // Imagine this function could cause an exception            
        let result = 
            if customer.IsVip then { customer with Credit = customer.Credit + 100M }
            else { customer with Credit = customer.Credit + 50M }
        Ok result
    with
    | ex -> Error ex

let upgradeCustomer customer =
    customer 
    |> getPurchases 
    |> Result.map tryPromoteToVip // Problem
    |> Result.bind increaseCreditIfVip

let customerVIP = { Id = 1; IsVip = true; Credit = 0.0M }
let customerSTD = { Id = 2; IsVip = false; Credit = 100.0M }

let assertVIP = upgradeCustomer customerVIP = Ok {Id = 1; IsVip = true; Credit = 100.0M }
let assertSTDtoVIP = upgradeCustomer customerSTD = Ok {Id = 2; IsVip = true; Credit = 200.0M }
let assertSTD = upgradeCustomer { customerSTD with Id = 3; Credit = 50.0M } = Ok {Id = 3; IsVip = false; Credit = 100.0M }

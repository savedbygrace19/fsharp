type UnitQuantity = private UnitQuantity of int

module UnitQuantity =
  let create qty = 
    if qty < 1 then
      Error "UQ cannot be negative"
    else if qty > 1000 then
      Error "UQ cannot be more than 1000"
    else
      Ok (UnitQuantity qty)

  let value (UnitQuantity qty) = qty
 
let unitQtyResult = UnitQuantity.create 1
match unitQtyResult with
| Error msg -> printfn "Failure, Message is %s" msg
| Ok uQty ->
  printfn "Success.Value is %A" uQty
  let innerValue = UnitQuantity.value uQty
  printfn "innerValue is %i" innerValue

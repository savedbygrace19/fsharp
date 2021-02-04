// type CustomerId =
//  | CustomerId of int

// The above can be written as
// type CustomerId = CustomerId of int
//
// The first CustomerId is the type, and the second is the case label
type CustomerId = CustomerId of int
type OrderId = OrderId of int

let customerId = CustomerId 42
let orderId = OrderId 42

// printfn "%b" (orderId = customerId) // This will generate a compiler error for the type mismatch

let (CustomerId innerValue) = customerId

let processCustomerId (CustomerId innerValue) = 
  printfn "innerValue is %i" innerValue

// This translates to that
// data Order =
//   CustomerInfo
//   AND ShippingAddress
//   AND BillingAddress
//   AND list of OrderLines
//   AND AmountToBill
type Order = {
    CustomerInfo: CustomerInfo
    ShippingAddress: ShippingAddress
    BillingAddress: BillingAddress
    OrderLines: OrderLine list
    AmountToBill: BillingAmount
}

// Note how everything above is effectively undefined, awaiting
// further definition. Represent the undefined types as follows, allowing
// things to compile as you create the model, but forcing you to resolve
// things before running.
type Undefined = exn // F#'s exception type

type CustomerInfo = Undefined
type ShippingAddress = Undefined
type BillingAddress = Undefined
type OrderLine = Undefined
type BillingAmount = Undefined

type Order = {
    CustomerInfo: CustomerInfo
    ShippingAddress: ShippingAddress
    BillingAddress: BillingAddress
    OrderLines: OrderLine list
    AmountToBill: BillingAmount
}

// Modeling choice types:
// data ProductCode =
//   WidgetCode
//   OR GizmoCode
type ProductCode =
  | Widget of WidgetCode
  | Gizmo of GizmoCode


// Working with complex inputs and outputs
// If a workflow returns multiple outputs (AND), create a record type to store them. For example:
type PlaceOrderEvents = {
    AcknowledgementSent : AcknowledgementSent
    OrderPlaced : OrderPlaced
    BillableOrderPlaced : BillableOrderPlaced
}

// The workflow, then, looks like this:
// "PlaceOrder takes an UnvalidatedOrder and returns PlaceOrderEvents"
type PlaceOrder = UnvalidatedOrder -> PlaceOrderEvents

// Conversely, if a workflow returns one of many possibilities, you create
// a choice type to store them:
// workflow "Categorize inbound mail" =
//   input: Envelope contents                    (EnvelopeContents below)
//   output:                                     (CategorizedMail below)
//     QuoteForm (put on appropriate pile)
//     OR OrderForm (put on appropriate pile)
//     OR ...
type EnvelopeContents = EnvelopeContents of string
type CategorizedMail =
  | Quote of QuoteForm
  | Order of OrderForm
  // etc
 
 type CategorizeInboundMail = EnvelopeContents -> CategorizedMail
 
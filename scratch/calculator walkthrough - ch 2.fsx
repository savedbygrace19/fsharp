type Number = float

type CalculatorDigit =
    | Zero | One | Two | Three | Four | Five | Six | Seven
    | Eight | Nine | DecimalSeparator

type CalculatorMathOp =
    | Add | Subtract | Multiply | Divide

type CalculatorAction =
    | Equals | Clear

type CalculatorInput =
    | Digit of CalculatorDigit
    | Op of CalculatorMathOp
    | Action of CalculatorAction

type CalculatorDisplay = string

type CalculatorState = {
    Display: CalculatorDisplay
    PendingOp: (CalculatorMathOp * Number) option
}

type Calculate = CalculatorInput * CalculatorState -> CalculatorState

type UpdateDisplayFromDigit = CalculatorDigit * CalculatorDisplay -> CalculatorDisplay

type MathOperationError =
    | DivideByZero

type MathOperationResult =
    | Success of Number
    | Failure of MathOperationError

type DoMathOperation = CalculatorMathOp * Number * Number -> MathOperationResult

type GetDisplayNumber = CalculatorDisplay -> Number option

type SetDisplayNumber = Number -> CalculatorDisplay

type InitState = unit -> CalculatorState

type SetDisplayError = MathOperationError -> CalculatorDisplay 

type CalculatorServices = {
    UpdateDisplayFromDigit: UpdateDisplayFromDigit
    DoMathOperation: DoMathOperation
    GetDisplayNumber: GetDisplayNumber
    SetDisplayNumber: SetDisplayNumber 
    SetDisplayError: SetDisplayError 
    InitState: InitState 
}

type MaybeBuilder() =
    member this.Bind(x, f) = Option.bind f x
    member this.Return(x) = Some x

let maybe = MaybeBuilder()

let ifNone defaultValue input = 
    // just reverse the parameters!
    defaultArg input defaultValue 

//------------------------------------------------------------------------------

module CalculatorImplementation = 
    let updateDisplayFromDigit services digit state =
        let newDisplay = services.UpdateDisplayFromDigit (digit, state.Display)
        let newState = {state with Display=newDisplay}
        newState //return

    let updateDisplayFromPendingOp services state =
        // helper to do the math op
        let doMathOp (op, pendingNumber, currentNumber) = 
            let result = services.DoMathOperation (op, pendingNumber, currentNumber)
            let newDisplay = 
                match result with
                | Success resultNumber ->
                    services.SetDisplayNumber resultNumber 
                | Failure error -> 
                    services.SetDisplayError error
            { state with Display = newDisplay; PendingOp = None }
            
        // fetch the two options and combine them
        let newState = maybe {
            let! (op,pendingNumber) = state.PendingOp
            let! currentNumber = services.GetDisplayNumber state.Display
            return doMathOp (op,pendingNumber,currentNumber)
        }
        newState |> ifNone state

    let addPendingMathOp services op state = 
        maybe {            
            let! currentNumber = 
                state.Display
                |> services.GetDisplayNumber 
            let pendingOp = Some (op, currentNumber)
            return {state with PendingOp = pendingOp}
        }
        |> ifNone state // return original state if anything fails

    let createCalculate (services:CalculatorServices) :Calculate = 
        fun (input,state) -> 
            match input with
            | Digit d ->
                updateDisplayFromDigit services d state
            | Op op ->
                let newState = updateDisplayFromPendingOp services state
                addPendingMathOp services op newState
            | Action Clear ->
                services.InitState()
            | Action Equals ->
                updateDisplayFromPendingOp services state


// create the services
let services = CalculatorServices.create()

// inject the services into the "factory" method
let calculate = CalculatorImplementation.createCalculate services


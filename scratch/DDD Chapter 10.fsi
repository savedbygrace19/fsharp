// Picture switch functions as train tracks like so:
//
// ------------------------
//          \
//           --------------

// Picture single track functions a train tracks like so:
//
// ------------------------- (yup, no divergence for errors at all)

// Bind takes a switch function and a Result, and calls the fn if the Result was successful
// or returns the error if the Result had failed.
let bind switchFn twoTrackInput =
  match twoTrackInput with
  | Ok success -> switchFn success
  | Error failure -> Error failure

// Map takes a single track function (no accommodation for receiving or returning Result data) and
// a result, and calls the fn if the Result was successful, wrapping THAT result in a successful Result,
// or returns the error if the Result had failed.
let map singleTrackFn twoTrackInput =
    match twoTrackInput with
    | Ok success -> Ok (singleTrackFn success)
    | Error failure -> Error failure

type AddError = AddError of string
type SubtractError = SubtractError of int

type FruitError  =
    | AddErrorCase of AddError
    | SubtractErrorCase of SubtractError

// MapError takes a function (we know we'll be passing in a FruitError case)
// and a Result, mapping success to success and failure to a new instance
// created by invoking 'f'.
let mapError f aResult =
    match aResult with
    | Ok success -> Ok success
    | Error failure -> Error (f failure)

// Of note: we wrap an AddError in an Error for the purposes of this example.
let add x =
    match x with
    | 5 -> Ok 11
    | _ -> Error (AddError "Must be 5!")

// Of note: we wrap a SubtractError in an Error for the purposes of this example.
let subtract x =
    match x with
    | 10 -> Ok 5
    | _ -> Error (SubtractError 22)

// These next two functions translate/map AddErrors and SubtractErrors respectively
// to their FruitError counterparts. Doing this allows us to deal with a single
// Result in the form of Result<int, FruitError>.
//
// This one passes the input to Add, takes the Result<int, AddError> and passes
// it to mapError, which, in the error case, assigns a new Error using the AddErrorCase
// constructor function.
let addWithFruitError = add >> mapError AddErrorCase
// This works just like the function above, only accommodating 'subtract'
let subtractWithFruitError = subtract >> mapError SubtractErrorCase

// Here's where we prove things work.
let combined = addWithFruitError >> bind subtractWithFruitError

// This was a wee helper to print whether the result
// was a success or failure.
let splitOut x =
    match x with
    | Ok success -> printfn "success!"
    | Error failure ->
        match failure with
        | AddErrorCase addFailure -> printfn "add failure!"
        | SubtractErrorCase subtractError -> printfn "sub failure!"
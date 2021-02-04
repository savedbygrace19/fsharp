let calculate mapping n =
    let isDivisibleBy (divisor, result) = if n % divisor = 0 then result else ""
    let handleEmpty input = if input = "" then n |> string else input

    mapping
    |> List.map isDivisibleBy
    |> List.fold (+) "" 
    |> handleEmpty

(* How this works:
   1. For every number 1 through 15
   2. Map that value to the second input to 'calculate'
      - First input is the list with fizz and buzz
   3. Call calculate
   4. Map each of the results from all of the calls to calculate to a new list,
      returned when map completes *)
[1..150] |> List.map (fun n -> ([(3, "Fizz"); (5, "Buzz")], n) ||> calculate)

(* Single point example to demonstrate how calculate is called

    calculate [(3, "Fizz"); (5, "Buzz")] 15
        mapping
        -> List.map iterates over the list 'mapping'
        -> isDivisibleBy (3, "Fizz") 15 // remember that '15' is captured by 'n' in the function definition
            -> returns "Fizz"
            -> returns "Buzz"
        -> List.map returns ["Fizz"; "Buzz"]
        -> List.fold takes the 'folder' (function), the starting/initial value (the empty string), and successively applies the folder
           across each element of the list, accumulating and returning the result once the list is empty.
        -> In this case, List.fold is called as 'List.fold (+) "" ["Fizz"; "Buzz"]'
        -> The result is a successive:
            (+) "" "Fizz" -> "Fizz"
            (+) "Fizz" "Buzz" -> "FizzBuzz"
        -> Then we call handleEmpty "FizzBuzz", which of course returns "FizzBuzz"
        -> If the input to 'handleEmpty' had been an empty string, then handleEmpty would return the input number (for instance, '4')
*)  

List.fold (+) 10 [1;2;3]
(*
    // the binary function is passed again as the folder in the first argument
    // the initial value is updated to add 0 1 = 1 in the second argument
    // the tail of the list (all elements except the first one) is passed in the third argument
    // it becomes this:
    List.fold (+) ((+) 0 1) [2; 3] 
    List.fold (+) 1 [2; 3]
    // Repeat untill the list is empty -> then return the "inital" value
    List.fold (+) ((+) 1 2) [3]
    List.fold (+) 3 [3] // add 1 2 = 3
    List.fold (+) ((+) 3 3) [] 
    List.fold (+) 6 [] // the list is empty now -> return 6
6
*)
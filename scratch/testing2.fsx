open System.Text.RegularExpressions

type ProductCode2 = private ProductCode2 of string

module ProductCode2 =
    let create inputStr =
        let validatingRegex = Regex(@"W\d+");
        if validatingRegex.IsMatch inputStr then
            Ok (ProductCode2 inputStr)
        else
            Error (sprintf "%s is not a valid ProductCode2." inputStr)
            
    let value (ProductCode2 innerData) = innerData

let kevin (x: ProductCode2) =
    printfn "%A" x

ProductCode2.create "W123" |> Result.map kevin



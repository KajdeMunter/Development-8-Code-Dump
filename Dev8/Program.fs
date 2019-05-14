open System

// %d integer (?)
// %i integer
// %b booleans
// %s string
// %f float
// %A Structures?
// %O Object

// Unit 1
let square x = x * x

let rec allNumber n =
    if n = 0 then "0"
    else sprintf "%s %i" (allNumber (n-1)) n

let rec allNumberRev n = 
    if n < 1 then "0"
    else sprintf "%i %s" n (allNumberRev (n-1))

let rec allNumberRange (min:int) (max:int) =
    if max = min then sprintf "%i" min
    else sprintf "%i %s" min (allNumberRange (min+1) max)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    printfn "%d squared is: %d" 12 (square 12)

    printfn "AllNumber Result: %A" (allNumber 10)

    printfn "AllNumberRev Result: %A" (allNumberRev 10)

    printfn "AllNumberRange between %i and %i: %s" 5 10 (allNumberRange 5 10) 

    0 // return an integer exit code

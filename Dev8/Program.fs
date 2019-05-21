open System

// %d integer (?)
// %i integer
// %b booleans
// %s string
// %f float
// %A Structures?
// %O Object

// -----UNIT 1-----
let square x = x * x
let (add: int -> int -> int) = fun (x:int) (y:int) -> x + y

let rec allNumber n =
    if n = 0 then "0"
    else sprintf "%s %i" (allNumber (n-1)) n

let rec allNumberRev n = 
    if n < 1 then "0"
    else sprintf "%i %s" n (allNumberRev (n-1))

let rec allNumberRange min max =
    if max = min then sprintf "%i" min
    else sprintf "%i %s" min (allNumberRange (min+1) max)

let rec allNumberRangeRev min max =
    if max = min then sprintf "%i" max
    else sprintf "%i %s" max (allNumberRangeRev min (max-1))

let rec allEvenRange min max =
    if max = min then sprintf "%i" min
    else if min % 2 = 0 then sprintf "%i %s" min (allEvenRange (min+1) max)
    else allEvenRange (min+1) max

let rec drawSymbols symbol length =
    if length < 1 then ""
    else symbol + drawSymbols symbol (length-1)


let rec toBinary n =
    if n < 1 then ""
    else sprintf "%s%i" (toBinary (n/2)) (n % 2)

let rec toBase n b =
    if n < 1 then ""
    else sprintf "%s%i" (toBase (n / b) b) (n % b)

// -----UNIT 2-----
let r = Random()
let nextDouble = r.NextDouble()

let randomFloatBetween min max = nextDouble * (max - min) + min

type Point2D = { Position: (float*float) }
    with 
        static member Create(x:float, y:float) : Point2D = { Position=(x, y) }
        static member CreateRandom(min, max) : Point2D = { Position=(randomFloatBetween min max, randomFloatBetween min max) }
        member this.getX : float = fst(this.Position)
        member this.getY : float = snd(this.Position)
        static member Distance (fst:Point2D) (snd:Point2D) = Math.Sqrt(((fst.getX - snd.getX) ** 2.0) + ((fst.getY - snd.getY) ** 2.0))

type Blob =
    {
        Position: Point2D
        Size: int
    }
    with
        static member Create : Blob = 
            {
                Position = Point2D.Create(randomFloatBetween -50.0 51.0, randomFloatBetween -50.0 51.0)
                Size = r.Next(1, 6)
            }
        

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#! \n"

    printfn "-----UNIT 1-----\n"

    printfn "%d squared is: %d" 12 (square 12)
    printfn "%i plus %i is: %i" 10 5 (add 10 5)
    printfn "allNumber: %A" (allNumber 10)
    printfn "allNumberRev: %A" (allNumberRev 10)
    printfn "allNumberRange between %i and %i: %s" 5 10 (allNumberRange 5 10)
    printfn "allNumberRangeRev between %i and %i: %s" 5 10 (allNumberRangeRev 5 10)
    printfn "allEvenRange between %i and %i: %s" 5 10 (allEvenRange 5 10)
    printfn "drawSymbols: %s" (drawSymbols "*" 5)
    printfn "%i in binary is: %s" 23 (toBinary 23)
    printfn "%i in base %i is: %s" 125 8 (toBase 125 8)

    0 // return an integer exit code

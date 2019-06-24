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
        

// -----UNIT 3-----
let rec last (l:List<'a>) =
    match l with
    | hd :: [] -> Some hd
    | _ :: tl -> last tl
    | _ -> None


let rec length (l:List<'a>) =
    match l with
    | [] -> 0
    | _ :: tl -> 1 + length tl

let rec unzip (l : List<'a * 'b>) : List<'a> * List<'b> =
    match l with
    | [] -> ([], [])
    | (x,y) :: tl ->
        let l1, l2 = unzip tl
        x :: l1, y :: l2

type ListElement<'a> =
    | Element of 'a
    | NestedList of List<ListElement<'a>>

let rec flatten (l:List<ListElement<'a>>) : List<'a> =
    match l with
    | [] -> []
    | Element hd :: tl -> hd :: (flatten tl) // If it isnt a nested element go to the next element
    | NestedList nested :: tl -> // If there is a nested list then flatten the nested and concat it with the flattened tail
        flatten nested @ (flatten tl)

let rec rev (l:List<'a>) : List<'a> =
    match l with
    | [] -> []
    | hd:'a :: tl -> (rev tl) @ [hd]

let rec concat (l1:List<'a>) (l2:List<'a>) : List<'a> =
    match (l1, l2) with
    | ([],[]) -> []
    | ([], _::_) -> l2
    | (_::_, []) -> l1
    | (hd1::tl1, hd2::tl2) ->
        hd1::tl1 @ hd2::tl2

let rec nth (n:int) (l:List<'a>) : Option<'a> =
    match (n,l) with
    | (n,_) when n < 0 -> None
    | (_,[]) -> None
    | (n,hd::_) when n = 0 -> Some hd
    | (n,_::tl) -> nth (n-1)(tl)
   
let splitAt (i:int) (l:List<'a>) : List<'a> * List<'a> =
    let rec splitUtil i l acc = // Use an accumulator to hold traversed elements 
        match l with
        | [] -> rev acc, [] // there is no element left to traverse
        | _ when i = 0 -> rev acc, l // you have reached i
        | hd::tl' -> splitUtil (i-1) tl' (hd::acc)
    splitUtil i l []

(*
split 2 [1;2;3]         -> call splitUtil
splitUtil 2 [1;2;3] []  -> Match case 3 of hd::tl'
splitUtil 1 [2;3] [1]   -> Match case 3 of hd::tl'
splitUtil 0 [3] [2;1]   -> Match case 2 of i=0
List.rev [2;1], [3]     -> call List.rev on acc
return [1;2], [3]
*)

// two sorted lists into a single sorted list
let rec merge (l1:List<'a>) (l2:List<'a>) : List<'a> =
    match (l1,l2) with
    | (_,[]) -> []
    | ([],_) -> []
    | ([],[]) -> []
    | (hd::tl, hd2::tl2) when hd > hd2 -> hd2 :: (merge (tl)(tl2))
    | (hd::tl, hd2::tl2) when hd <= hd2 -> hd :: (merge (tl)(tl2))

let rec mapList (f:'a -> 'b) (l:List<'a>) : List<'b> =
    match l with
    | [] -> []
    | hd::tl -> f hd :: mapList f tl

let rec filterList (p:'a -> bool) (l:List<'a>) =
    match l with
    | [] -> []
    | hd::tl when p hd -> hd::filterList p tl
    | _::tl -> filterList p tl

let rec foldList (z:'b) (f: 'a -> 'b -> 'b) (l:List<'a>) : 'b =
    match l with
    | [] -> z // z will be the 'endstate'
    | hd::tl -> f hd (foldList z f tl)

let mapFoldList (f : 'a -> 'b) (l:List<'a>) : List<'b> =
    foldList [] (fun mapList hd -> mapList @ [f hd]) l

let mapFold (f : 'a -> 'b) (l : List<'a>) : List<'b> =
  l |> List.fold (fun mappedList x -> mappedList @ [f x]) []


// let rec filterFoldList (f:'a -> bool) (l:List<'a>) : List<'a> =
//    foldList [] (fun filterList hd -> if f hd then hd :: filterList else filterList) l

let filterFold (f : 'a -> bool) (l : List<'a>) : List<'a> =
  l |> List.fold 
          (fun filteredList hd -> 
            if f hd then 
              hd :: filteredList 
            else
              filteredList) [] |> List.rev

let flattenList (l : List<List<'a>>) : List<'a> =
  l |> List.fold (fun flattenedList l -> flattenedList @ l) []



// ------ sample exam ------
type Entry<'k, 'v> =
    {
        Key: 'k
        Value: 'v
    }
    with
        static member Create(key:'k, value: 'v) =
            {
                Key = key
                Value = value
            }

type BinaryNode<'k, 'v> when 'k : comparison =
    {
        Entry: Entry<'k, 'v>
        Left: BinarySearchTree<'k, 'v>
        Right: BinarySearchTree<'k, 'v>
    }
    with
        static member Create(entry:Entry<'k,'v>, left:BinarySearchTree<'k,'v>, right:BinarySearchTree<'k,'v>) =
            {
                Entry = entry
                Left = left
                Right = right
            }
and BinarySearchTree<'k, 'v> when 'k : comparison =
    | Empty
    | Node of BinaryNode<'k, 'v>

let rec findKey (key:'k) (tree:BinarySearchTree<'k, 'v>) : Option<'v> =
    match (key,tree) with
    | (key, Node node) when key = node.Entry.Key -> Some(node.Entry.Value)
    | (key, Node node) when key < node.Entry.Key -> findKey key node.Right
    | (key, Node node) when key > node.Entry.Key -> findKey key node.Left
    | (_,_) -> None

let curry f a b = f (a,b)
let uncurry f (a,b) = f a b

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

    printfn "\n-----UNIT 3-----\n"
    printfn "The last element of %A is %O" ([0..5]) (last [0..5])
    printfn "The length of %A is %i" ([0..5]) (length [0..5])
    printfn "The reverse of %A is %A" ([0..5]) (rev [0..5])
    printfn "Concatted %A and %A is %A" ([0..2]) ([3..5]) (concat ([0..2]) ([3..5]))
    printfn "element %i of %A is %O" (1) ([0..3]) (nth 1 [0..3])

    0 // return an integer exit code

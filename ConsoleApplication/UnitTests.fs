open System.IO
open CoreTypeSystem
open NUnit.Framework
open FsUnit

// Change TagSeq list to TFJ string
let rec ToString(lst : TagSeq) : string = 
    match lst with
    | [] -> ""
    | (Tag.Plus, n1) :: xs -> "+" + n1.ToString() + ToString xs
    | (Tag.Minus, n1) :: xs -> "-" + n1.ToString() + ToString xs
    | (Tag.Max, n1) :: xs -> "#" + n1.ToString() + ToString xs
    | (Tag.Join, n1) :: xs -> ":" + n1.ToString() + ToString xs
    | x :: xs -> x.ToString() + ToString xs

let test ast expectedResult =
    let res = infer ast [] in
        ToString res |> should equal expectedResult


//+2-1 -> #2
let ast1 = 
    [ Leaf(Tag.Plus, 2)
      Leaf(Tag.Minus, 1) ] in 
      test ast1 "#2"

//+1+2-1-1 -> #(1+2)
let ast2 = 
    [ Leaf(Tag.Plus, 1)
      Leaf(Tag.Plus, 2)
      Leaf(Tag.Minus, 1)
      Leaf(Tag.Minus, 1) ]in 
      test ast2 "#3"

//+2(-1)-1 -> #4
let ast3 = 
    [ Leaf(Tag.Plus, 2)
      Branch([ Leaf(Tag.Minus, 1) ])
      Leaf(Tag.Minus, 1) ] in 
      test ast3 "#4"

//+2+3-1(-1)-1 -> #5
let ast4 = 
    [ Leaf(Tag.Plus, 2)
      Leaf(Tag.Plus, 3)
      Leaf(Tag.Minus, 1)
      Branch([ Leaf(Tag.Minus, 1) ])
      Leaf(Tag.Minus, 1) ]in 
      test ast4 "#5"

//+2(+3-1-1)-1 -> #7
let ast5 = 
    [ Leaf(Tag.Plus, 2)
      Branch([ Leaf(Tag.Plus, 3)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1) ])
      Leaf(Tag.Minus, 1) ]in 
      test ast5 "#7"

//+2(+2-2)-1 -> #6
let ast6 = 
    [ Leaf(Tag.Plus, 2)
      Branch([ Leaf(Tag.Plus, 2)
               Leaf(Tag.Minus, 2) ])
      Leaf(Tag.Minus, 1) ]in 
      test ast6 "#6"

//+2(+1-1-1)-1 -> #5
let ast7 = 
    [ Leaf(Tag.Plus, 2)
      Branch([ Leaf(Tag.Plus, 1)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1) ])
      Leaf(Tag.Minus, 1) ] in 
      test ast7 "#5"

//+1+2-1(-1)-1 -> #3
let ast8 = 
    [ Leaf(Tag.Plus, 1)
      Leaf(Tag.Plus, 2)
      Leaf(Tag.Minus, 1)
      Branch([ Leaf(Tag.Minus, 1) ])
      Leaf(Tag.Minus, 1) ] in 
      test ast8 "#3"

//+1+2(+4-1-1-1)+3(+5-1-1-1-1)-1+6-1-1+7-1-1
let ast9 = 
    [ Leaf(Tag.Plus, 1)
      Leaf(Tag.Plus, 2)
      Branch([ Leaf(Tag.Plus, 4)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1) ])
      Leaf(Tag.Plus, 3)
      Branch([ Leaf(Tag.Plus, 5)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1) ])
      Leaf(Tag.Minus, 1)
      Leaf(Tag.Plus, 6)
      Leaf(Tag.Minus, 1)
      Leaf(Tag.Minus, 1)
      Leaf(Tag.Plus, 7)
      Leaf(Tag.Minus, 1)
      Leaf(Tag.Minus, 1) ] in 
      test ast9 "#24"

//+1+2(+4-1-1-1)+3(+5-1-1-1-1)-1+12-1-1+7-1-1
let ast10 = 
    [ Leaf(Tag.Plus, 1)
      Leaf(Tag.Plus, 2)
      Branch([ Leaf(Tag.Plus, 4)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1) ])
      Leaf(Tag.Plus, 3)
      Branch([ Leaf(Tag.Plus, 5)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1) ])
      Leaf(Tag.Minus, 1)
      Leaf(Tag.Plus, 12)
      Leaf(Tag.Minus, 1)
      Leaf(Tag.Minus, 1)
      Leaf(Tag.Plus, 7)
      Leaf(Tag.Minus, 1)
      Leaf(Tag.Minus, 1) ] in 
      test ast10 "#25"

//+1+2(+1-1-1-1)+3(+1-1-1-1-1)-1+6-1-1+15-1-1
let ast11 = 
    [ Leaf(Tag.Plus, 1)
      Leaf(Tag.Plus, 2)
      Branch([ Leaf(Tag.Plus, 1)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1) ])
      Leaf(Tag.Plus, 3)
      Branch([ Leaf(Tag.Plus, 1)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1)
               Leaf(Tag.Minus, 1) ])
      Leaf(Tag.Minus, 1)
      Leaf(Tag.Plus, 6)
      Leaf(Tag.Minus, 1)
      Leaf(Tag.Minus, 1)
      Leaf(Tag.Plus, 15)
      Leaf(Tag.Minus, 1)
      Leaf(Tag.Minus, 1) ] in 
      test ast11 "#18"

printfn "\nAll tests were executed. Press a key to exit.\n"
System.Console.ReadKey(true) |> ignore

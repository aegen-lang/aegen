open Aegen.Compiler

[<EntryPoint>]
let main arv =
    let p = Parser()
    let input = @"
package main
import aegen::std::fmt

func main(*a: i32) {
    let *a: i32 = &1
}
"
    p.run input |> printfn "%A"
    p.getFlatAST() |> printfn "%A"
    0
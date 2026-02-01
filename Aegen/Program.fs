open Aegen.Compiler

[<EntryPoint>]
let main arv =
    let p = Parser()
    let input = @"
package main
import aegen::std::fmt

func main() {
    let ch = chan i32(3)
}
"
    p.run input |> printfn "%A"
    p.getFlatAST() |> printfn "%A"
    0
namespace Aegen.Compiler

open FParsec

type ASTNode = { Type: string; Line: int64; Column: int64; Data: string }

exception FASTParseException of string

type FASTResult<'T, 'E> =
    | Ok of 'T
    | Err of 'E
    member this.unwrap() =
        match this with
        | Ok v -> v
        | Err _ -> failwith "FASTResult.unwrap() ->! It's Not FASTRsult.Ok."

type FlatAST() =
    let ident =
        regex @"[\p{L}_][\p{L}\p{N}_]*"
    let bool_ =
        pstring "bool"
        .>> spaces
        .>> pchar ':'
        .>> spaces
        >>. choice [
            attempt (stringReturn "true" true)
            stringReturn "false" false
        ]
        .>> spaces
        |>> box
    let str_ =
        pstring "str"
        .>> spaces
        .>> pchar ':'
        >>. between
            (spaces .>> pchar '"')
            (pchar '"' .>> spaces)
            ident
        .>> spaces
        |>> box
    let ref_ =
        pstring "ref"
        .>> spaces
        .>> pchar ':'
        .>> spaces
        >>. pint32
        .>> spaces
        |>> box
    let arr_ =
        pstring "arr"
        .>> spaces
        .>> pchar ':'
        >>. between
            (spaces .>> pchar '[' .>> spaces)
            (spaces .>> pchar ']' .>> spaces)
            (sepBy
                (choice [
                    attempt bool_
                    attempt str_
                    ref_
                ])
                (pchar ',' .>> spaces)
            )
        .>> spaces
        |>> box
    let program =
        between
            (spaces .>> pchar '[' .>> spaces)
            (pchar ']' .>> spaces)
            (sepBy
                (choice [
                    attempt bool_
                    attempt str_
                    attempt ref_
                    arr_
                ])
                (spaces .>> pchar ',' .>> spaces)
            )
        .>> eof

    member val private Ast = [||] with get, set
    member val private Data = [||] with get, set

    override this.ToString (): string = 
        sprintf "FlatAST: %A\n\n" this.Ast

    member this.add(ast) =
        this.Ast <- [|ast|] |> Array.append this.Ast
        this.Ast.Length - 1

    member this.initData() =
        this.Data <-
            this.Ast
            |> Array.map
                (fun ast ->
                    match run program ast.Data with
                    | Success(res, _, _) -> res
                    | Failure(msg, _, _) -> failwith <| sprintf "FlatAST.Data ->! Failured Parse.\n%s" msg
                )

    member this.getAst i =
        if i < 0 || this.Ast.Length <= i
        then Err("FlatAST.Ast ->! Index Out Of Range.")
        else Ok(this.Ast[i])

    member this.getData i =
        if i < 0 || this.Data.Length <= i
        then Err("FlatAST.Data ->! Index Out Of Range.")
        else Ok(this.Data[i])
    // TODO

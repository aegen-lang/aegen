namespace Aegen.Compiler

open Aegen.Compiler

open FParsec

type Node = string * int64 * int64 * string

type Assoc = Associativity

type Parser() =
    let fast = FlatAST()

    let mySpaces = many (pchar ' ')
    let mySpaces1 = many1 (pchar ' ')
    let endLines = many (newline <|> pchar ';')
    let funcEndLines = many newline
    let stEndLines = many newline
    let prEndLines = many newline
    let ident = regex @"[\p{L}_][\p{L}\p{N}_]*"

    let opp = OperatorPrecedenceParser()

    let expTerm = choice [
        attempt (
            pipe2
                getPosition
                pfloat
                (fun pos value ->
                    fast.add {
                        Type = "operand_float"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf "[str: \"%f\"]" value
                    }
                )
        )
        attempt (
            pipe2
                getPosition
                (pint32 .>> attempt (pchar 'l'))
                (fun pos value ->
                    fast.add {
                        Type = "operand_int32"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf "[str: \"%i\"]" value
                    }
                )
        )
        attempt (
            pipe2
                getPosition
                (pint64 .>> attempt (pchar 'L'))
                (fun pos value ->
                    fast.add {
                        Type = "operand_int64"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf "[str: \"%i\"]" value
                    }
                )
        )
        attempt (
            pipe2
                getPosition
                (puint32 .>> pchar 'u')
                (fun pos value ->
                    fast.add {
                        Type = "operand_uint32"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf "[str: \"%i\"]" value
                    }
                )
        )
        attempt (
            pipe2
                getPosition
                (puint64 .>> pstring "UL")
                (fun pos value ->
                    fast.add {
                        Type = "operand_uint64"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf "[str: \"%i\"]" value
                    }
                )
        )
        attempt (
            pipe2
                getPosition
                (pint16 .>> pchar 's')
                (fun pos value ->
                    fast.add {
                        Type = "operand_int16"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf "[str: \"%i\"]" value
                    }
                )
        )
        attempt (
            pipe2
                getPosition
                (puint16 .>> pstring "us")
                (fun pos value ->
                    fast.add {
                        Type = "operand_uint16"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf "[str: \"%i\"]" value
                    }
                )
        )
        attempt (
            pipe2
                getPosition
                (pint8 .>> pchar 'y')
                (fun pos value ->
                    fast.add {
                        Type = "operand_int8"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf "[str: \"%i\"]" value
                    }
                )
        )
        attempt (
            pipe2
                getPosition
                (puint8 .>> pstring "uy")
                (fun pos value ->
                    fast.add {
                        Type = "operand_uint8"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf "[str: \"%i\"]" value
                    }
                )
        )
        attempt (
            pipe2
                getPosition
                (between (pchar '"') (pchar '"') (manyStrings (regex @"\\?.")))
                (fun pos value ->
                    fast.add {
                        Type = "operand_string"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf "[str: \"%s\"]" value
                    }
                )
        )
        attempt (
            pipe2
                getPosition
                (between (pchar '\'') (pchar '\'') (regex @"\\?."))
                (fun pos value ->
                    fast.add {
                        Type = "operand_char"
                        Line = pos.Line
                        Column = pos.Column
                        Data = sprintf "[str: \"%s\"]" value
                    }
                )
        )
    ]
    let funcTerm, funcTermRef = createParserForwardedToRef()
    let structTerm, structTermRef = createParserForwardedToRef()
    let protocolTerm, protocolTermRef = createParserForwardedToRef()

    let block p =
        between
            (spaces .>> pchar '{' .>> spaces)
            (spaces .>> pchar '}' .>> spaces)
            (many p)
    let block1 p =
        between
            (spaces .>> pchar '{' .>> spaces)
            (spaces .>> pchar '}' .>> spaces)
            (many1 p)
    let blockOrExp p =
        choice [
            attempt (block1 funcTerm)
            spaces >>. p |>> (fun x -> [x])
        ]
    
    let func_base typ modi =
        pipe2
            getPosition
            (opt (stringReturn modi true .>> mySpaces1) .>> pstring "func" .>> mySpaces1
                .>>. ident
                .>>. between
                    (spaces .>> pchar '(' .>> spaces)
                    (spaces .>> pchar ')' .>> spaces)
                    (sepBy (ident) (spaces .>> attempt (pchar ',' .>> spaces)))
                .>>. ident
                .>>. block1 funcTerm
                .>> funcEndLines
            )
            (fun pos ((((isMod, name), args), rettyp), content) ->
                fast.add {
                    Type = sprintf "func%s" (match typ with | "" -> "" | s -> "_" + s)
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", str: \"%s\", arr: [%s]], arr[%s]"
                        (match isMod with | Some v -> v | None -> false)
                        name
                        rettyp
                        (args |> List.map (sprintf "str: \"%s\"") |> String.concat ", ")
                        (content |> List.map (sprintf "ref: %i") |> String .concat ", ")
                }
            )

    let package_ =
        pipe2
            getPosition
            (between
                (spaces .>> pstring "package" .>> mySpaces1)
                endLines
                (sepBy ident (mySpaces .>> pstring "::" .>> mySpaces))
            )
            (fun pos lst ->
                fast.add {
                    Type = "package"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf "[str: \"%s\"]" (lst |> String.concat "::")
                }
            )
        .>> spaces
    let import_ =
        pipe2
            getPosition
            (between
                (spaces .>> pstring "import" .>> mySpaces1)
                endLines
                (sepBy ident (mySpaces .>> pstring "::" .>> mySpaces .>> notFollowedByString "*")
                    .>>. opt (pstring "::" .>> mySpaces .>> pstring "*" >>% byte(0))
                )
            )
            (fun pos (lst, o) ->
                fast.add {
                    Type = "import"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s%s\"]"
                        (lst |> String.concat "::")
                        (match o with | Some _ -> "::*" | None -> "")
                }
            )
        .>> spaces

    let let_ =
        pipe2
            getPosition
            (pstring "let" .>> mySpaces1
                >>. ident
                .>> (spaces .>> pchar '=' .>> spaces)
                .>>. blockOrExp
                    opp.ExpressionParser
                .>> endLines
            )
            (fun pos (name, content) ->
                fast.add {
                    Type = "let"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s\", arr: [%s]]"
                        name
                        (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                }
            )
    let val_st =
        pipe2
            getPosition
            (opt (attempt (stringReturn "pub" true .>> spaces1)) .>> pstring "val" .>> mySpaces1
                .>>. ident
                .>> (spaces .>> pchar '=' .>> spaces)
                .>>. blockOrExp
                    opp.ExpressionParser
                .>> endLines
            )
            (fun pos ((isPub, name), content) ->
                fast.add {
                    Type = "val_struct"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", arr: [%s]]"
                        (match isPub with | Some v -> v | None -> false)
                        name
                        (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                }
            )
    let val_pr =
        pipe2
            getPosition
            (opt (attempt (stringReturn "abs" true .>> spaces1)) .>> pstring "val" .>> mySpaces1
                .>>. ident
                .>> (spaces .>> pchar '=' .>> spaces)
                .>>. blockOrExp
                    opp.ExpressionParser
            )
            (fun pos ((isAbs, name), content) ->
                fast.add {
                    Type = "val_protocol"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", arr: [%s]]"
                        (match isAbs with | Some v -> v | None -> false)
                        name
                        (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                }
            )
    let func_ = func_base "" "pub"
    let func_st = func_base "struct" "pub"
    let func_pr = func_base "protocol" "abs"

    let struct_ =
        pipe3
            getPosition
            (opt (attempt (stringReturn "pub" true .>> spaces1)))
            (pstring "struct" .>> spaces1
                >>. ident
                .>>. opt
                    (spaces1
                        .>> pstring "impl"
                        .>> spaces
                        >>. ident
                        .>>. opt (attempt (many (spaces .>> pchar ',' .>> spaces >>. ident)))
                    )
                .>>. block
                    structTerm
                .>> stEndLines
            )
            (fun pos modi ((name, protocols), content) ->
                fast.add {
                    Type = "struct"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", arr: [%s], arr: [%s]]"
                        (match modi with | Some v -> v | None -> false)
                        name
                        ((match protocols with | Some (f, lst) -> [f] @ (match lst with | Some l -> l | None -> []) | None -> []) |> List.map (sprintf "str: \"%s\"") |> String.concat ", ")
                        (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                }
            )
    
    let protocol_ =
        pipe3
            getPosition
            (opt (attempt (stringReturn "pub" true .>> spaces1)))
            (pstring "protocol" .>> mySpaces1
                >>. ident
                .>>. block
                    (protocolTerm)
                .>> prEndLines
            )
            (fun pos modi (name, content) ->
                fast.add {
                    Type = "protocol"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", arr: [%s]]"
                        (match modi with | Some v -> v | None -> false)
                        name
                        (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                }
            )
    let program =
        spaces
        >>. package_
        .>>. many import_
        .>>. many (choice [
            attempt func_
            attempt struct_
            protocol_
        ]) .>> eof
        |>> (fun ((package, imports), body) ->
            fast.add {
                Type = "program"
                Line = 1l
                Column = 1l
                Data = sprintf
                    "[ref: %i, arr: [%s], arr: [%s]]"
                    package
                    (imports |> List.map (sprintf "ref: %i") |> String.concat ", ")
                    (body |> List.map (sprintf "ref: %i") |> String.concat ", ")
            }
        )

    do
        funcTermRef.Value <- choice [
            attempt func_
            attempt let_
            opp.ExpressionParser
        ]
        structTermRef.Value <- choice [
            attempt val_st
            func_st
        ]
        protocolTermRef.Value <- choice [
            attempt val_pr
            func_pr
        ]

    member _.Struct = struct_

    member _.run (s: string) =
        let s = s.Replace("\r", "")
        #if DEBUG
        printfn "parse: %A\n" s
        #endif
        match run program s with
        | Success(res, _, _) -> res
        | Failure(error, _, _) -> failwith error
namespace Aegen.Compiler

open Aegen.Compiler

open FParsec

type Node = string * int64 * int64 * string

type Assoc = Associativity


/// <summary>
/// require type inference: "", -1
/// </summary>
type Parser() =
    let fast = FlatAST()

    let endLines = many (newline <|> pchar ';')
    let funcEndLines = many newline
    let stEndLines = many newline
    let prEndLines = many newline
    let ident = regex @"[\p{L}_][\p{L}\p{N}_]*"
    
    let funcTerm, funcTermRef = createParserForwardedToRef()
    let structTerm, structTermRef = createParserForwardedToRef()
    let protocolTerm, protocolTermRef = createParserForwardedToRef()
    let typ, typRef = createParserForwardedToRef()
    let exprTerm, exprTermref = createParserForwardedToRef()
    
    let typp =
        choice [
            attempt (
                pipe2
                    getPosition
                    (pstring "chan" .>> spaces1 >>. typ)
                    (fun pos t ->
                        fast.add {
                            Type = "type_chan"
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf "[str: \"%s\"]" t
                        }
                    )
            )
            attempt (
                pipe2
                    getPosition
                    (
                        pstring "func"
                        >>. between
                            (spaces .>> pchar '(')
                            (spaces .>> pchar ')')
                            (sepBy (spaces >>. typ) (spaces .>> pchar ','))
                        .>>. opt (attempt (spaces >>. typ .>> spaces))
                    )
                    (fun pos (arg, rettyp) ->
                        fast.add {
                            Type = "type_func"
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf
                                "[arr: [%s], str: \"%s\"]"
                                (arg |> List.map (sprintf "str: \"%s\"") |> String.concat ", ")
                                (match rettyp with | Some t -> t | None -> "")
                        }
                    )
            )
            (
                pipe2
                    getPosition
                    typ
                    (fun pos t ->
                        fast.add {
                            Type = "type"
                            Line = pos.Line
                            Column = pos.Column
                            Data = sprintf "[str: \"%s\"]" t
                        }
                    )
            )
        ]

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

    let opp = OperatorPrecedenceParser()
    
    let func_base typ modi =
        pipe2
            getPosition
            (opt (stringReturn modi true .>> spaces1) .>> pstring "func" .>> spaces1
                .>>. ident
                .>>. between
                    (spaces .>> pchar '(')
                    (spaces .>> pchar ')')
                    (sepBy
                        (
                            choice [
                                attempt (spaces >>. stringReturn "*" true .>> spaces)
                                spaces >>% false
                            ]
                            .>>.
                            ident
                            .>>. opt (attempt (spaces .>> pchar ':' .>> spaces >>. typp))
                        )
                        (spaces .>> pchar ',')
                    )
                .>>. opt (attempt (spaces >>. typp .>> spaces))
                .>>. block1 funcTerm
                .>> funcEndLines
            )
            (fun pos ((((isMod, name), args), rettyp), content) ->
                fast.add {
                    Type = sprintf "func%s" (match typ with | "" -> "" | s -> "_" + s)
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", ref: %i, arr: [%s], arr[%s]]"
                        (match isMod with | Some v -> v | None -> false)
                        name
                        (match rettyp with | Some typ -> typ | None -> -1)
                        (args |> List.map (fun ((isrepo, f), s) -> sprintf "bool: %b, str: \"%s\", ref: %i" isrepo f (match s with | Some i -> i | None -> -1)) |> String.concat ", ")
                        (content |> List.map (sprintf "ref: %i") |> String .concat ", ")
                }
            )

    let package_ =
        pipe2
            getPosition
            (between
                (spaces .>> pstring "package" .>> spaces1)
                endLines
                (sepBy1 ident (attempt (spaces .>> pstring "::" .>> spaces)))
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
                (spaces .>> pstring "import" .>> spaces1)
                endLines
                (sepBy1 ident (attempt (spaces .>> pstring "::" .>> spaces)))
            )
            (fun pos lst ->
                fast.add {
                    Type = "import"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s\"]"
                        (lst |> String.concat "::")
                }
            )
        .>> spaces

    let let_ =
        pipe2
            getPosition
            (pstring "let"
                .>> spaces1
                >>. choice [
                    attempt (stringReturn "mut" true)
                    pstring "" >>% false
                ]
                .>> spaces
                .>>. choice [
                    attempt (stringReturn "*" true .>> spaces)
                    pstring "" >>% false
                ]
                .>>. ident
                .>>. opt (attempt (spaces .>> pchar ':' .>> spaces >>. typp))
                .>> (spaces .>> pchar '=' .>> spaces)
                .>>. blockOrExp
                    opp.ExpressionParser
                .>> endLines
            )
            (fun pos ((((ismut, isrepo), name), t), content) ->
                fast.add {
                    Type = "let"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, bool: %b, str: \"%s\", ref: %i, arr: [%s]]"
                        ismut
                        isrepo
                        name
                        (
                            match t with
                            | Some t -> t
                            | None -> -1
                        )
                        (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                }
            )
    let val_st =
        pipe2
            getPosition
            (opt (attempt (stringReturn "pub" true .>> spaces1)) .>> pstring "val" .>> spaces1
                .>>. ident
                .>>. opt (attempt (spaces .>> pchar ':' .>> spaces >>. typp))
                .>> (spaces .>> pchar '=' .>> spaces)
                .>>. blockOrExp
                    opp.ExpressionParser
                .>> endLines
            )
            (fun pos (((isPub, name), t), content) ->
                fast.add {
                    Type = "val_struct"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", ref: %i, arr: [%s]]"
                        (match isPub with | Some v -> v | None -> false)
                        name
                        (
                            match t with
                            | Some t -> t
                            | None -> -1
                        )
                        (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                }
            )
    let val_pr =
        pipe2
            getPosition
            (opt (attempt (stringReturn "abs" true .>> spaces1)) .>> pstring "val" .>> spaces1
                .>>. ident
                .>>. opt (attempt (spaces .>> pchar ':' .>> spaces >>. typp))
                .>> (spaces .>> pchar '=' .>> spaces)
                .>>. blockOrExp
                    opp.ExpressionParser
            )
            (fun pos (((isAbs, name), t), content) ->
                fast.add {
                    Type = "val_protocol"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[bool: %b, str: \"%s\", ref: %i, arr: [%s]]"
                        (match isAbs with | Some v -> v | None -> false)
                        name
                        (
                            match t with
                            | Some t -> t
                            | None -> -1
                        )
                        (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                }
            )

    let func_ = func_base "" "pub"
    let func_st = func_base "struct" "pub"
    let func_pr = func_base "protocol" "abs"

    let async_ =
        pipe2
            getPosition
            (
                pstring "async"
                >>. between
                    (spaces .>> pchar '(')
                    (pchar ')' .>> spaces)
                    (spaces >>. ident .>> spaces)
                .>>. opp.ExpressionParser
            )
            (fun pos (chan, content) ->
                fast.add {
                    Type = "async"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[str: \"%s\", ref: %i]"
                        chan
                        content
                }
            )

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
            (pstring "protocol" .>> spaces1
                >>. ident
                .>>. block
                    protocolTerm
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
    let oprators = [|
        "=", 1
    |]

    do
        opp.TermParser <- exprTerm

        let adjustPosition offset (pos: Position) =
            Position(pos.StreamName, pos.Index + int64 offset,
                     pos.Line, pos.Column + int64 offset)
        let addOpr name prece assoc mapping =
            let op = InfixOperator(name, getPosition .>> spaces, prece, assoc, (), fun opPos lhs rhs -> mapping (adjustPosition -name.Length opPos) lhs rhs)
            opp.AddOperator op
        
        addOpr "+" 1 Assoc.Left
            (fun pos lhs rhs ->
                fast.add {
                    Type = "operator_add"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[ref: %i, ref: %i]"
                        lhs
                        rhs
                }
            )
        addOpr "*" 2 Assoc.Left
            (fun pos lhs rhs ->
                fast.add {
                    Type = "operator_mul"
                    Line = pos.Line
                    Column = pos.Column
                    Data = sprintf
                        "[ref: %i, ref: %i]"
                        lhs
                        rhs
                }
            )

        typRef.Value <-
            choice [
                attempt (
                    pipe2
                        ident
                        (between
                            (spaces .>> pchar '<')
                            (pchar '>' .>> spaces)
                            (sepBy1 (spaces >>. typ) (spaces .>> pchar ','))
                        )
                        (fun f s -> f + "<" + (String.concat "," s) + ">")
                )
                ident
            ]
        funcTermRef.Value <- choice [
            attempt (func_ .>> funcEndLines)
            attempt (let_ .>> endLines)
            attempt (async_ .>> endLines)
            opp.ExpressionParser
        ] .>> endLines
        structTermRef.Value <- choice [
            attempt (val_st .>> endLines)
            func_st .>> funcEndLines
        ]
        protocolTermRef.Value <- choice [
            attempt (val_pr .>> endLines)
            func_pr .>> funcEndLines
        ]
        exprTermref.Value <-
            choice [
                attempt (
                    pipe2
                        getPosition
                        (pfloat .>> (pchar 'f' <|> pchar 'F'))
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
                        (pint32 .>> opt (attempt (pchar 'l')))
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
                attempt (
                    pipe2
                        getPosition
                        (
                            pstring "func"
                            >>. between
                                (spaces .>> pchar '(')
                                (spaces .>> pchar ')' .>> spaces)
                                (sepBy (spaces >>. typp) (spaces .>> pchar ','))
                            .>>. opt (attempt (spaces >>. typp .>> spaces))
                            .>>. block1
                                funcTerm
                        )
                        (fun pos ((arg, rettyp), content) ->
                            fast.add {
                                Type = "operand_func"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf
                                    "[arr: [%s], ref: %i, arr: [%s]]"
                                    (arg |> List.map (sprintf "ref: %i") |> String.concat ", ")
                                    (match rettyp with | Some typ -> typ | None -> -1)
                                    (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                            }
                        )

                )
                attempt (
                    pipe2
                        getPosition
                        (
                            pstring "chan"
                            .>> spaces1
                            >>. ident
                            .>>. between
                                (spaces .>> pchar '(')
                                (pchar ')')
                                (spaces >>. getPosition .>>. opt (attempt opp.ExpressionParser .>> spaces))
                        )
                        (fun pos (typ, (pos2, cap)) ->
                            fast.add {
                                Type = "chan"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf
                                    "[str: \"%s\", ref: %i]"
                                    typ
                                    (
                                        match cap with
                                        | Some cap -> cap
                                        | None ->
                                            fast.add {
                                                Type = "operand_int32"
                                                Line = pos2.Line
                                                Column = pos2.Column
                                                Data = sprintf "[str: \"1\"]"
                                            }
                                    )
                            }
                        )
                )
                attempt (
                    pipe2
                        getPosition
                        (between (pchar '(' .>> spaces) (spaces .>> pchar ')') opp.ExpressionParser)
                        (fun pos expr ->
                            fast.add {
                                Type = "paren"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[ref: %i]" expr
                            }
                        )
                )
                attempt (
                    pipe2
                        getPosition
                        (block1 funcTerm)
                        (fun pos content ->
                            fast.add {
                                Type = "block"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf
                                    "[arr: [%s]]"
                                    (content |> List.map (sprintf "ref: %i") |> String.concat ", ")
                            }
                        )
                )
                attempt (
                    pipe2
                        getPosition
                        (
                            pchar '*'
                            .>> spaces
                            >>. exprTerm
                        )
                        (fun pos expr ->
                            fast.add {
                                Type = "repo_move"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[ref: %i]" expr
                            }
                        )
                )
                (
                    pipe2
                        getPosition
                        (
                            pchar '&'
                            .>> spaces
                            >>. exprTerm
                        )
                        (fun pos expr ->
                            fast.add {
                                Type = "com"
                                Line = pos.Line
                                Column = pos.Column
                                Data = sprintf "[ref: %i]" expr
                            }
                        )
                )
            ]
            .>> spaces

    member _.Struct = struct_

    member _.run (s: string) =
        let s = s.Replace("\r", "")
        #if DEBUG
        printfn "parse: %A\n" s
        #endif
        match run program s with
        | Success(res, _, _) -> res
        | Failure(error, _, _) ->
            eprintfn "%s" error
            -1

    member _.getFlatAST() = fast
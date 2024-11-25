module LambdaScript.Parser

open FParsec

type LambdaScript =
    | Expression of LambdaExpr
    | StatementList of LambdaStatement list
and LambdaStatement =
    | Assign of Variable:string * Expr:LambdaExpr
    | If of Condition:LambdaExpr * Then:LambdaStatement * Else:LambdaStatement option
    | Block of LambdaStatement list
    | Return of LambdaExpr
and LambdaExpr =  
    | Number of N:decimal
    | String of S:string
    | Boolean of B:bool
    | PropertyAccessor of Expr:LambdaExpr * PropertyName:string
    | FunctionCall of FnName:string * Args:LambdaExpr list
    | CollectionFilter of CololectionExpr:LambdaExpr * Predicate:LambdaExpr
    | Identifier of Id: string
    | UnaryOp of Op:(UnaryOperator * LambdaExpr)
    | BinaryOp of Op:(LambdaExpr * BinaryOperator * LambdaExpr)
    | TernaryOp of Condition:LambdaExpr * Then:LambdaExpr * Else:LambdaExpr
and UnaryOperator = 
    | Bang
and BinaryOperator =
    | Eq
    | NotEq
    | Lt
    | Lte
    | Gt
    | Gte
    | And
    | Or
    | Plus
    | Minus
    | Mult
    | Divide
    | Mod

type LambdaScriptParserState = unit
type LambdaScriptParser<'a> = Parser<'a, LambdaScriptParserState>
type LambdaScriptParser = LambdaScriptParser<LambdaScript>

module Internal =
    type Assoc = Associativity

    let justSpaces  = skipMany  (pchar ' ' <|> pchar '\t')
    let ws = spaces
    let ignore_ws_str s = pstring s >>. justSpaces
    let str = pstring
    let char = pchar
    let underscore = char '_'
    let atSign = char '@'
    let doubleAtSign = pstring "@@"

    let numericParser = 
        choice [
            many1Chars digit
            pstring "-" >>. (many1Chars digit) |>> (fun n -> "-" + n)
        ]|>> (System.Decimal.Parse >> Number) <?> "number"

    let booleanParser = 
        choice [
            pstring "true" |>> fun _ -> Boolean true
            pstring "True" |>> fun _ -> Boolean true
            pstring "false" |>> fun _ -> Boolean false
            pstring "False" |>> fun _ -> Boolean false

        ] <?> "boolean"

    let betweenQuotes = between (ignore_ws_str "\"") (ignore_ws_str "\"")
    let betweenParens = between (ignore_ws_str "(") (ignore_ws_str ")")
    let betweenParens2 = between (ignore_ws_str "(") (ignore_ws_str ")")
    let betweenParens3 = between (ignore_ws_str "(") (ignore_ws_str ")")
    let betweenBrackets = between (ignore_ws_str "[") (ignore_ws_str "]")
    let notQuoteChar = noneOf (Seq.toList "\"")
    let unquotedString = manyChars notQuoteChar
    let stringParser = betweenQuotes unquotedString |>> String <?> "string"

    let identifierParser = 
        parse {
            let! first = attempt doubleAtSign <|> (letter <|> underscore <|> atSign |>> string)       
            let! rest = manyChars (letter <|> underscore <|> digit)
            return Identifier <| first.ToString() + rest
        } 
        <?> "identifier"

    let propertyAccessorParser = 
        parse {
            let! _ = pstring "."
            let! first = letter <|> underscore
            let! rest = manyChars (letter <|> underscore <|> digit)
            let prop = first.ToString() + rest
            return (fun expr -> PropertyAccessor (expr, prop))
        }
        <?> "property accessor"

    let collectionFilterParser expressionParser =  
        parse {
            let! predicateExpr = betweenParens3 expressionParser
            return (fun expr -> CollectionFilter(expr, predicateExpr))
        } <?> "collection filter"

    let chainedPropertyAccessorParser expressionParser = 
        parse {
            let! identifier = identifierParser
            let! properties = many (propertyAccessorParser <|> collectionFilterParser expressionParser)
            return List.fold (fun expr fn -> fn expr) identifier properties
        }
        <?> "chained property accessor"


    let functionCallParser expressionParser = 
        parse {
            let! fnName = parse {
                let! first = letter <|> underscore
                let! rest = manyChars (letter <|> underscore <|> digit)
                return first.ToString() + rest
            }

            let! args = betweenParens2 (sepBy expressionParser (ignore_ws_str ","))
            return FunctionCall (fnName, args)
        }
        <?> "function call"

    let opp = new OperatorPrecedenceParser<LambdaExpr,unit,unit>()
    let lambdaExprParser: LambdaScriptParser<LambdaExpr> = opp.ExpressionParser

    let termParser =  choice [
        stringParser
        numericParser
        booleanParser
        attempt (functionCallParser lambdaExprParser)
        chainedPropertyAccessorParser lambdaExprParser
    ]


    opp.TermParser <- (termParser .>> ws) <|> betweenParens lambdaExprParser <|> betweenBrackets termParser


    opp.AddOperator(TernaryOperator("?", ws, ":", ws,  1, Assoc.Right, fun x y z -> TernaryOp(x, y, z)));
    opp.AddOperator(InfixOperator("||", ws, 2, Assoc.Left, fun x y -> BinaryOp (x, Or, y)))
    opp.AddOperator(InfixOperator("|", ws, 2, Assoc.Left, fun x y -> BinaryOp (x, Or, y)))
    opp.AddOperator(InfixOperator("&", ws, 3, Assoc.Left, fun x y -> BinaryOp (x, And, y)))
    opp.AddOperator(InfixOperator("&&", ws, 3, Assoc.Left, fun x y -> BinaryOp (x, And, y)))

    opp.AddOperator(InfixOperator("==", ws, 4, Assoc.Left, fun x y -> BinaryOp (x, Eq, y)))
    opp.AddOperator(InfixOperator("!=", ws, 4, Assoc.Left, fun x y -> BinaryOp (x, NotEq, y)))
    opp.AddOperator(InfixOperator("<", ws, 4, Assoc.Left, fun x y -> BinaryOp (x, Lt, y)))
    opp.AddOperator(InfixOperator("<=", ws, 4, Assoc.Left, fun x y -> BinaryOp (x, Lte, y)))
    opp.AddOperator(InfixOperator(">", ws, 4, Assoc.Left, fun x y -> BinaryOp (x, Gt, y)))
    opp.AddOperator(InfixOperator(">=", ws, 4, Assoc.Left, fun x y -> BinaryOp (x, Gte, y)))

    opp.AddOperator(InfixOperator("+", ws, 5, Assoc.Left, fun x y -> BinaryOp (x, Plus, y)))
    opp.AddOperator(InfixOperator("-", ws, 5, Assoc.Left, fun x y -> BinaryOp (x, Minus, y)))
    opp.AddOperator(InfixOperator("*", ws, 6, Assoc.Left, fun x y -> BinaryOp (x, Mult, y)))
    opp.AddOperator(InfixOperator("/", ws, 6, Assoc.Left, fun x y -> BinaryOp (x, Divide, y)))
    opp.AddOperator(InfixOperator("%", ws, 6, Assoc.Left, fun x y -> BinaryOp (x, Mod, y)))
    
    opp.AddOperator(PrefixOperator("!", ws, 7, true, fun x -> UnaryOp (Bang, x)))

    let lambdaStatementParser, lambdaStatementParserRef = createParserForwardedToRef<LambdaStatement, Unit>()

    let statementListParser = sepEndBy1 lambdaStatementParser (many1 (pchar ';' <|> newline))   

    let blockParser = 
        parse {
            do! pstring "{" >>. spaces
            let! statements = statementListParser .>> spaces
            do! optional (pchar ';' .>> spaces)
            do! pstring "}" >>. justSpaces
            return Block statements
        } <?> "block statement"

    let variableParser: Parser<string, unit> = 
        parse {
            let! first = atSign
            let! rest = manyChars (letter <|> underscore <|> digit)
            return first.ToString() + rest
        } <?> "variable"

    let assignmentParser =
        parse {
            do! pstring "SET" >>. justSpaces .>> pstring "(" .>> spaces
            let! variable = variableParser .>> spaces
            do! ignore_ws_str ","
            let! expr = lambdaExprParser .>> spaces
            do! ignore_ws_str ")"
            return Assign(variable, expr)
        } <?> "assignment statement"

    let ifParser = 
        parse {
            do! ignore_ws_str "if"
            let! condition = betweenParens lambdaExprParser .>> spaces
            let! thenBranch = lambdaStatementParser
            do! optional (pchar ';' .>> spaces)
            let! x = opt (attempt(spaces .>> pstring "else"))
            let! elseBranch = x |> function
                | Some _ -> spaces >>. lambdaStatementParser |>> Some
                | None -> parse { return None }
            return If(condition, thenBranch, elseBranch)
        } <?> "if statement"

    let returnParser = 
        parse {
            do! ignore_ws_str "return"
            let! expr = lambdaExprParser
            return Return expr
        } <?> "return statement"

    do lambdaStatementParserRef.Value <- justSpaces >>. choice [        
        assignmentParser
        blockParser
        ifParser
        returnParser
    ]

    let lambdaScriptParser: Parser<LambdaScript, unit> = spaces >>. choice [
        statementListParser |>> StatementList
        lambdaExprParser |>> Expression
    ]

open Internal

let parseLambdaScript s = 
    let result = run lambdaScriptParser s
    match result with
        | Success(s,_,_) -> s |> Result.Ok
        | Failure(f, _, _) -> f |> Result.Error


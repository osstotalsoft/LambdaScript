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
    | PropertyAccessor of Expr:LambdaExpr * PropertyName:string
    | Identifier of Id: string
    | UnaryOp of Op:(UnaryOperator * LambdaExpr)
    | BinaryOp of Op:(LambdaExpr * BinaryOperator * LambdaExpr)
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

type LambdaExprParserState = unit
type LambdaExprParser<'a> = Parser<'a, LambdaExprParserState>
type LambdaExprParser = LambdaExprParser<LambdaExpr>

module Internal =
    type Assoc = Associativity

    let ws = spaces
    let ignore_ws_str s = pstring s >>. ws
    let str = pstring
    let char = pchar
    let underscore = char '_'
    let atSign = char '@'

    let numericParser = many1Chars digit |>> (System.Decimal.Parse >> Number) <?> "number"

    let betweenQuotes = between (ignore_ws_str "\"") (ignore_ws_str "\"")
    let betweenParens = between (ignore_ws_str "(") (ignore_ws_str ")")
    let notQuoteChar = noneOf (Seq.toList "\"")
    let unquotedString = manyChars notQuoteChar
    let stringParser: LambdaExprParser = betweenQuotes unquotedString |>> String <?> "string"

    let identifierParser = 
        parse {
            let! first = letter <|> underscore <|> atSign
            let! rest = manyChars (letter <|> underscore <|> digit)
            return Identifier <| first.ToString() + rest
        } 
        <?> "identifier"

    let propertyAccessorParser = 
        parse {
            let! _ = pstring "."
            let! first = letter <|> underscore
            let! rest = manyChars (letter <|> underscore <|> digit)
            return first.ToString() + rest
        }
        <?> "property accessor"

    let chainedPropertyAccessorParser = 
        parse {
            let! identifier = identifierParser
            let! properties = many propertyAccessorParser
            return List.fold (fun expr prop -> PropertyAccessor (expr, prop)) identifier properties
        }
        <?> "identifier and or property accessor"

    let termParser = choice [
        stringParser
        numericParser
        chainedPropertyAccessorParser
    ]

    let opp = new OperatorPrecedenceParser<LambdaExpr,unit,unit>()
    let lambdaExprParser = opp.ExpressionParser
    opp.TermParser <- (termParser .>> ws) <|> betweenParens lambdaExprParser

    opp.AddOperator(PrefixOperator("!", ws, 1, true, fun x -> UnaryOp (Bang, x)))
    opp.AddOperator(InfixOperator("|", ws, 2, Assoc.Left, fun x y -> BinaryOp (x, Or, y)))
    opp.AddOperator(InfixOperator("&", ws, 3, Assoc.Left, fun x y -> BinaryOp (x, And, y)))

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


    let lambdaStatementParser, lambdaStatementParserRef = createParserForwardedToRef<LambdaStatement, Unit>()
    let variableParser: Parser<string, unit> = 
        parse {
            let! first = atSign
            let! rest = manyChars (letter <|> underscore <|> digit)
            return first.ToString() + rest
        } <?> "variable"

    let blockParser = 
        parse {
            do! ignore_ws_str "{"
            let! statements = sepEndBy1 lambdaStatementParser (many1 (pchar ';') >>. spaces)
            do! ignore_ws_str "}"
            return Block statements
        }
    let assignmentParser =
        parse {
            do! ignore_ws_str "SET("
            let! variable = variableParser .>> spaces
            do! ignore_ws_str ","
            let! expr = lambdaExprParser .>> spaces
            do! ignore_ws_str ")"
            return Assign(variable, expr)
        }

    let ifParser = 
        parse {
            do! ignore_ws_str "if"
            let! condition = betweenParens lambdaExprParser .>> spaces
            let! thenBranch = blockParser .>> spaces
            let! elseBranch = opt (ignore_ws_str "else" >>. blockParser)
            return If(condition, thenBranch, elseBranch)
        }

    do lambdaStatementParserRef.Value <- choice [
        assignmentParser
        blockParser
        ifParser
    ]

    let lambdaScriptParser: Parser<LambdaScript, unit> = choice [
        spaces >>. sepEndBy1 lambdaStatementParser (many1 (pchar ';') >>. spaces) |>> StatementList
        lambdaExprParser |>> Expression
    ]

open Internal

//let parseLambdaExpr s = 
//    let result = run lambdaExprParser s
//    match result with
//        | Success(s,_,_) -> s |> Result.Ok
//        | Failure(f, _, _) -> f |> Result.Error

//let parseLambdaStatement s = 
//    let result = run lambdaStatementParser s
//    match result with
//        | Success(s,_,_) -> s |> Result.Ok
//        | Failure(f, _, _) -> f |> Result.Error

let parseLambdaScript s = 
    let result = run lambdaScriptParser s
    match result with
        | Success(s,_,_) -> s |> Result.Ok
        | Failure(f, _, _) -> f |> Result.Error


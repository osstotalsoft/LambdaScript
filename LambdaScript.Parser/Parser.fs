module LambdaScript.Parser

open FParsec

type LambdaScript =
    | Expression of LambdaExpr
    | Statement of LambdaStatement
and LambdaStatement =
    | Set of Variable:string * Expr:LambdaExpr
    | If of Condition:LambdaExpr * Then:LambdaStatement * Else:LambdaStatement
    | Block of LambdaStatement list
    | Assign of Variable:string * Expr:LambdaExpr
    | Return of LambdaExpr
and LambdaExpr =  
    | Identifier of Id: string
    | Number of N:decimal
    | String of S:string
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
type Assoc = Associativity


let ws = spaces
let str_ws s = pstring s >>. ws
let str = pstring
let char = pchar
let underscore = char '_'
let atSign = char '@'




let numericParser = many1Chars digit |>> (System.Decimal.Parse >> Number) <?> "number"

let betweenQuotes = between (pstring "\"") (pstring "\"")
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

let termParser = stringParser
                <|> numericParser
                <|> identifierParser

let opp = new OperatorPrecedenceParser<LambdaExpr,unit,unit>()
let lambdaExprParser = opp.ExpressionParser
opp.TermParser <- (termParser .>> ws) <|> between (str_ws "(") (str_ws ")") lambdaExprParser

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
    }
let setParser: Parser<LambdaStatement, Unit> =
    parse {
        let! variable = variableParser .>> spaces
        let! _ = pstring "=" .>> spaces
        let! expr = lambdaExprParser .>> spaces
        return Set(variable, expr)
    }

do lambdaStatementParserRef.Value <- choice [
    setParser
]

let lambdaScriptParser: Parser<LambdaScript, unit> = choice [
    lambdaExprParser |>> Expression
    lambdaStatementParser |>> Statement  
]


let parseLambdaExpr s = 
    let result = run lambdaExprParser s
    match result with
        | Success(s,_,_) -> s |> Result.Ok
        | Failure(f, _, _) -> f |> Result.Error

let parseLambdaStatement s = 
    let result = run lambdaStatementParser s
    match result with
        | Success(s,_,_) -> s |> Result.Ok
        | Failure(f, _, _) -> f |> Result.Error

let parseLambdaScript s = 
    let result = run lambdaExprParser s
    match result with
        | Success(s,_,_) -> s |> Result.Ok
        | Failure(f, _, _) -> f |> Result.Error


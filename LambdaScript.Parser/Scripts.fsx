#r "nuget: FParsec"
#load "Parser.fs"
open LambdaScript.Parser

let showExpr s = sprintf "%s  ------> %A" s (parseLambdaExpr s)// |> printfn "%s"
let showStatement s = sprintf "%s  ------> %A" s (parseLambdaStatement s) //|> printfn "%s"
let showScript s = sprintf "%s  ------> %A" s (parseLambdaScript s) //|> printfn "%s"

"InstallmentNo == @ActualCurrentRate & ServiceCode == \"CASCO_A\"" |> showExpr
"InstallmentNo == (10 & ServiceCode == \"CASCO_A\")" |> showExpr
"(InstallmentNo == 10 & ServiceCode == \"CASCO_A\")" |> showExpr
"InstallmentNo == 10 & ServiceCode == \"CASCO_A\"" |> showExpr
"InstallmentNo & 10" |> showExpr
"InstallmentNo == 10" |> showExpr
"InstallmentNo" |> showExpr
"10" |> showExpr
"\"10\"" |> showExpr
"5 % 4" |> showExpr
"!IsActive" |> showExpr

"@InstallmentNo = 10" |> showStatement

"@InstallmentNo = 10" |> showScript

#r "nuget: FParsec"
#load "Parser.fs"
open LambdaScript.Parser

let showScript s = sprintf "%s  ------> %A" s (parseLambdaScript s) //|> printfn "%s"

"InstallmentNo == @ActualCurrentRate & ServiceCode == \"CASCO_A\"" |> showScript
"InstallmentNo == (10 & ServiceCode == \"CASCO_A\")" |> showScript
"(InstallmentNo == 10 & ServiceCode == \"CASCO_A\")" |> showScript
"InstallmentNo == 10 & ServiceCode == \"CASCO_A\"" |> showScript
"InstallmentNo & 10" |> showScript
"InstallmentNo == 10" |> showScript
"InstallmentNo" |> showScript
"10" |> showScript
"\"10\"" |> showScript
"5 % 4" |> showScript
"!IsActive" |> showScript
"Document.ServiceGenerationParams" |> showScript
"Document.Detail.Id" |> showScript

"ISNULL(@RunningAmount_CASCO_A,0)" |> showScript


"SET(@InstallmentNo, 10)" |> showScript
"SET(@InstallmentNo, 10);" |> showScript
"SET(@InstallmentNo, 10);SET(@XY, 1);" |> showScript
"SET(@InstallmentNo, 10) ;;  SET(@XY, 1);" |> showScript

"""
SET(@Unu, 1);
SET(@TipRata, 4);
SET(@ActualCurrentRate, 10);
SET(@ActualPeriodInterest, 1);
""" |> showScript

"""
{
    SET(@Unu, 1);
    SET(@TipRata, 4);
    SET(@ActualCurrentRate, 10);
    SET(@ActualPeriodInterest, 1);
}
""" |> showScript

"""
if(@TerminationServicePeriod > 0)
{
    SET(@Unu, 1);
    SET(@TipRata, 4);
    SET(@ActualCurrentRate, 10);
    SET(@ActualPeriodInterest, 1);
}
""" |> showScript

"""
if(@TerminationServicePeriod > 0)
{
    SET(@Unu, 1);
    SET(@TipRata, 4);
}
else{
    SET(@ActualCurrentRate, 10);
    SET(@ActualPeriodInterest, 1);
}
""" |> showScript

"""
if(@TerminationServicePeriod > 0)
    if(@TerminationServicePeriod > 0)
        SET(@Unu, 1);
    else
        SET(@TipRata, 4);
else
    SET(@ActualCurrentRate, 10);
""" |> showScript

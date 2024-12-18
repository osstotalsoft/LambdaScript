#r "nuget: FParsec"
#load "Parser.fs"
open LambdaScript.Parser
open LambdaScript.Parser.Internal
open FParsec

let showScript s = sprintf "%s  ------> %A" s (parseLambdaScript s) //|> printfn "%s"

"true" |> showScript

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
SET(@Unu, 1)
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
SET(@Unu, 1);
if (@TipRata != @TipRata)
{
return 0;
}
SET(@Unu, 1);
""" |> showScript

"""
if(@RemainingServMonths_CASCO_A <= 0) 
    SET (@Calc_CASCO_A_Interest, 0)
""" |> showScript

"""SET (@Calc_CASCO_A_Interest, 0)""" |> run assignmentParser


"""
if(@TerminationServicePeriod > 0)
{
return 0;
}
else{
if (@ActualCurrentRate < @ServiceFrom | @ActualCurrentRate > @ServiceTo)
{
return 0;
}
else{
return 1;
}
}
""" |> showScript



"""@RemSrvMonthsInPeriod_CASCO_A == 1 ? ISNULL(@MonthFactor_CASCO_A, 0) : 2""" |> showScript

"""SET(@ServiceType, [Document.ServiceDetail(ServiceCode == "CASCO_A").IsTotalValue])""" |> showScript

"""Document.ServiceDetail(ServiceCode == "CASCO_A").IsTotalValue""" |> showScript



"""
SET(@Unu, 1);
SET(@TipRata, 4);
SET(@ActualCurrentRate, 10);
SET(@ActualPeriodInterest, 1);


SET(@ServiceType, [Document.ServiceDetail(ServiceCode == "CASCO_A").IsTotalValue]);
SET(@ServiceIsFinanced, [Document.ServiceDetail(ServiceCode == "CASCO_A").FinancedYN]);
SET(@ServiceFrom, [Document.ServiceDetail(ServiceCode == "CASCO_A").FromScheduleNo]);
SET(@ServiceTo, [Document.ServiceDetail(ServiceCode == "CASCO_A").ToScheduleNo]);
SET(@ServicePeriodicity, [Document.ServiceDetail(ServiceCode == "CASCO_A").Periodicity]);
if (@TipRata != @TipRata)
{
return 0;
}
if (@ActualCurrentRate < @ServiceFrom | @ActualCurrentRate > @ServiceTo)
{
return 0;
}



SET(@RemainingServMonths_CASCO_A, ISNULL([Document.ServiceGenerationParams(InstallmentNo == @ActualCurrentRate & ServiceCode == "CASCO_A").RemainingServices], 0));
if(@RemainingServMonths_CASCO_A <= 0){SET (@Calc_CASCO_A_Interest, 0); return 0;}



SET(@CurrentPerMonthNo_CASCO_A, ISNULL([Document.ServiceGenerationParams(InstallmentNo == @ActualCurrentRate & ServiceCode == "CASCO_A").ServiceMonthNo], 0));
SET(@MonthFactor_CASCO_A, ISNULL([Document.ServiceGenerationParams(InstallmentNo == @ActualCurrentRate & ServiceCode == "CASCO_A").Factor], 0));
SET(@ServiceAmount, ISNULL([Document.ServiceDetail(ServiceCode == "CASCO_A").ServiceAmount],0));
SET(@RunningAmount_CASCO_A, ISNULL(@RunningAmount_CASCO_A,0));
SET(@CurrentPeriodNo, [Document.ServiceGenerationParams(InstallmentNo == @ActualCurrentRate & ServiceCode == "CASCO_A").PeriodNo]);
SET(@TerminationServicePeriod, ISNULL([Document.ServiceGenerationParams(InstallmentNo == @ActualCurrentRate & ServiceCode == "CASCO_A").TerminationServicePeriod],0));



if(@TerminationServicePeriod > 0)
{
SET(@ServiceAmount , ISNULL(@TerminationServicePeriod / 12 * @ServiceAmount,0));
}
else{
if(@ServiceType == -2002)
{
SET(@MaxServiceRate, MAX(MAX([Document.ServiceGenerationParams(ServiceCode == "CASCO_A" & PeriodNo == @CurrentPeriodNo).ServiceMonthNo]), 0));
SET(@ServiceAmount , ISNULL(@ServiceAmount, 0) * @MaxServiceRate/12 + ISNULL(@ServiceAmount, 0) * (@CurrentPeriodNo -1));
}
if(@ServiceType == -2003)
{
SET(@ServiceAmount , @CurrentPeriodNo * @ServiceAmount);
}
}
SET(@CurrentPrevious_CASCO_A, SUM([Document.Scadentar(SysMonthNo < @ActualStartRate).CASCO_A_Amount]));
SET(@Recompute_CASCO_A, ISNULL(@Recompute_CASCO_A, 1));
if(@CurrentPerMonthNo_CASCO_A == 1)
{
SET(@Recompute_CASCO_A, 1);
}
SET(@RemSrvMonthsInPeriod_CASCO_A, ISNULL([Document.ServiceGenerationParams(InstallmentNo == @ActualCurrentRate & ServiceCode == "CASCO_A").RemainingServices], 0));




SET(@RemainingServiceAmount_CASCO_A, @ServiceAmount - @CurrentPrevious_CASCO_A - @RunningAmount_CASCO_A);



if(@Recompute_CASCO_A == 1)
{
if(@ServiceIsFinanced == true & @CurrentPerMonthNo_CASCO_A > 0)
{
SET (@CASCO_A_PMT, ROUND(PMT(@RemainingServiceAmount_CASCO_A, @ActualPeriodInterest, @RemainingServMonths_CASCO_A, 0), 2));
SET(@Recompute_CASCO_A, 0);
}
else
{
SET(@CASCO_A_PMT, ROUND(@RemainingServiceAmount_CASCO_A/@RemainingServMonths_CASCO_A, 2));
SET(@Recompute_CASCO_A, 0);
}
}
SET(@Calc_CASCO_A_Principal, 0);
if ((@TipRata == @TipRata) & (@ActualCurrentRate >= @ServiceFrom) & (@ActualCurrentRate <= @ServiceTo))
{
SET (@Calc_CASCO_A_Interest, 0);
if(@ServiceIsFinanced == true)
{
SET(@Calc_CASCO_A_Interest, ROUND(ISNULL(@MonthFactor_CASCO_A, 0) * @RemainingServiceAmount_CASCO_A * @ActualPeriodInterest, 2));
}
SET(@Calc_CASCO_A_Principal, (@RemSrvMonthsInPeriod_CASCO_A == 1 ? ISNULL(@MonthFactor_CASCO_A, 0) *ISNULL(@RemainingServiceAmount_CASCO_A , 0) : ISNULL(@MonthFactor_CASCO_A, 0)*(ISNULL(@CASCO_A_PMT, 0) - @Calc_CASCO_A_Interest)));
}
else
{
SET(@Calc_CASCO_A_Interest, 0);
SET(@Calc_CASCO_A_Principal, 0);
}
SET(@RunningAmount_CASCO_A, @RunningAmount_CASCO_A + @Calc_CASCO_A_Principal);
return @Calc_CASCO_A_Principal;
""" |> showScript

"if (@a == 2) { SET(@v, 5); return 2 } return @a >= @b;"  |> showScript // ok

"""
{ 
    SET(@v, 5)    ;     
            ;;;;
            ;;;
            ;
            ;;
            ;;  ; ; ; 

                            return 2;

}
"""  |> showScript // not ok

"if (@a == 2) { SET(@v, 5); return 2; } return @a >= @b;"  |> showScript // not ok

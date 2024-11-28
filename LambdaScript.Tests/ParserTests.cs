using FluentAssertions;
using Microsoft.FSharp.Core;
using static LambdaScript.Parser;

namespace LambdaScript.Tests
{
    public class ParserTests
    {
        [Fact]
        public void Parse_IfThenElse()
        {
            var script =
            """
            if (@ActualCurrentRate < @ServiceFrom | @ActualCurrentRate > @ServiceTo)
                return 0;
            else{
                    return 1;
                }
            """;

            var r = Parser.parseLambdaScript(script);
            var elseBranch = r.IsOk && r.ResultValue switch
            {
                Parser.LambdaScript.StatementList { Item: var statements } => statements.ToList() switch
                {
                [LambdaStatement.If { Else: var elseB}] when FSharpOption<LambdaStatement>.get_IsSome(elseB) => true,
                    _ => false
                },
                _ => false
            };

            elseBranch.Should().BeTrue();
        }

        [Fact]
        public void Parse_IfThenElse_2()
        {
            var script =
            """
            if (@ActualCurrentRate < @ServiceFrom | @ActualCurrentRate > @ServiceTo) return 0 else return 1
            """;

            var r = Parser.parseLambdaScript(script);
            var elseBranch = r.IsOk && r.ResultValue switch
            {
                Parser.LambdaScript.StatementList { Item: var statements } => statements.ToList() switch
                {
                [LambdaStatement.If { Else: var elseB }] when FSharpOption<LambdaStatement>.get_IsSome(elseB) => true,
                    _ => false
                },
                _ => false
            };

            elseBranch.Should().BeTrue();
        }

        [Fact]
        public void Parse_CascoA()
        {
            var script =
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
            """;

            var r = Parser.parseLambdaScript(script);
            r.IsOk.Should().BeTrue();

        }
    }
}

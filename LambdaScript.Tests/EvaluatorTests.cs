using FluentAssertions;
using LambdaScript.Evaluator;
using Microsoft.VisualStudio.TestPlatform.CommunicationUtilities.Serialization;
using System.Linq.Expressions;
using System.Reflection.Metadata;

namespace LambdaScript.Tests;

public class EvaluatorTests
{
    [Fact]
    public void Evaluate_SimpleScript()
    {
        var script = "1 + 2";
        var ctx = new Evaluator.ExecutionContext { Document = new { } };
        var evaluator = new LambdaEvaluator();
        var result1 = evaluator.Evaluate<int>(script, ctx);
        var result2 = evaluator.Evaluate<int?>(script, ctx);
        var result3 = evaluator.Evaluate<decimal>(script, ctx);
        var result4 = evaluator.Evaluate<decimal?>(script, ctx);
        var result5 = evaluator.Evaluate<string>(script, ctx);

        result1.Should().Be(3);
        result2.Should().Be(3);
        result3.Should().Be(3);
        result4.Should().Be(3);
        result5.Should().Be("3");
    }

    [Fact]
    public void Evaluate_Chained_PropertyAccessor()
    {
        var document = new
        {
            Person = new
            {
                Name = "John"
            }
        };
        var ctx = new Evaluator.ExecutionContext { Document = document };
        var res = new LambdaEvaluator().Evaluate<string>("Document.Person.Name", ctx);
        res.Should().Be(document.Person.Name);
    }

    [Fact]
    public void Evaluate_Variable_Set()
    {
        var ctx = new Evaluator.ExecutionContext { Document = new { } };
        var res = new LambdaEvaluator().Evaluate<string>("""SET(@Unu, "sdfsdf")""", ctx);
        res.Should().Be(null);
    }


    [Fact]
    public void Evaluate_Early_Return_Test()
    {
        var ctx = new Evaluator.ExecutionContext { Document = new { } };
        var res = new LambdaEvaluator().Evaluate<decimal>("""return 0; return 1;""", ctx);
        res.Should().Be(0);
    }

    [Fact]
    public void Evaluate_Some_Script_1()
    {
        var ctx = new Evaluator.ExecutionContext { Document = new { } };
        var res = new LambdaEvaluator().Evaluate<decimal>("""SET(@Unu, "sdfsdf"); return 0;""", ctx);
        res.Should().Be(0);
    }


    [Fact]
    public void Evaluate_Some_Script_2()
    {
        var script =
        """
            SET(@a, 1);
            SET(@b, 2);
            SET(@c, 3);
            return @a + @b + @c;
        """;

        var ctx = new Evaluator.ExecutionContext { Document = new { } };
        var res = new LambdaEvaluator().Evaluate<decimal>(script, ctx);
        res.Should().Be(1+2+3);
    }

    //[Fact]
    //public void Test1()
    //{
    //    // Define a parameter expression for the variable 'x'
    //    ParameterExpression x = Expression.Parameter(typeof(int), "x");

    //    // Define a constant expression for the value '5'
    //    ConstantExpression value = Expression.Constant(5, typeof(int));

    //    // Create an assignment expression 'x = 5'
    //    BinaryExpression assignExpr = Expression.Assign(x, value);

    //    // Create a block expression to include the assignment
    //    BlockExpression blockExpr = Expression.Block(
    //        new[] { x }, // Declare the variable 'x'
    //        assignExpr   // Include the assignment expression
    //    );

    //    // Compile the block expression into a lambda and execute it
    //    var lambda = Expression.Lambda<Func<int>>(blockExpr).Compile();
    //    var result = lambda();

    //    result.Should().Be(5);


    //}


}

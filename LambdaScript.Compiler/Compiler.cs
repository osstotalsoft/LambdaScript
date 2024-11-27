using LambdaScript.ClassLibrary;
using System.Linq.Expressions;
using System.Reflection;
using static LambdaScript.Parser;

namespace LambdaScript.Compiler;

public class Compiler
{
    private static ParameterExpression ctx = Expression.Parameter(typeof(IExecutionContext), "ctx");
    private static MethodInfo getVariable = typeof(IExecutionContext).GetMethod(nameof(IExecutionContext.GetVariable), [typeof(string)])!;

    private static MethodInfo eq = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.Eq), BindingFlags.Static | BindingFlags.Public)!;
    private static MethodInfo notEq = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.NotEq), BindingFlags.Static | BindingFlags.Public)!;
    private static MethodInfo lt = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.Lt), BindingFlags.Static | BindingFlags.Public)!;
    private static MethodInfo lte = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.Lte), BindingFlags.Static | BindingFlags.Public)!;
    private static MethodInfo gt = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.Gt), BindingFlags.Static | BindingFlags.Public)!;
    private static MethodInfo gte = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.Gte), BindingFlags.Static | BindingFlags.Public)!;

    private static MethodInfo plus = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.Plus), BindingFlags.Static | BindingFlags.Public)!;
    private static MethodInfo minus = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.Minus), BindingFlags.Static | BindingFlags.Public)!;
    private static MethodInfo mult = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.Mult), BindingFlags.Static | BindingFlags.Public)!;
    private static MethodInfo divide = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.Divide), BindingFlags.Static | BindingFlags.Public)!;
    private static MethodInfo modulo = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.Modulo), BindingFlags.Static | BindingFlags.Public)!;

    private static Expression From(LambdaExpr predicate) => predicate switch
    {
        LambdaExpr.Identifier { Id: var i } /*when i.StartsWith("@")*/ => Expression.Call(ctx, getVariable, Expression.Constant(i)),
        //LambdaExpr.Identifier { Id: var i } => Expression.Call(document, getPropertyValue, Expression.Constant(i)),
        LambdaExpr.String { S: var s } => Expression.Constant(s, typeof(object)),
        LambdaExpr.Number { N: var n } => Expression.Constant(n, typeof(object)),
        LambdaExpr.UnaryOp { Op: var (op, x) } => op switch
        {
            _ => Expression.Not(Expression.Convert(From(x), typeof(bool?)))
        },
        LambdaExpr.BinaryOp { Op: var (left, op, right) } => op switch
        {
            { IsAnd: true } => Expression.AndAlso(Expression.Convert(From(left), typeof(bool?)), Expression.Convert(From(right), typeof(bool?))),
            { IsOr: true } => Expression.OrElse(Expression.Convert(From(left), typeof(bool?)), Expression.Convert(From(right), typeof(bool?))),
            { IsEq: true } => Expression.Call(null, eq, From(left), From(right)),
            { IsNotEq: true } => Expression.Call(null, notEq, From(left), From(right)),
            { IsLt: true } => Expression.Call(null, lt, From(left), From(right)),
            { IsLte: true } => Expression.Call(null, lte, From(left), From(right)),
            { IsGt: true } => Expression.Call(null, gt, From(left), From(right)),
            { IsGte: true } => Expression.Call(null, gte, From(left), From(right)),
            { IsPlus: true } => Expression.Call(null, minus, From(left), From(right)),
            { IsMinus: true } => Expression.Call(null, minus, From(left), From(right)),
            { IsMult: true } => Expression.Call(null, mult, From(left), From(right)),
            { IsDivide: true } => Expression.Call(null, divide, From(left), From(right)),
            { IsMod: true } => Expression.Call(null, modulo, From(left), From(right)),
            _ => throw new Exception($"Unknown binary operation {op}")
        },

        var e => throw new Exception($"Unknown expression of type {e.GetType()}")
    };

    private static Expression FromStatement(LambdaStatement statement)
    {
        return statement switch
        {
            LambdaStatement.Block { Item: var statements } => FromStatements(statements),
            LambdaStatement.Assign { Variable: var variableName, Expr: var exp } => 
                Expression.Assign(
                    Expression.Variable(typeof(object), variableName), 
                    From(exp)
                ),
            LambdaStatement.If { Condition: var condition, Then: var thenBranch, Else: var elseBranch } =>
                Expression.Condition(
                    From(condition),
                    FromStatement(thenBranch),
                    elseBranch == null ? Expression.Empty() : FromStatement(elseBranch.Value)
                ),
            LambdaStatement.Return { Item: var exp } => Expression.Convert(From(exp), typeof(object)),
            _ => throw new Exception($"Unknown statement of type {statement}")
        };
    }

    private static Expression FromStatements(IEnumerable<LambdaStatement> statements)
    {
        if (statements.Count() == 0)
            return Expression.Block(Expression.Empty());

        var expressions = statements.Select(stmt => FromStatement(stmt)).ToList();

        return Expression.Block(expressions);
    }

    private static Expression FromScript(Parser.LambdaScript script) => script switch
    {
        Parser.LambdaScript.Expression { Item: var expression } => From(expression),
        Parser.LambdaScript.StatementList { Item: var statements } => FromStatements(statements),
        _ => throw new Exception($"Unknown script of type {script}")
    };

    private static Func<IExecutionContext, T> Compile<T>(string expression)
    {
        var resultOrError = parseLambdaScript(expression);
        if (resultOrError.IsError)
        {
            throw new Exception(resultOrError.ErrorValue);
        }

        var result = resultOrError.ResultValue;
        var bodyExpr = Expression.Convert(FromScript(result), typeof(T));

        var expr = Expression.Lambda<Func<IExecutionContext, T>>(bodyExpr, ctx);
        var predicate = expr.Compile();
        return predicate;
    }
}

using System.Collections.ObjectModel;
using System.Linq.Expressions;
using System.Reflection;
using static LambdaScript.Parser;

namespace LambdaScript.Evaluator;

public class CompilationContext
{
    private Dictionary<string, ParameterExpression> variables = [];
    private LabelTarget returnTarget = Expression.Label(typeof(object));

    public ParameterExpression GetVariable(string name)
    {
        if (!variables.TryGetValue(name, out var variable))
        {
            variable = Expression.Variable(typeof(object), name);
            variables.Add(name, variable);
        }
        return variable;
    }

    public IReadOnlyCollection<ParameterExpression> GetAllVariables() => [..variables.Values];
    public LabelTarget GetReturnTarget() => returnTarget;
}

public class Compiler
{
    private static ParameterExpression executionContext = Expression.Parameter(typeof(IExecutionContext), "ctx");
    private static MethodInfo getPropertyValue = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.GetPropertyValue), BindingFlags.Static | BindingFlags.Public)!;
    private static MethodInfo filter = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.Filter), BindingFlags.Static | BindingFlags.Public)!;
    private static MethodInfo coerce = typeof(ObjAlgebra).GetMethod(nameof(ObjAlgebra.Coerce), BindingFlags.Static | BindingFlags.Public)!;
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

    private static Expression From(LambdaExpr predicate, CompilationContext cc) => predicate switch
    {
        LambdaExpr.Identifier { Id: var identifier } when identifier.StartsWith("@") => cc.GetVariable(identifier),
        LambdaExpr.Identifier { Id: var identifier } => Expression.Call(null, getPropertyValue, executionContext, Expression.Constant(identifier)),
        LambdaExpr.String { S: var s } => Expression.Constant(s, typeof(object)),
        LambdaExpr.Number { N: var n } => Expression.Constant(n, typeof(object)),
        LambdaExpr.UnaryOp { Op: var (op, x) } => op switch
        {
            _ => Expression.Convert(Expression.Not(Expression.Convert(From(x, cc), typeof(bool?))), typeof(object)),
        },
        LambdaExpr.BinaryOp { Op: var (left, op, right) } => op switch
        {
            { IsAnd: true } => Expression.Convert(Expression.AndAlso(Expression.Convert(From(left, cc), typeof(bool?)), Expression.Convert(From(right, cc), typeof(bool?))), typeof(object)),
            { IsOr: true } => Expression.Convert(Expression.OrElse(Expression.Convert(From(left, cc), typeof(bool?)), Expression.Convert(From(right, cc), typeof(bool?))), typeof(object)),
            { IsEq: true } => Expression.Call(null, eq, From(left, cc), From(right, cc)),
            { IsNotEq: true } => Expression.Call(null, notEq, From(left, cc), From(right, cc)),
            { IsLt: true } => Expression.Call(null, lt, From(left, cc), From(right, cc)),
            { IsLte: true } => Expression.Call(null, lte, From(left, cc), From(right, cc)),
            { IsGt: true } => Expression.Call(null, gt, From(left, cc), From(right, cc)),
            { IsGte: true } => Expression.Call(null, gte, From(left, cc), From(right, cc)),
            { IsPlus: true } => Expression.Call(null, plus, From(left, cc), From(right, cc)),
            { IsMinus: true } => Expression.Call(null, minus, From(left, cc), From(right, cc)),
            { IsMult: true } => Expression.Call(null, mult, From(left, cc), From(right, cc)),
            { IsDivide: true } => Expression.Call(null, divide, From(left, cc), From(right, cc)),
            { IsMod: true } => Expression.Call(null, modulo, From(left, cc), From(right, cc)),
            _ => throw new Exception($"Unknown binary operation {op}")
        },
        LambdaExpr.PropertyAccessor { Expr: var expr, PropertyName: var propertyName }
            => Expression.Call(null, getPropertyValue, From(expr, cc), Expression.Constant(propertyName)),

        LambdaExpr.CollectionFilter { CololectionExpr: var collection, Predicate: var pred }
            => Expression.Call(null, filter, executionContext, From(collection, cc), FromPredicate(pred, cc)),

        var e => throw new Exception($"Unknown expression of type {e.GetType()}")
    };

    private static Expression FromPredicate(LambdaExpr predicate, CompilationContext cc)
    {
        var bodyExpr = Expression.Convert(From(predicate, cc), typeof(bool));
        var expr = Expression.Lambda<Func<IExecutionContext, bool>>(bodyExpr, executionContext);
        return expr;
    }

    private static Expression FromStatement(LambdaStatement statement, CompilationContext cc) => statement switch
    {
        LambdaStatement.Block { Item: var statements } 
            => Expression.Block(statements.Select(s => FromStatement(s, cc))),
        LambdaStatement.Assign { Variable: var variableName, Expr: var exp } 
            => Expression.Assign(cc.GetVariable(variableName), From(exp, cc)),
        LambdaStatement.If { Condition: var condition, Then: var thenBranch, Else: var elseBranch } =>
            Expression.Condition(
                Expression.Convert(From(condition, cc), typeof(bool)),
                FromStatement(thenBranch, cc),
                elseBranch == null ? Expression.Empty() : FromStatement(elseBranch.Value, cc)
            ),
        LambdaStatement.Return { Item: var exp } => Expression.Return(cc.GetReturnTarget(), From(exp, cc), typeof(object)),
        _ => throw new Exception($"Unknown statement of type {statement}")
    };


    private static Expression FromScript(Parser.LambdaScript script, CompilationContext cc)
    {
        var exprs = script switch
        {
            Parser.LambdaScript.Expression { Item: var expression } => [Expression.Return(cc.GetReturnTarget(), From(expression, cc), typeof(object))],
            Parser.LambdaScript.StatementList { Item: var statements } => statements.Select(s=> FromStatement(s,cc)).ToList(),
            _ => throw new Exception($"Unknown script of type {script}")
        };
        var variables = cc.GetAllVariables();
        return Expression.Block(variables, [..exprs, Expression.Label(cc.GetReturnTarget(), Expression.Constant(null))]);
    }

    public static Func<IExecutionContext, TResult> Compile<TResult>(Parser.LambdaScript script)
    {
        var cc = new CompilationContext();
        var bodyExpr = Expression.Convert(
            Expression.Call(null, coerce, FromScript(script, cc), Expression.Constant(typeof(TResult))),
            typeof(TResult));

        var expr = Expression.Lambda<Func<IExecutionContext, TResult>>(bodyExpr, executionContext);
        var fn = expr.Compile();
        return fn;
    }
}

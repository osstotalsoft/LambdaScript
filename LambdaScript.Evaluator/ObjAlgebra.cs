namespace LambdaScript.Evaluator;

public static class ObjAlgebra
{
    public static object? Filter(IExecutionContext executionCtx, object collection, Func<IExecutionContext, bool> predicate)
    {
        var col = (IEnumerable<object>)collection;
        foreach (var item in col)
        {
            if (predicate(executionCtx.SetFilterContext(item)))
            {
                return item;
            }
        }
        return null;
    }

    public static object IfThenElse(object condition, object then, object @else) =>
         TryChangeType(condition, typeof(bool)) switch
         {
             (bool c, true) => c ? then : @else,
             _ => throw new InvalidCastException("Condition must be a boolean")
         };

    public static object IsNull(object a) => a switch
    {
        null => true,
        _ => false
    };

    public static object Coalesce(object a, object b) => a switch
    {
        null => b,
        _ => a
    };

    public static object? GetPropertyValue(object obj, string prop) => obj switch
    {
        IPropertyAccessor accessor => accessor.GetPropertyValue(prop),
        //use reflection to get property value
        _ => (obj.GetType().GetProperty(prop) ?? throw new Exception($"Property {prop} not found"))?.GetValue(obj),
    };

    public static object? Coerce(object value, Type conversionType) =>
        TryChangeType(value, conversionType) switch
        {
            (var result, true) => result,
            _ => throw new InvalidCastException($"Cannot convert {value ?? "null"} to {conversionType}")
        };

    public static object Eq(object a, object b) => (a, b) switch
    {
        (null, null) => true,
        (null, _) => false,
        (_, null) => false,
        _ => a.ToString() == b.ToString()
    };

    public static object NotEq(object a, object b) => (a, b) switch
    {
        (null, null) => false,
        (null, _) => true,
        (_, null) => true,
        _ => a.ToString() != b.ToString()
    };

    public static object Lt(object a, object b) =>
        TryChangeTypes(a, b) is var (a1, b1, ok) &&
        ok &&
        a1 is IComparable a2 &&
        a2.CompareTo(b1) < 0;

    public static object Lte(object a, object b) =>
        TryChangeTypes(a, b) is var (a1, b1, ok) &&
        ok &&
        a1 is IComparable a2 &&
        a2.CompareTo(b1) <= 0;

    public static object Gt(object a, object b) =>
        TryChangeTypes(a, b) is var (a1, b1, ok) &&
        ok &&
        a1 is IComparable a2 &&
        a2.CompareTo(b1) > 0;

    public static object Gte(object a, object b) =>
        TryChangeTypes(a, b) is var (a1, b1, ok) &&
        ok &&
        a1 is IComparable a2 &&
        a2.CompareTo(b1) >= 0;

    public static object Plus(object a, object b) =>
        Convert.ToDecimal(a) + Convert.ToDecimal(b);

    public static object Minus(object a, object b) =>
        Convert.ToDecimal(a) - Convert.ToDecimal(b);

    public static object Mult(object a, object b) =>
        Convert.ToDecimal(a) * Convert.ToDecimal(b);

    public static object Divide(object a, object b) =>
        Convert.ToDecimal(a) / Convert.ToDecimal(b);

    public static object Modulo(object a, object b) =>
        Convert.ToDecimal(a) % Convert.ToDecimal(b);


    private static bool CanChangeType(object value, Type conversionType) =>

        conversionType is not null &&
        (value is IConvertible v && v is not null);

    private static (object? Result, bool Success) TryChangeType(object value, Type conversionType) => value switch
    {
        null when conversionType.IsValueType => (value, false),
        null => (value, true),
        _ when value.GetType() == conversionType => (value, true),
        _ => CanChangeType(value, conversionType) switch
        {
            true => Nullable.GetUnderlyingType(conversionType) switch
            {
                null => (Convert.ChangeType(value, conversionType), true),
                var underlyingType => (Convert.ChangeType(value, underlyingType), true)
            },
            _ => (value, false)
        }
    };


    private static (object? ResultA, object? ResultB, bool Success) TryChangeTypes(object a, object b) =>
        (a, b) switch
        {
            (null, _) => (a, b, false),
            (_, null) => (a, b, false),
            _ => (a.GetType(), b.GetType()) switch
            {
                (var aType, var bType) when aType == bType => (a, b, true),
                (var aType, var bType) => TryChangeType(a, bType) switch
                {
                    (var a1, true) => (a1, b, true),
                    _ => TryChangeType(b, aType) switch
                    {
                        (var b1, true) => (a, b1, true),
                        _ => (a, b, false)
                    }
                }
            }
        };
}

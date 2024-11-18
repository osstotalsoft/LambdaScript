namespace LambdaScript.ClassLibrary
{
    public static class ObjAlgebra
    {
        public static bool Eq(object a, object b) => (a, b) switch
        {
            (null, null) => true,
            (null, _) => false,
            (_, null) => false,
            _ => a.ToString() == b.ToString()
        };

        public static bool NotEq(object a, object b) => (a, b) switch
        {
            (null, null) => false,
            (null, _) => true,
            (_, null) => true,
            _ => a.ToString() != b.ToString()
        };

        public static bool Lt(object a, object b) =>
            TryChangeTypes(a, b) is var (a1, b1, ok) &&
            ok &&
            a1 is IComparable a2 &&
            a2.CompareTo(b1) < 0;

        public static bool Lte(object a, object b) =>
            TryChangeTypes(a, b) is var (a1, b1, ok) &&
            ok &&
            a1 is IComparable a2 &&
            a2.CompareTo(b1) <= 0;

        public static bool Gt(object a, object b) =>
            TryChangeTypes(a, b) is var (a1, b1, ok) &&
            ok &&
            a1 is IComparable a2 &&
            a2.CompareTo(b1) > 0;

        public static bool Gte(object a, object b) =>
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
            value is IConvertible v &&
            v is not null;

        private static (object Result, bool Success) TryChangeType(object value, Type conversionType) =>
            CanChangeType(value, conversionType)
                ? (System.Convert.ChangeType(value, conversionType), true)
                : (value, false);

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
}

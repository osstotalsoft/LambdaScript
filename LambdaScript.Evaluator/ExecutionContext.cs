

namespace LambdaScript.Evaluator;

public interface IPropertyAccessor
{
    object? GetPropertyValue(string name);
}

public interface IExecutionContext : IPropertyAccessor
{
    object GetLambdaContext();
}

public class ExecutionContext : IExecutionContext
{

    public required object Document { get; init; }
    public object GetLambdaContext()
    {
        throw new NotImplementedException();
    }

    public object? GetPropertyValue(string name) => name switch
    {
        "Document" => Document,
        _ => throw new Exception($"Property {name} not found")
    };
}

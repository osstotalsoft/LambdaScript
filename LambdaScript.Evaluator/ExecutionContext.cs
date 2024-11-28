

namespace LambdaScript.Evaluator;

public interface IPropertyAccessor
{
    object? GetPropertyValue(string name);
}

public interface IExecutionContext : IPropertyAccessor
{
    IExecutionContext SetFilterContext(object item);
}

public class ExecutionContext : IExecutionContext
{
    public object? FilterContext { get; init; }
    public required object Document { get; init; }

    public object? GetPropertyValue(string name) => name switch
    {
        "Document" => Document,
        _ when FilterContext is not null => ObjAlgebra.GetPropertyValue(FilterContext, name),
        _ => throw new Exception($"Property {name} not found")
    };

    public IExecutionContext SetFilterContext(object item)
        => new ExecutionContext { Document = Document, FilterContext = item };
}

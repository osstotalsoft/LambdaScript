namespace LambdaScript.ClassLibrary;


public interface IPropertyAccessor
{
    object GetPropertyValue(string name);
}

public interface IExecutionContext: IPropertyAccessor
{
    object GetVariable(string name);
    void SetVariable(string name, object value);

    IPropertyAccessor? GetLambdaContext();
}

namespace LambdaScript.ClassLibrary;

public interface IExecutionContext
{
    object GetVariable(string name);
    void SetVariable(string name, object value);
}

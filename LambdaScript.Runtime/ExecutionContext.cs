using LambdaScript.ClassLibrary;

namespace LambdaScript.Runtime
{
    public class ExecutionContext : IExecutionContext
    {
        public Dictionary<string, object> Variables { get; } = new();
        public object GetVariable(string name) => Variables[name];

        public void SetVariable(string name, object value) => Variables[name] = value;
    }
}

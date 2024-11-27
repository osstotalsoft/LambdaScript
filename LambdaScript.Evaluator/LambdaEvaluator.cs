


namespace LambdaScript.Evaluator;

public class LambdaEvaluator
{
    public TResult Evaluate<TResult>(string script, IExecutionContext ctx)
    {
        var parserResult = Parser.parseLambdaScript(script);
        if (parserResult.IsError)
        {
            throw new ParseException(parserResult.ErrorValue);
        }

        Func<IExecutionContext, TResult> fn;

        try
        {
            fn = Compiler.Compile<TResult>(parserResult.ResultValue);
        }
        catch (Exception e)
        {
            throw new CompileException("Script compilation exception.", e);
        }

        try
        {
            var result = fn(ctx);
            return result;
        }
        catch (Exception e)
        {
            throw new EvaluationException("Script evaluation exception.", e);
        }

    }
}

[Serializable]
public class ParseException : Exception
{
    public ParseException()
    {
    }

    public ParseException(string? message) : base(message)
    {
    }

    public ParseException(string? message, Exception? innerException) : base(message, innerException)
    {
    }
}

[Serializable]
public class CompileException : Exception
{
    public CompileException()
    {
    }

    public CompileException(string? message) : base(message)
    {
    }

    public CompileException(string? message, Exception? innerException) : base(message, innerException)
    {
    }
}

[Serializable]
internal class EvaluationException : Exception
{
    public EvaluationException()
    {
    }

    public EvaluationException(string? message) : base(message)
    {
    }

    public EvaluationException(string? message, Exception? innerException) : base(message, innerException)
    {
    }
}
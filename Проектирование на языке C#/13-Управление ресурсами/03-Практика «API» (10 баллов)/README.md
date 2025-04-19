# Практика «API»

Используем паттерн Disposable в ситуации, схожей с работой со внешним API. В [проекте Memory.API](Memory.API.zip) в роли внешнего API выступает класс MagicAPI, методы которого позволяют выделить ресурс, освободить его, и проверить, какие ресурсы выделены в настоящий момент.

Реализуйте класс APIObject, который будет оберткой над API. Выделение ресурса через внешнее API должна происходить в конструкторе, а освобождение - в соответствие с паттерном Disposable.

Все тесты пройдены, задача сдана:
```cs
namespace Memory.API;

public class APIObject : IDisposable
{
    private int id;
    
    public APIObject(int n)
    {
        id = n;
        MagicAPI.Allocate(n);
    }
    
    private bool isDisposed = false;
    
    ~APIObject() => Dispose(false);
    
    public void Dispose()
    {
        Dispose(true);
        GC.SuppressFinalize(this);
    }
    
    protected virtual void Dispose(bool fromDisposeMethod)
    {
        if (!isDisposed)
        {
            MagicAPI.Free(this.id);
            isDisposed = true;
        }
    }
}
```

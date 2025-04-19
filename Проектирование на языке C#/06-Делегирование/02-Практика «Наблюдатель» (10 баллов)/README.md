# Практика «Наблюдатель»

Делегаты и их производные (такие, как события) можно использовать для замены классического объектно-ориентированного шаблона [Наблюдатель](https://en.wikipedia.org/wiki/Observer_pattern). Этот шаблон реализован в [проекте Delegates.Observers](Delegates.Observers.zip) в файле ObservableStack.cs в классическом виде, так как он описан в книжках.

Видно, как много инфраструктурного кода необходимо для обеспечения очень несложной функциональности в чистом ООП.

Вспомните, что такое событие в языке C#, и отрефакторьте код с его использованием. Сравните объём кода с классической реализацией шаблона проектирования Наблюдатель.

Все тесты пройдены, задача сдана:
```cs
using System.Text;

namespace Delegates.Observers;

public class StackOperationsLogger
{
    public StringBuilder Log = new();
    public void SubscribeOn<T>(ObservableStack<T> stack)
    {
        stack.OnStackChanged += (sender, eventData) => Log.Append(eventData);
    }
    
    public string GetLog() => Log.ToString();
}

public class ObservableStack<T>
{
    List<T> data = new();
    
    public event EventHandler<object>? OnStackChanged;
    
    protected void Notify(object eventData)
        => OnStackChanged?.Invoke(this, eventData);
    
    public void Push(T obj)
    {
        data.Add(obj);
        Notify(new StackEventData<T> { IsPushed = true, Value = obj });
    }
    
    public T Pop()
    {
        if (data.Count == 0)
            throw new InvalidOperationException();
        var result = data[data.Count - 1];
        Notify(new StackEventData<T> { IsPushed = false, Value = result });
        return result;
    }
}
```

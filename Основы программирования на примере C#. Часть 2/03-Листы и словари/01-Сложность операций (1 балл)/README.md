# Сложность операций

Изучите следующий код:

```cs
public class List<T> : IEnumerable<T>
{
    T[] collection = new T[100];
    int count = 0;
   
    public void Add(T value)
    {
        if (count == collection.Length)
        {
            var enlargedCollection = new T[count * 2];
            Array.Copy(collection, enlargedCollection, collection.Length);
            collection = enlargedCollection;
        }
        collection[count++] = value;
    }
   
    public T this[int index]
    {
        get { return collection[index]; }
        set { collection[index] = value; }
    }
    
    public bool Contains(T value)
    {
        for (int i = 0; i < count; i++)
            if (collection[i].Equals(value)) return true;
        return false;
    }
}
```

1. Что вы можете сказать об этом коде? (1 из 1 балла)
   * ❌ **Сложность операции Add всегда &Theta;(1)**
   * ✅ **Сложность операции доступа к элементу по индексу &Theta;(1)** (Правильно!)
   * ✅ **Сложность операции Contains &Theta;(n)** (Правильно!)

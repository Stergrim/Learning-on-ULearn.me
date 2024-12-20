# Динамический массив

В C#, Java и других языках обычно есть класс, представляющий собой список, в который можно добавлять новые элементы. Внутри из соображений эффективности его часто реализуют с помощью массива, в котором зарезервировано места с запасом. Как только место заканчивается, выделяется место под больший массив и все элементы копируются в него.

Часто такой трюк реализации называют динамический массив. В C# эта концепция встречается в нескольких классах и методах: List, ArrayList, в методе ToArray() и, наверняка, ещё много где.

Вот так это может быть реализовано:

```cs
class DynamicArray
{
    public int Count = 0;
    public int[] Items = new int[1];
   
    public void Add(int x)
    {
        if (Count >= Items.Length)
        {
            int[] newItems = new int[2 * Items.Length];
            for (int j = 0; j < Count; j++)
                newItems[j] = Items[j];
            Items = newItems;
        }
        Items[Count] = x;
        Count++;
    }
   
    public void AddRange(int n)
    {
        for (int i = 0; i < n; i++)
            Add(i);
    }
}
```

1. Какая сложность у метода AddRange? (1 из 1 балла)
   * 🔴 **Θ(log(n))**
   * 🟢 **Θ(n)** (Правильно! Предыдущая задача — это упрощение этой, более реалистичной. Поэтому сложность та же)
   * 🔴 **Θ(n log(n))**
   * 🔴 **Θ(n²)**

# Практика «Бинарное дерево»

Наиболее очевидный случай использования дженериков — создание коллекций. Скачайте [проект Generics.BinaryTrees](Generics.BinaryTrees.zip) и создайте в нем класс [бинарного дерева поиска](https://ru.wikipedia.org/wiki/%D0%94%D0%B2%D0%BE%D0%B8%D1%87%D0%BD%D0%BE%D0%B5_%D0%B4%D0%B5%D1%80%D0%B5%D0%B2%D0%BE_%D0%BF%D0%BE%D0%B8%D1%81%D0%BA%D0%B0) так, чтобы он проходил приложенные тесты.

**Защита целостности**

Класс должен защищать целостность. То есть действиями извне должно быть невозможно нарушить основное свойство дерева поиска. Например, такой код не должен работать:

```cs
var tree = new BinaryTree<int>();
...
tree.Left.Left.Value = 100500;
```

Все тесты пройдены, задача сдана:
```cs
using System.Collections;

namespace Generics.BinaryTrees;

public class BinaryTree<T> : IEnumerable<T>
    where T : IComparable
{
    private SubTree<T> root;
    
    public T Value { get => root.Value; }
    public SubTree<T> Left { get => root.Left; }
    public SubTree<T> Right { get => root.Right; }
    
    public void Add(T key)
    {
        var end = root;
        if (root == null) root = new SubTree<T>(key);
        else
            while (true)
            {
                end.Size++;
                if (end.Value.CompareTo(key) >= 0)
                    if (end.Left == null)
                    {
                        end.Left = new SubTree<T>(key);
                        break;
                    }
                    else end = end.Left;
                else
                    if (end.Right == null)
                    {
                        end.Right = new SubTree<T>(key);
                        break;
                    }
                else end = end.Right;
            }
    }
    
    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    
    public IEnumerator<T> GetEnumerator() => GetValues(root).GetEnumerator();
    
    private static IEnumerable<T> GetValues(SubTree<T> root)
    {
        while (true)
        {
            if (root == null) yield break;
            foreach (var e in GetValues(root.Left))
                yield return e;
    
            yield return root.Value;
            root = root.Right;
        }
    }
}


public class BinaryTree
{
    public static BinaryTree<int> Create(params int[] array)
    {
        var tree = new BinaryTree<int>();
    
        foreach (var item in array)
            tree.Add(item);
    
        return tree;
    }
}

public class SubTree<T>
{
    public T Value { get; }
    public SubTree<T> Left { get; set; }
    public SubTree<T> Right { get; set; }
    public SubTree(T value) { Value = value; }
    
    public int Size = 1;
}
```

# Практика «Add и Contains»

В этой и следующей задаче нужно будет реализовать структуру данных «Бинарное дерево поиска».

[Скачайте проект](BinaryTrees.zip). Реализуйте бинарное дерево так, как это описано в видеолекциях, с двумя операциями:

```cs
Add(T key);
bool Contains(T key);
```

Эти два метода должны быть реализованы без использования рекурсии.

Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections;
using System.Collections.Generic;

namespace BinaryTrees;

public class BinaryTree<T> where T : IComparable
{
    private SubTree<T> root;
    
    public void Add(T key)
    {
        var end = root;
        if (root == null) root = new SubTree<T>(key);
        else
            while (true)
            {
                end.Size++;
                if (end.Value.CompareTo(key) > 0)
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
    
    public bool Contains(T key)
    {
        var newRoot = root;
        while (newRoot != null)
        {
            var result = newRoot.Value.CompareTo(key);
            if (result == 0) return true;
            newRoot = result > 0 ? newRoot.Left : newRoot.Right;
        }
        return false;
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

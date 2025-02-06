# Практика «Enumerable и Индексатор»

Продолжайте [в том же проекте](BinaryTrees.zip). В этой задаче вам предстоит реализовать ещё две операции с деревом.

Реализуйте интерфейс `IEnumerable<T>` в вашем дереве так, чтобы перечисление элементов происходило в порядке их возрастания и имело сложность ***O(n)***, где ***n*** — количество элементов в дереве.

Реализуйте индексатор `T this[int i]` у дерева, возвращающий i-ый по порядку (в порядке возрастания) элемент, содержащийся в дереве.

Сложность этой операции должна быть ***O(h)***, где ***h*** — высота дерева.

Для решения этой задачи для каждого узла дерева вам придется хранить и поддерживать еще и размер его поддерева.

Для простоты, обе операции реализуйте с использованием рекурсии.

Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections;
using System.Collections.Generic;

namespace BinaryTrees;

public class BinaryTree<T> : IEnumerable<T> where T : IComparable
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
    
    public T this[int i]
    {
        get
        {
            var tree = root;
            while (true)
            {
                if (tree == null) continue;
                var leftSize = tree.Left?.Size ?? 0;
                if (i == leftSize) return tree.Value;
                if (i < leftSize) tree = tree.Left;
                else if (i > leftSize)
                {
                    tree = tree.Right;
                    i -= leftSize + 1;
                }
            }
        }
    }
    
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
    
    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
    
    public IEnumerator<T> GetEnumerator()
    {
        return GetValues(root).GetEnumerator();
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

**Проект со всеми внесенными решениями.**
[BinaryTrees Edit](BinaryTrees_Edit.zip)

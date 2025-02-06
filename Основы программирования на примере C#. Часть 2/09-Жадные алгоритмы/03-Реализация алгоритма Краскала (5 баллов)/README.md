# Реализация алгоритма Краскала

Реализуйте описанный в лекции алгоритм Краскала в методе `FindMinimumSpanningTree`. Он принимает на вход список ребер с весами, а возвращает список ребер, из которых состоит минимальный остов.

Используйте уже готовый метод проверки наличия цикла.

```cs
public static bool HasCycle(List<Edge> edges);
```

`Edge` определен следующим образом:

```cs
public struct Edge
{
    public int From;
    public int To;
    public int Weight;
}
```

Все тесты пройдены, задача сдана:
```cs
public static IEnumerable<Edge> FindMinimumSpanningTree(IEnumerable<Edge> edges)
{
    var tree = new List<Edge>();
    foreach (var edge in edges.OrderBy(x => x.Weight))
    {
        tree.Add(edge);
        if (HasCycle(tree)) tree.Remove(edge);
    }
    return tree;
}
```

Вывод программы:
```cs
OK
```

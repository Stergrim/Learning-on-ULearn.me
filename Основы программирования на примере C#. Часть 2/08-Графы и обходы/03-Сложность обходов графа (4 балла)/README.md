# Сложность обходов графа

```cs
public static IEnumerable<Node> DepthSearch(Node startNode)
{
    var visited = new HashSet<Node>();
    var stack = new Stack<Node>();
    visited.Add(startNode);
    stack.Push(startNode);
    while (stack.Count != 0)
    {
        var node = stack.Pop();
        yield return node;
        foreach (var nextNode in node.IncidentNodes.Where(n => !visited.Contains(n)))
        {
            visited.Add(nextNode);
            stack.Push(nextNode);
        }
    }
}
```

1. Если V — количество вершин графа, а E — количество ребер графа, то оцените временную сложность обхода в глубину? (1 из 1 балла)
   * 🔴 ***O(V × E)***
   * 🟢 ***O(V + E)*** (Правильно! Проверка !visited.Contains выполнится E раз. Весь остальной код — не более V раз)
   * 🔴 ***O(V)***
   * 🔴 ***O(V²)***
   * 🔴 ***O(V² + E)***


2. Оцените пространственную сложность обхода в глубину? (1 из 1 балла)
   * 🔴 ***O(V × E)***
   * 🔴 ***O(V + E)***
   * 🟢 ***O(V)*** (Правильно! В худшем случае в visited и в стеке — все V вершин)
   * 🔴 ***O(V²)***
   * 🔴 ***O(V² + E)***


```cs
public static IEnumerable<Node> BreadthSearch(Node startNode)
{
    var visited = new HashSet<Node>();
    var queue = new Queue<Node>();
    visited.Add(startNode);
    queue.Enqueue(startNode);
    while (queue.Count != 0)
    {
        var node = queue.Dequeue();
        yield return node;
        foreach (var nextNode in node.IncidentNodes.Where(n => !visited.Contains(n)))
        {
            visited.Add(nextNode);
            queue.Enqueue(nextNode);
        }
    }
}
```

3. Если V — количество вершин графа, а E — количество ребер графа, то оцените временную сложность обхода в ширину? (1 из 1 балла)
   * 🔴 ***O(V × E)***
   * 🟢 ***O(V + E)*** (Правильно! Проверка !visited.Contains выполнится E раз. Весь остальной код — не более V раз)
   * 🔴 ***O(V)***
   * 🔴 ***O(V²)***
   * 🔴 ***O(V² + E)***


4. Оцените пространственную сложность обхода в ширину? (1 из 1 балла)
   * 🔴 ***O(V × E)***
   * 🔴 ***O(V + E)***
   * 🟢 ***O(V)*** (Правильно! В худшем случае в visited и в очереди — все V вершин)
   * 🔴 ***O(V²)***
   * 🔴 ***O(V² + E)***

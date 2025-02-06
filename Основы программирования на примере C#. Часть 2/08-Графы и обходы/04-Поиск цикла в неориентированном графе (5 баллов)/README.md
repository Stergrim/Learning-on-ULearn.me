# Поиск цикла в неориентированном графе

Проснувшись утром, вы обнаружили, что почти дописали метод проверки наличия цикла в неориентированном связном графе. Осталась самая сложная часть — что-то про серые и черные цвета.

Для улучшения читаемости кода вместо цвета вы решили использовать говорящие названия — visited для серого цвета и finished для черного.

Допишите метод HasCycle!

```cs
public class Node
{
    public int NodeNumber;
    public List<Node> IncidentNodes = new List<Node>();
}

public static void Main()
{
    // описание ребер разделены пробелами
    // дефисом разделены номера вершин ребра
    CheckHasCycle("0-1", false);
    CheckHasCycle("0-1 0-2", false);
    CheckHasCycle("0-1 0-2 1-2", true);
    CheckHasCycle("0-1 0-2 0-3", false);
    CheckHasCycle("0-1 0-2 0-3 1-3", true);
    RunSecretTests();
    Console.WriteLine("OK");
}
```

Все тесты пройдены, задача сдана:
```cs
public static bool HasCycle(List<Node> graph)
{
    var visited = new HashSet<Node>();
    var finished = new HashSet<Node>();
    var stack = new Stack<Node>();
    visited.Add(graph.First());
    stack.Push(graph.First());
    while (stack.Count != 0)
    {
        var node = stack.Pop();
        foreach (var nextNode in node.IncidentNodes)
        {
            if (finished.Contains(nextNode)) continue;
            if (visited.Contains(nextNode)) return true;
            visited.Add(nextNode);
            stack.Push(nextNode);
        }
        finished.Add(node);
    }
    return false;
}
```

Вывод программы:
```cs
OK
```

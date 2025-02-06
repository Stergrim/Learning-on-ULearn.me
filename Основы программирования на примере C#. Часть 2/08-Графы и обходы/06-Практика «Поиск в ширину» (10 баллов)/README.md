# Практика «Поиск в ширину»

Скачайте проект [Dungeon](Dungeon.zip).

На карте расположено несколько сундуков. Для тех сундуков, до которых существует путь от точки start, необходимо найти путь от сундука до точки start в виде односвязного списка `SinglyLinkedList`.

Для этого в классе `BfsTask` нужно реализовать поиск в ширину с указанной сигнатурой. Кстати, он вам понадобится и для следующей задачи!

Проверить корректность своего решения можно запустив тесты в классе `Bfs_Should`. Там же, по тестам, можно уточнить постановку задачи на различных крайних случаях.

После корректного выполнения задания, можно будет запустить проект. Кликнув на пустую ячейку вы увидите найденный вашим алгоритмом путь.

Все тесты пройдены, задача сдана:
```cs
using System.Collections.Generic;

namespace Dungeon;

public class BfsTask
{
    public static IEnumerable<SinglyLinkedList<Point>> FindPaths(Map map, Point start, Chest[] chests)
    {
        var queue = new Queue<SinglyLinkedList<Point>>();
        queue.Enqueue(new SinglyLinkedList<Point>(start));
        var visitedPoint = new HashSet<Point>();
        var hashChests = new HashSet<Point>();
        foreach (var chest in chests) { hashChests.Add(chest.Location); }
   
        while (queue.Count != 0)
        {
            var link = queue.Dequeue();
            if (link.Value.X < 0 || link.Value.Y < 0 || !map.InBounds(link.Value)) continue;
            if (map.Dungeon[link.Value.X, link.Value.Y] == MapCell.Wall) continue;
            if (visitedPoint.Contains(link.Value)) continue;
            visitedPoint.Add(link.Value);
   
            if (hashChests.Contains(link.Value)) { hashChests.Remove(link.Value); yield return link; }
            if (hashChests.Count == 0) break;
   
            for (var dy = -1; dy <= 1; dy++)
                for (var dx = -1; dx <= 1; dx++)
                    if (dx != 0 && dy != 0) continue;
                    else queue.Enqueue(new SinglyLinkedList<Point>(link.Value + new Point(dx,dy), link));
        }
    }
}
```

# Практика «Поделить территорию!»

Скачайте проект [Rivals](Rivals.zip).

Оказалось, что в лабиринте есть и другие охотники за сокровищами. Естественно, кто первый доберется до сундука, тот его и заберет себе.

Неплохо бы знать, кто из соперников до каких клеток лабиринта успеет добраться быстрее других.

В классе `RivalsTask` реализуйте функцию, разделяющую карту между игроками.

Нужно определить, до каких из клеток карты каждый игрок сможет дойти быстрее, вне зависимости от тактики остальных. При этом клетки с сундуками особенные, при попадании в них охотники начинают их охранять и прекращают перемещаться в данном направлении. Ходят игроки по очереди, начиная с первого.

Сделайте так, чтобы все тесты в классе `Rivals_Should` проходили.

После выполнения этого задания, при запуске проекта можно увидеть визуализацию процесса раздела карты.

Все тесты пройдены, задача сдана:
```cs
using System.Collections.Generic;

namespace Rivals;

public class RivalsTask
{
    public static IEnumerable<OwnedLocation> AssignOwners(Map map)
    {
        var queueOwned = new Queue<OwnedLocation>();
        var visitedPoint = new HashSet<Point>();
        var hashChests = new HashSet<Point>();
        foreach (var chest in map.Chests) { hashChests.Add(chest); }
        for (int i = 0; i < map.Players.Length; i++) { queueOwned.Enqueue(new OwnedLocation(i, map.Players[i], 0)); }
        
        while (queueOwned.Count != 0)
        {
            var playersQueue = queueOwned.Dequeue();
            if (playersQueue.Location.X < 0 || playersQueue.Location.Y < 0 || !map.InBounds(playersQueue.Location)) continue;
            if (map.Maze[playersQueue.Location.X, playersQueue.Location.Y] == MapCell.Wall) continue;
            if (visitedPoint.Contains(playersQueue.Location)) continue;
            visitedPoint.Add(playersQueue.Location);
            yield return new OwnedLocation(playersQueue.Owner, playersQueue.Location, playersQueue.Distance);
            if (hashChests.Contains(playersQueue.Location)) continue;
    
            for (var dy = -1; dy <= 1; dy++)
                for (var dx = -1; dx <= 1; dx++)
                    if (dx != 0 && dy != 0) continue;
                    else queueOwned.Enqueue(new OwnedLocation(playersQueue.Owner,
                        playersQueue.Location + new Point(dx, dy), playersQueue.Distance + 1));
        }
        yield break;
    }
}
```

**Проект со всеми внесенными решениями.**
[Rivals Edit](Rivals_Edit.zip)

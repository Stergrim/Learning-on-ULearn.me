# Практика «Жадина в лабиринте»

Продолжайте [в том же проекте](Greedy.zip).

Чтобы жизнь удалась, Жадине нужно собрать N сундуков. При этом, Жадина отказывается идти к сундуку, если есть другой сундук, путь до которого потребует меньше сил: он не только жадина, но и лентяй!

Помогите Жадине собрать N сундуков!

После выполнения этой задачи, Жадина начнёт передвигаться по лабиринту после запуска приложения.

Для того, чтобы сдать задачу, в файле `GreedyPathFinder`.cs реализуйте метод `FindPathToCompleteGoal`. Он должен возвращать путь передвижения Жадины. Путь не должен содержать исходную позицию — ту из которой Жадина начинает движение. Если подходящего пути не существует, метод должен возвращать пустой список.

Текущее состояние уровня передается в метод в объекте типа `State`.

Используйте класс `DijkstraPathFinder`, реализованный в предыдущей задаче. Его не нужно включать в отправляемый на проверку файл, считайте, что этот класс уже есть в проекте. При проверке этой задачи будет использоваться авторская реализация `DijkstraPathFinder`, а не ваша.

Гарантируется, что если рассмотреть множество всех сундуков и добавить в него исходную позицию, то в нём не существует тройки A, B, C, такой, что от А добраться до B так же трудно, как и от A до C. Другими словами, у Жадины всегда есть только один вариант дальнейших действий.

Тесты в классах `GreedyPathFinder_Should` и `GreedyTimeLimit_Tests` должны завершаться успехом.

Все тесты пройдены, задача сдана:
```cs
using System.Collections.Generic;
using Greedy.Architecture;
using System.Linq;

namespace Greedy;

public class GreedyPathFinder : IPathFinder
{
    public List<Point> FindPathToCompleteGoal(State state)
    {
        var pathFinder = new DijkstraPathFinder();
        var paths = pathFinder.GetPathsByDijkstra(state, state.Position, state.Chests);
        if (state.Goal > state.Chests.Count) return new List<Point>();
    
        int cost = 0;
        var totalPath = new List<Point>();
        while (paths.Any())
        {
            var path = paths.First();
            state.Scores++;
            if (state.Scores > state.Goal) break;
            totalPath.AddRange(path.Path.Skip(1).ToList());
            cost += path.Cost;
            state.Position = path.End;
            state.Chests.Remove(path.End);
            paths = pathFinder.GetPathsByDijkstra(state, state.Position, state.Chests);
        }
    
        if (totalPath.Count == 0) return new List<Point>();
        if (state.Energy < cost) return new List<Point>();
        return totalPath;
    }
}
```

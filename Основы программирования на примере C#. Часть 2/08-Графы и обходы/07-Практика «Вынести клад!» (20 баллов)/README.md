# Практика «Вынести клад!»

Продолжайте в том же проекте [Dungeon](Dungeon.zip).

Подготовка закончилась и вы в настоящем лабиринте с сокровищами! Сил хватит только на один сундук и то еле-еле. Найдите кратчайший путь из начальной точки до выхода, проходящий через хотя бы один сундук.

В случае если кратчайших путей несколько необходимо выбрать путь проходящий через самый "тяжелый" сундук.

Решайте задачу в классе `DungeonTask`.

Детали реализации для граничных случаев можно найти в классе с тестами `Dungeon_Should`. Сделайте так, чтобы все тесты проходили.

После выполнения этого задания, при запуске проекта можно увидеть визуализацию пути. Наслаждайтесь найденными сокровищами!

Эту задачу можно элегантно решить без циклов, используя LINQ.

Все тесты пройдены, задача сдана:
```cs
using System;
using System.Linq;
using System.Collections.Generic;

namespace Dungeon;

public class DungeonTask
{
    public static MoveDirection[] FindShortestPath(Map map)
    {
        var dictionaryChest = new Dictionary<Point, int>();
        foreach (var chest in map.Chests) { dictionaryChest[chest.Location] = chest.Value; }
    
        var shortLink = BfsTask.FindPaths(map, map.InitialPosition, new Chest[1] { new Chest(map.Exit, 1) });
        
        if (!shortLink.Any()) return new MoveDirection[0];
        
        var listPoint = shortLink.SelectMany(x => x).ToList();
        
        if (dictionaryChest.Count() == 1)
            foreach (var point in listPoint)
                if (dictionaryChest.ContainsKey(point)) return ConvertPathToMove(listPoint);
    
        var exitLink = BfsTask.FindPaths(map, map.Exit, map.Chests);
        if (!exitLink.Any()) return ConvertPathToMove(listPoint);
        
        var playerLink = BfsTask.FindPaths(map, map.InitialPosition, map.Chests);
    
        var shortedLink = FindShortedLink(playerLink,exitLink);
    
        if (shortedLink.Count() == 1) return ConvertPathToMove(ConvertTupleToPath(shortedLink.First()));
        
        return ConvertPathToMove(ConvertTupleToPath(FindBestShortedLink(shortedLink, dictionaryChest).Last()));
    }
    
    public static IEnumerable<Tuple<SinglyLinkedList<Point>, SinglyLinkedList<Point>>>
        FindShortedLink(IEnumerable<SinglyLinkedList<Point>> playerLink,
                        IEnumerable<SinglyLinkedList<Point>> exitLink)
    {
        var totalLink = playerLink
                        .Join(exitLink, x => x.Value, y => y.Value, (x, y) => Tuple.Create(x, y));
    
        int minLength = totalLink
            .Select(x => x.Item1.Length + x.Item2.Length - 1)
            .Min();
    
        return totalLink.Where(x => x.Item1.Length + x.Item2.Length - 1 == minLength);
    }
    
    public static IEnumerable<Tuple<SinglyLinkedList<Point>,SinglyLinkedList<Point>>>
        FindBestShortedLink(IEnumerable<Tuple<SinglyLinkedList<Point>,
                                              SinglyLinkedList<Point>>> shortedLink,
                            Dictionary<Point, int> dictionaryChest)
    {
        int valueChest = -1;
        var shortedOne = shortedLink.Take(1);
        foreach (var link in shortedLink)
            if (dictionaryChest[link.Item1.Value] > valueChest)
            {
                valueChest = dictionaryChest[link.Item1.Value];
                shortedOne = shortedOne.Append(link);
            }
        return shortedOne;
    }
    
    public static List<Point> ConvertTupleToPath(Tuple<SinglyLinkedList<Point>, SinglyLinkedList<Point>> tupleLink)
    {
        return tupleLink.Item1.Skip(1).Reverse().Concat(tupleLink.Item2).Reverse().ToList();
    }
    
    public static MoveDirection[] ConvertPathToMove(List<Point> listPoint)
    {
        listPoint.Reverse();
    
        return listPoint
            .Skip(1)
            .Zip(listPoint, (curr,prev) => Walker.ConvertOffsetToDirection(curr-prev))
            .ToArray();
    }
}
```

**Проект со всеми внесенными решениями.**
[Dungeon Edit](Dungeon_Edit.zip)

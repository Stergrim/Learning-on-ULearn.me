# Практика «Оптимальный маршрут»

Продолжайте [в том же проекте](Greedy.zip).

Однажды после очередного путешествия по лабиринту Жадина вдруг осознал, что его жадная стратегия не всегда позволяет собрать наибольшее возможное количество сундуков.

От этого счастье куда-то улетучилось!

Помогите Жадине собрать максимум сундуков при заданном ограничении сил. Без перебора порядка посещения сундуков тут не обойтись. На больших лабиринтах перебор, конечно, не справится, но на маленьких может сработать лучше, чем жадная стратегия.

Реализуйте метод `FindPathToCompleteGoal` в классе `NotGreedyPathFinder` так, чтобы тесты в классе `NotGreedyPathFinder_Should` завершались успешно.

Используйте класс `DijkstraPathFinder`, реализованный в предыдущих задачах.

Все тесты пройдены, задача сдана:
```cs
using System.Linq;
using Greedy.Architecture;
using System.Collections.Generic;

namespace Greedy;

public class NotGreedyPathFinder : IPathFinder
{
    private List<PathWithCost>? bestPath;
    
    public List<Point> FindPathToCompleteGoal(State state)
    {
        var finder = new DijkstraPathFinder();
        var pathsThroughAllChests = new Stack<PathWithCost>();
        var notUsedChests = new HashSet<Point>(state.Chests);
        var dictionaryOfPaths = new Dictionary<(Point start, Point end), PathWithCost>();
    
        foreach (var chest in state.Chests)
        {
            var result = FindPathByFirstChest(notUsedChests, state.Position, chest, 0,
                state, pathsThroughAllChests, dictionaryOfPaths, finder);
            if (result != null) return result;
        }
    
        return ConvertToPath(bestPath);
    }
    
    private List<Point>? FindPathByFirstChest(HashSet<Point> notUsedChests,
        Point previousNode,Point currentChest,
        int cost, State state, Stack<PathWithCost> pathsThroughAllChests,
        Dictionary<(Point start, Point end), PathWithCost> dictionaryOfPaths,
        DijkstraPathFinder finder)
    {
        var pathToCurrentChest = GetPathToNode(previousNode, currentChest, dictionaryOfPaths, state, finder);
        
        if (pathToCurrentChest == null) return null;
        cost += pathToCurrentChest.Cost;
        notUsedChests.Remove(currentChest);
        pathsThroughAllChests.Push(pathToCurrentChest);
        
        if (cost <= state.Energy && notUsedChests.Count > 0)
            foreach (var newCurrentChest in notUsedChests.ToList())
            {
                var result = FindPathByFirstChest(notUsedChests, currentChest, newCurrentChest, cost,
                    state, pathsThroughAllChests, dictionaryOfPaths, finder);
               
                if (result != null) return result;
            }
        else
            return CheckOnBestPath(state, cost, notUsedChests, pathsThroughAllChests, currentChest);
        
        notUsedChests.Add(currentChest);
        pathsThroughAllChests.Pop();
        return null;
    }
    
    private List<Point>? CheckOnBestPath(State state, int cost, HashSet<Point> notUsedChests,
        Stack<PathWithCost> pathsThroughAllChests, Point currentChest)
    {
        if (cost <= state.Energy && notUsedChests.Count == 0)
            return ConvertToPath(pathsThroughAllChests.ToList());
    
        notUsedChests.Add(currentChest);
        pathsThroughAllChests.Pop();
        if (bestPath == null || bestPath.Count < pathsThroughAllChests.Count)
            bestPath = pathsThroughAllChests.ToList();
        return null;
    }
    
    private List<Point> ConvertToPath(List<PathWithCost> pathsThroughAllChests)
    {
        var result = new List<Point>();
        pathsThroughAllChests.Reverse();
    
        foreach (var path in pathsThroughAllChests)
            result.AddRange(path.Path.Skip(1).ToList());
    
        return result;
    }
    
    private PathWithCost GetPathToNode(Point previousNode, Point currentChest,
        Dictionary<(Point start, Point end), PathWithCost> dictionaryOfPaths,
        State state, DijkstraPathFinder finder)
    {
        if (dictionaryOfPaths.ContainsKey((previousNode, currentChest)))
            return dictionaryOfPaths[(previousNode, currentChest)];
    
        var path = finder.GetPathsByDijkstra(state, previousNode, new[] { currentChest }).First();
        dictionaryOfPaths[(previousNode, currentChest)] = path;
        return path;
    }
}
```

**Проект со всеми внесенными решениями.**
[Greedy Edit](Greedy_Edit.zip)

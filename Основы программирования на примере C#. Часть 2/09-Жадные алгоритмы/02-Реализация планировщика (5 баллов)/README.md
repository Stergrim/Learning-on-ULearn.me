# Реализация планировщика

Закончите алгоритм составления оптимального расписания `PlanSchedule`.

Метод `PlanSchedule` принимает на вход последовательность встреч и должен вернуть оптимальное расписание — список совместимых друг с другом встреч, содержащий максимальное количество элементов.

Встреча представляется кортежем `ValueTuple<int Start, int End>`, в котором `Start` — это время начала встречи, а `End` — время конца встречи.

Все тесты пройдены, задача сдана:
```cs
public static IEnumerable<(int Start, int End)> PlanSchedule(IEnumerable<(int Start, int End)> meetings)
{
    var leftEdge = int.MinValue;
    foreach (var meeting in meetings.OrderBy(m => m.End))
    {
        if (meeting.Item1 >= leftEdge)
        {
            leftEdge = meeting.Item2;
            yield return meeting;
        }
    }
}
```

Вывод программы:
```cs
OK
```

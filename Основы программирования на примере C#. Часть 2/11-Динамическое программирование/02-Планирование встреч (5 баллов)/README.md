# Планирование встреч

Пусть встреча описывается таким классом:

```cs
public class Event
{
    public int Price;
    public int StartTime, FinishTime;
}
```

Завершите реализацию метода составления оптимального расписания.

```cs
public static void Main()
{
    Assert.AreEqual(0, GetOptimalScheduleGain(new Event[0]));
    Assert.AreEqual(50, GetOptimalScheduleGain(
        new Event { StartTime = 1, FinishTime = 11, Price = 50 }));
    Assert.AreEqual(300, GetOptimalScheduleGain(
        new Event { StartTime = 9, FinishTime = 11, Price = 50 },
        new Event { StartTime = 10, FinishTime = 13, Price = 190 },
        new Event { StartTime = 14, FinishTime = 17, Price = 90 },
        new Event { StartTime = 12, FinishTime = 15, Price = 200 },
        new Event { StartTime = 16, FinishTime = 18, Price = 50 }));
    SecretTest();
    Console.WriteLine("OK");
}
```

Все тесты пройдены, задача сдана:
```cs
public static int GetOptimalScheduleGain(params Event[] events)
{
    var fakeBorderEvent = new Event { StartTime = int.MinValue, FinishTime = int.MinValue, Price = 0 };
    events = events.Concat(new[] { fakeBorderEvent }).OrderBy(e => e.FinishTime).ToArray();
    
    var opt = new int[events.Length];
    opt[0] = 0;
    for (var k = 1; k < events.Length; k++)
    {
        var i = k - 1;
        while (i > 0)
        {
            if (events[i].FinishTime < events[k].StartTime) break;
            i--;
        }
        opt[k] = Math.Max(opt[k - 1], opt[i] + events[k].Price);
    }
    return opt.Last();
}
```

Вывод программы:
```cs
OK
```

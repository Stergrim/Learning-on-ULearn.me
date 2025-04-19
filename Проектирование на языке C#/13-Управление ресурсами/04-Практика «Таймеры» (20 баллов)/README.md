# Практика «Таймеры»

Скачайте проект с тестами на это задание [проекте Memory.Timers](Memory.Timers.zip).

В этом задании вы напишите библиотеку для профилирования кода, то есть для измерения скорости работы отдельных методов. В реальности для поиска медленных частей программы чаще используют специальную программу — профилировщик. Например, встроенный профилировщик в Visual Studio или внешний профилировщик, такой как JetBrains dotTrace.

Но стандартные профилировщики подходят не всегда. Давайте сделаем свой с удобным программным интерфейсом на основе использования оператора using и интерфейса IDisposable.

Использование должно быть примерно такое:

```cs
var writer = new StringWriter();
using (var timer = Timer.Start(writer, "T1"))
{
    // do things 1
    using (timer.StartChildTimer("T2"))
    {
        // do things 2
        ...
    }
    using (timer.StartChildTimer("T3"))
    {
        // do things 3
        ...
    }
}
```

Таймер T1 должен оценивать время выполнения do things 1, 2, 3; T2 — do things 2; T3 — do things 3. Естественно, нужна поддержка произвольной вложенности.

Результаты должны записываться в StringWriter, переданный в корневой таймер, в таком формате:

```cs
T1                  : 600
    T2              : 250
    T3              : 300
    Rest            : 50
```

Более точно формат зафиксирован в модульных тестах. Кроме того, в стартовом коде дан метод для форматирования одной строчки отчета. Используйте его, чтобы проще было пройти проверки в тестах.

Идея задачи в том, чтобы на старте using начинать засекать время, а при вызове Dispose заканчивать засекать.

Все тесты пройдены, задача сдана:
```cs
using System.Diagnostics;

namespace Memory.Timers;

public class Timer : IDisposable
{
    private readonly int level;
    private readonly string name;
    private readonly StringWriter writer;
    private readonly Stopwatch stopWatch = new Stopwatch();
    private readonly List<Timer> childs = new List<Timer>();
    
    public Timer(StringWriter writer, string name, int level)
    {
        this.name = name;
        this.level = level;
        this.writer = writer;
        stopWatch.Start();
    }
    
    public static Timer Start(StringWriter writer, string name = "*")
        => new Timer(writer, name, 0);
    
    public Timer StartChildTimer(string timerName)
    {
        var timer = new Timer(writer, timerName, level + 1);
        childs.Add(timer);
        return timer;
    }
    
    public void Dispose()
    {
        stopWatch.Stop();
        if (level == 0)
        {
            WriteReport();
            writer.Dispose();
        }
    }
    
    private void WriteReport()
    {
        writer.Write(FormatReportLine(name, level, stopWatch.ElapsedTicks));
        if (childs.Count != 0)
        {
            childs.ForEach(child => child.WriteReport());
            var totalChildsTicks = childs.Sum(child => child.stopWatch.ElapsedTicks);
            writer.Write(FormatReportLine("Rest", level + 1, stopWatch.ElapsedTicks - totalChildsTicks));
        }
    }
    
    private static string FormatReportLine(string timerName, int level, long value)
    {
        var intro = new string(' ', level * 4) + timerName;
        return $"{intro,-20}: {value}\n";
    }
}
```

# Практика «Benchmark»

В следующей практической задаче мы поставим два эксперимента с измерением производительности некоторых операций со структурами и обычными классами. Эти эксперименты дадут общее понимание, в каких случаях структуры могут быть полезны, а в каких наоборот вредны.

Но сначала нужно создать вспомогательный код для бенчмаркинга — так называется измерение производительности отдельных участков кода.

[Скачайте проект](StructBenchmarking.zip)

**Часть 1. Benchmark**

В файле `BenchmarkTask.cs` реализуйте в классе Benchmark интерфейс IBenchmark. Подробности ниже.

Интерфейс IBenchmark содержит единственный метод, принимающий task — действие, скорость работы которого нужно измерить. И возвращает длительность в миллисекундах.

```cs
double MeasureDurationInMs(ITask task, ...)
```

В этот метод можно посылать любую реализацию простейшего интерфейса `ITask`:

```cs
public interface ITask
{
    void Run();
}
```

Измерить длительность выполнения Run в методе MeasureDurationInMs можно с помощью класса `Stopwatch`. Однако важно учитывать ряд тонкостей бенчмаркинга:
- Современные компьютеры умеют измерять время лишь с некоторой точностью. Поэтому измерять время выполнения одной очень короткой операции бессмысленно. Нужно повторить её множество раз, засечь суммарное время и поделить результат на количество повторений. Для этого метод MeasureDurationInMs принимает вторым аргументом `repetitionCount` — количество повторений.
- Среда исполнения .NET компилирует отдельные методы в машинный код только тогда, когда он понадобился первый раз. Этот подход называется JIT — just in time compilation. Как следствие, первый вызов метода может быть значительно медленнее последующих. Поэтому перед измерением времени нужно сделать один «прогревочный» вызов.
- Проект собирается в режиме отладки (Debug) или в режиме релиза (Release). В режиме отладки компилятор не применяет оптимизации кода, что негативно сказывается на производительности. Поэтому перед запуском тестов на производительность [переключите сборку проекта в релиз](https://learn.microsoft.com/ru-ru/visualstudio/debugger/how-to-set-debug-and-release-configurations?view=vs-2019).
- В произвольный момент времени выполнение программы может быть приостановлено сборщиком мусора. Это тоже негативно влияет на точность измерений. Минимизировать вероятность этого можно, вызвав сборщик мусора принудительно перед тем, как начинать засекать время. Это можно сделать так:


```cs
GC.Collect();
GC.WaitForPendingFinalizers();
```

Кроме того, имейте в виду, что современные компьютеры имеют режим энергосбережения. В таком режиме частота его процессора может быть искусственно понижена и результаты всех замеров производительности могут стать непредсказуемыми. Ноутбуки могут иногда автоматически включать этот режим, например, когда они отключены от сети или когда остаётся мало заряда батареи.

Теперь у вас достаточно информации, чтобы реализовать Benchmark в файле `BenchmarkTask.cs`.

**Часть 2. Тест StringConstructorFasterThanStringBuilder**

Пора как-нибудь применить ваш Benchmark. Например, сравнив производительность каких-нибудь двух способов сделать одно и то же.

Напишите в том же файле unit-тест в методе `StringConstructorFasterThanStringBuilder`. В нём нужно сравнить два способа создания строки, состоящей из 10000 букв 'а':
- Создать `StringBuilder`, много раз вызвать `Append`, а в конце вызвать у него `ToString()`.
- Вызвать специализированный конструктор строки `new string('a', 10000)`.

Постарайтесь выбрать количество повторений так, чтобы суммарно весь этот тест работал около секунды, чтобы нивелировать описанные выше эффекты.

Тест должен с помощью `Assert.Less` проверять, что специализированный конструктор строки работает быстрее, чем StringBuilder.

Для каждого метода создания строки создайте свою реализацию ITask в том же файле. Используйте их совместно с классом Benchmark в тесте, чтобы сравнить время работы.


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Diagnostics;
using NUnit.Framework;

namespace StructBenchmarking
{
    public class Benchmark : IBenchmark
    {
        public double MeasureDurationInMs(ITask task, int repetitionCount)
        {
            task.Run();
            double timeAll = 0.0;
            for (var i = 0; i < repetitionCount; i++)
            {
                GC.Collect();
                GC.WaitForPendingFinalizers();
                var time = Stopwatch.StartNew();
                task.Run();
                time.Stop();
                timeAll += time.ElapsedMilliseconds;
            }
            return (double)timeAll/repetitionCount;
        }
    }
    
    public class BuilderTest : ITask
    {
        public System.Text.StringBuilder Str = new System.Text.StringBuilder { };
        public void Run()
        {
            for (var i = 0; i < 10000; i++)
                Str.Append('a');
            Str.ToString();
        }
    }
    
    public class StringTest : ITask
    {
        public string Str;
        public void Run() { Str = new string('a', 10000); }
    }
    
    [TestFixture]
    public class RealBenchmarkUsageSample
    {
        [Test]
        public void StringConstructorFasterThanStringBuilder()
        {
            var builderTest = new BuilderTest();
            var stringTest = new StringTest();
            var benchmark = new Benchmark();
    
            var firstTest = benchmark.MeasureDurationInMs(builderTest, 100);
            var secondTest = benchmark.MeasureDurationInMs(stringTest, 100);
            Assert.Less(secondTest, firstTest);
        }
    }
}
```

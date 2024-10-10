# Практика «Эксперименты»

Пришло время делать эксперименты. Продолжайте в том же проекте, в файле ExperimentsTask.cs

**Эксперимент 1. Создание массива**

В файле `ArrayCreationTasks.cs` есть две реализации уже знакомого вам интерфейса ITask для работы с классом Benchmark. Оба класса создают массив в методе Run. Но один делает массив структур, а второй массив классов.

В классе `ExperimentsTask` реализуйте метод `BuildChartDataForArrayCreation`. Этот метод должен измерять длительность работы метода Run у классов `StructArrayCreationTask` и `ClassArrayCreationTask` с помощью Benchmark из прошлого задания.

Нужно измерить время для структур и классов всех размеров, указанных в `Constants.FieldCounts`. Результаты измерения вернуть в виде объекта ChartData. Дальше в Program.cs эти результаты будут показаны на графиках.

Запустите код на исполнение. Вы должны увидеть первый график скорости работы от количества полей в классе/структуре. На нём должно быть видно, что массивы классов создаются дольше, чем массивы структур.

**Эксперимент 2. Передача в метод**

Аналогично в файле `MethodCallTasks.cs` есть ещё пара реализаций ITask. Они вызывают метод, передавая в качестве аргумента класс или структуру с большим количеством полей.

В том же классе `ExperimentsTask` реализуйте метод `BuildChartDataForMethodCall`.

Избавьтесь от дублирования кода в методах `BuildChartDataForMethodCall` и `BuildChartDataForArrayCreation`. Возможно, для этого понадобится создать новые классы.

Запустите код на исполнение. Вы должны увидеть второй график, показывающий, что большие классы передаются в метод быстрее, чем большие структуры.

Попробуйте объяснить наблюдаемый результат.

**Подсказка**

Избавиться от дублирования в коде `BuildChartDataForMethodCall` и `BuildChartDataForArrayCreation` поможет [паттерн абстрактная фабрика](https://habr.com/ru/articles/465835/).


Все тесты пройдены, задача сдана:
```cs
using System.Collections.Generic;

namespace StructBenchmarking
{
    public class Experiments
    {
        public static ChartData BuildChartDataForArrayCreation(IBenchmark benchmark,
                                                               int repetitionsCount)
        {
            var classesTimes = new List<ExperimentResult>();
            var structuresTimes = new List<ExperimentResult>();
            foreach(var size in Constants.FieldCounts)
            {
                var structArrayCreationTask = new StructArrayCreationTask(size);
                var classArrayCreationTask = new ClassArrayCreationTask(size);
                var structTest = benchmark.MeasureDurationInMs(structArrayCreationTask,
                                                               repetitionsCount);
                var classTest = benchmark.MeasureDurationInMs(classArrayCreationTask,
                                                              repetitionsCount);
                structuresTimes.Add(new ExperimentResult(size, structTest));
                classesTimes.Add(new ExperimentResult(size, classTest));
            }
            return new ChartData
            {
                Title = "Create array",
                ClassPoints = classesTimes,
                StructPoints = structuresTimes,
            };
        }
    
        public static ChartData BuildChartDataForMethodCall(IBenchmark benchmark,
                                                            int repetitionsCount)
        {
            var classesTimes = new List<ExperimentResult>();
            var structuresTimes = new List<ExperimentResult>();
            foreach (var size in Constants.FieldCounts)
            {
                var structArrayCreationTask = new MethodCallWithStructArgumentTask(size);
                var classArrayCreationTask = new MethodCallWithClassArgumentTask(size);
                var structTest = benchmark.MeasureDurationInMs(structArrayCreationTask,
                                                               repetitionsCount);
                var classTest = benchmark.MeasureDurationInMs(classArrayCreationTask,
                                                              repetitionsCount);
                structuresTimes.Add(new ExperimentResult(size, structTest));
                classesTimes.Add(new ExperimentResult(size, classTest));
            }
            return new ChartData
            {
                Title = "Call method with argument",
                ClassPoints = classesTimes,
                StructPoints = structuresTimes,
            };
        }
    }
}
```

**Проект со всеми внесенными решениями.**
[StructBenchmarking Edit](StructBenchmarking_Edit.zip)

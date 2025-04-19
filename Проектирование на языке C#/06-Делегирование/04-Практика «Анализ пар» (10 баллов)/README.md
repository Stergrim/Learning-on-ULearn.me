# Практика «Анализ пар»

Делегаты позволяют нам в ряде случаев обходится вообще без создания классов. Скачайте проект [Delegates.PairsAnalysis](Delegates.PairsAnalysis.zip).

Метод `Analysis.FindMaxPeriodIndex(params DateTime[] data)` должен последовательно разбить список дат по парам и вернуть индекс пары с наибольшим периодом между датами.

Видно, как много классов приходится писать и какую нетривиальную систему наследования использовать.

Избавьтесь от лишних классов и переведите вычисления в LINQ-стиль, разработав два метода расширения
- Pairs, который превращает последовательность T в последовательность Tuple<T,T>
- MaxIndex, который ищет индекс максимального элемента.

В классе `Analysis` перепишите два метода так, чтобы они использовали Pairs и MaxIndex, а три вспомогательных класса из проекта перестали использоваться.

Все тесты пройдены, задача сдана:
```cs
namespace Delegates.PairsAnalysis;

public static class Analysis
{
    public static int FindMaxPeriodIndex(params DateTime[] data)
    {
        return data.Pairs().Select(x => x.Item2 - x.Item1).MaxIndex();
    }
    
    public static double FindAverageRelativeDifference(params double[] data)
    {
        return data.Pairs().Average(x => (x.Item2 - x.Item1) / x.Item1);
    }
}

public static class IEnumerableExtension
{
    public static IEnumerable<Tuple<T, T>> Pairs<T>(this IEnumerable<T> collection)
    {
        var prev = default(T);
        var count = 0;
    
        foreach (var curr in collection)
        {
            if (count != 0) yield return new Tuple<T, T>(prev, curr);
            prev = curr;
            count++;
        }
    }
    
    public static int MaxIndex<T>(this IEnumerable<T> collection)
        where T : IComparable
    {
        return collection
            .Select((element, index) => (element, index))
            .Max()
            .index;
    }
}
```

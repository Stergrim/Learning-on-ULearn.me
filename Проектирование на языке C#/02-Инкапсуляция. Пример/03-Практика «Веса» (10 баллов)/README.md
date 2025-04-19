# Практика «Веса»

Нейронная сеть состоит из нейронов, каждый из которых имеет вектор весов. Иногда нужно иметь доступ к весам отдельных нейронов, например, при их инициализации. Однако, при разработке алгоритмов обучения оказывается удобным работать со всеми весами в сети как с единым вектором. Таким образом, нужно организовать различные виды доступа к одним и тем же данным.

Скачайте [проект Incapsulation.Weights](Incapsulation.Weights.zip) и напишите класс Indexer, который создается как обертка над массивом double[], и открывает доступ к его подмассиву некоторой длины, начиная с некоторого элемента. Ваше решение должно проходить тесты, содержащиеся в проекте. Как и всегда, вы должны следить за целостностью данных в Indexer.

Все тесты пройдены, задача сдана:
```cs
namespace Incapsulation.Weights;

public class Indexer
{
    public readonly double[] Weights;
    
    private int start;
    public int Start
    {
        get => start;
        private set
        {
            if (value < 0 || value > Weights.Length)
                throw new ArgumentException();
            start = value;
        }
    }
    
    private int length;
    public int Length
    {
        get => length;
        private set
        {
            if (value < 0 || value + this.Start > Weights.Length)
                throw new ArgumentException();
            length = value;
        }
    }
    
    public Indexer(double[] weights, int start, int length)
    {
        Weights = weights;
        Start = start;
        Length = length;
    }
    
    public double this[int index]
    {
        get => Weights[CheckIndex(start + index)];
        set => Weights[CheckIndex(start + index)] = value;
    }
    
    private int CheckIndex(int index)
    {
        if (index < start || index >= start + length)
            throw new IndexOutOfRangeException();
        return index;
    }
}
```

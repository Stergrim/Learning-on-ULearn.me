# Практика «Случайные объекты»

Для нагрузочного тестирования вашей программы вам нужно уметь создавать большое количество экземпляров классов, при этом они должны быть существенно различны. Вы решили использовать для этой цели генератор случайных чисел, и решили использовать атрибуты для того, чтобы указать, из какого распределения брать значения для тех или иных свойств в объектах.

Для простоты, генерировать нужно только значения типа double и с помощью двух распределений: нормального и экспоненциального.

[Проект Reflection.Randomness](Reflection.Randomness.zip)

Создавать экземпляры классов должен `Generator`, все требования к которому сформулированы в виде модульных тестов в проекте.

**Указания к решению**
1. С помощью рефлексии найдите все свойства генерируемого класса, помеченные нужным атрибутом.
2. По значениям, указанным в атрибутах с помощью рефлексии создайте экземпляры классов, генерирующих числа по нужному распределению.
3. В методе Generate создайте объект генерируемого класса и проинициализируйте все его свойства сгенерированными числами.
4. Ради производительности, постарайтесь все действия, которые можно не делать на генерацию каждого экземпляра, сделать в конструкторе генератора только один раз.

Все тесты пройдены, задача сдана:
```cs
using System.Reflection;

namespace Reflection.Randomness;

public class Generator<T> where T : new()
{
    private static readonly PropertyInfo[] properties;
    private static readonly List<IContinuousDistribution>? distributions;
    static Generator()
    {
        properties = typeof(T)
            .GetProperties()
            .Where(p => p.GetCustomAttributes<FromDistributionAttribute>().Any())
            .ToArray();
        
        var attributes = properties
            .Select(p => p.GetCustomAttributes<FromDistributionAttribute>().First())
            .ToArray();
    
        for (var i = 0; i < properties.Length; i++)
        {
            var attribute = attributes[i];
            var types = attribute.Parameters.Select(value => value.GetType()).ToArray();
            var constructor = attribute.DistributionType.GetConstructor(types);
            
            distributions ??= new List<IContinuousDistribution>();
            distributions.Add((IContinuousDistribution)constructor
                    .Invoke(attribute.Parameters.Select(v => (object)v).ToArray()));
        }
    }
    
    public T Generate(Random rnd)
    {
        var result = new T();
        for (var i = 0; i < properties.Length; i++)
            properties[i].SetValue(result, distributions?[i].Generate(rnd));
        return result;
    }
}

public class FromDistributionAttribute : Attribute
{
    public readonly Type DistributionType;
    public readonly int[] Parameters;
    public FromDistributionAttribute(Type type, params int[] parameters)
    {
        if (!type.IsAssignableTo(typeof(IContinuousDistribution)))
            throw new ArgumentException($"Type '{type.FullName}' is not derived from IContinuousDistribution.");
        if (type.GetConstructors().All(c => c.GetParameters().Length != parameters.Length))
            throw new ArgumentException($"Type '{type.FullName}' wrong count of arguments.");
        DistributionType = type;
        Parameters = parameters;
    }
}
```

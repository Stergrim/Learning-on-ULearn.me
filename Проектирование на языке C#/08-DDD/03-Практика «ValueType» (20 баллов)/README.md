# Практика «ValueType»

Скачайте проект [Ddd.Taxi](Ddd.Taxi.zip).

Все Value-типы, согласно DDD, должны поддерживать семантику значений, то есть сравниваться по содержимому своих свойств. Каждый раз реализовывать Equals, GetHashCode и ToString соответствующим образом — довольно муторное занятие. Часто для этого создают базовый класс, наследование от которого реализует нужным образом все эти стандартные методы. Это вам и предстоит сделать!

В рамках этого задания сравнивать Value-типы можно только по значению их публичных свойств, без учета значения полей. Хотя как правильно это стоит делать на практике — вопрос дискуссионный и, скорее, предмет договорённостей в вашей команде.

В файле Infrastructure/ValueType.cs реализуйте класс `ValueType` так, чтобы проходили все тесты в файле `ValueType_Tests.cs`.

После решения этой задачи посмотрите подсказки!

Все тесты пройдены, задача сдана:
```cs
using System.Text;
using Ddd.Taxi.Domain;
using System.Reflection;

namespace Ddd.Taxi.Infrastructure;

public class ValueType<T>
{
    private readonly List<PropertyInfo> properties;
    public ValueType()
    {
        properties = this.GetType()
                         .GetProperties(BindingFlags.Instance | BindingFlags.Public)
                         .OrderBy(p => p.Name)
                         .ToList();
    }
    
    public override bool Equals(object obj)
    {
        if (ReferenceEquals(null, obj)) return false;
        if (ReferenceEquals(this, obj)) return true;
        if (obj.GetType() != this.GetType()) return false;
        foreach (var property in properties)
        {
            var thisValue = property.GetValue(this, null);
            var objectValue = property.GetValue(obj, null);
            if (thisValue == null & objectValue == null) continue;
            if (thisValue == null || objectValue == null || !thisValue.Equals(objectValue)) return false;
        }
        return true;
    }
    
    public bool Equals(PersonName name) => Equals((object)name);
    
    public override int GetHashCode()
    {
        int offsetBasis = unchecked((int)2166136261);
        int prime = 16777619;
        int hash = offsetBasis;
        foreach (var property in properties)
            hash = unchecked((hash * prime) ^ property.GetValue(this, null).GetHashCode());
        return hash;
    }
    
    public override string ToString()
    {
        var result = new StringBuilder(this.GetType().Name + "(");
        int index = 0;
        foreach (var property in properties)
        {
            if (index != properties.Count - 1)
                result.AppendFormat("{0}: {1}; ", property.Name, property.GetValue(this, null));
            else
                result.AppendFormat("{0}: {1})", property.Name, property.GetValue(this, null));
            index++;
        }
        return result.ToString();
    }
}
```

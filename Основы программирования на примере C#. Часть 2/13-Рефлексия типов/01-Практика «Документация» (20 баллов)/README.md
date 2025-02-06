# Практика «Документация»

Если документация хранится отдельно от кода, она очень легко и быстро устаревает и становится неактуальной. Один из способов сохранять документацию в актуальном состоянии — это писать её максимально близко к коду. Иногда для этого используют атрибуты, информацию из которых извлекают и собирают с помощью рефлексии.

Вот эту технику вам и предстоит применить в этой задаче.

[Скачайте проект Documentation](Documentation.zip)

В файле `Specifier` реализуйте методы, возвращающие структурированное описание методов класса, ориентируясь на атрибуты, которыми этот класс размечен.

Начните с изучения класса `VkApi`, на котором будет тестироваться ваш `Specifier`. Это всего лишь пример класса, который мы хотим документировать, поэтому ни один метод там не реализован (и реализовывать их не нужно).

Изучите тест `Specifier_should`. В нём зафиксированы требования к поведению вашей реализации `ISpecifier`.

Все тесты пройдены, задача сдана:
```cs
using System;
using System.Linq;
using System.Reflection;

namespace Documentation;

public class Specifier<T> : ISpecifier
{
    private Type type = typeof(T);
    private ILookup<string, MethodInfo> methods;
    
    public Specifier()
    {
        type = typeof(T);
        methods = type.GetMethods().ToLookup(x => x.Name);
    }
    
    public string GetApiDescription()
    {
        return type.GetCustomAttributes<ApiDescriptionAttribute>().FirstOrDefault()?.Description;
    }
    
    public string[] GetApiMethodNames()
    {
        return methods.SelectMany(x => x)
            .Where(x => x.GetCustomAttributes<ApiMethodAttribute>().Any())
            .Select(x => x.Name)
            .ToArray();
    }
    
    public string GetApiMethodDescription(string methodName)
    {
        return methods[methodName].FirstOrDefault()?
            .GetCustomAttributes<ApiDescriptionAttribute>()
            .FirstOrDefault()?.Description;
    }
    
    public string[] GetApiMethodParamNames(string methodName)
    {
        return methods[methodName].FirstOrDefault()?
            .GetParameters()
            .Select(x => x.Name)
            .ToArray();
    }
    
    public string GetApiMethodParamDescription(string methodName, string paramName)
    {
        return methods[methodName].FirstOrDefault()?
            .GetParameters().FirstOrDefault(x => x.Name == paramName)?
            .GetCustomAttributes<ApiDescriptionAttribute>()
            .FirstOrDefault()?.Description;
    }
    
    public ApiParamDescription GetApiMethodParamFullDescription(string methodName, string paramName)
    {
        var attributes = methods[methodName].FirstOrDefault()?
            .GetParameters().FirstOrDefault(x => x.Name == paramName)?
            .GetCustomAttributes();
        var validationAttributes = attributes?.OfType<ApiIntValidationAttribute>().FirstOrDefault();
    
        return new ApiParamDescription
        {
            ParamDescription = new CommonDescription
            {
                Name = paramName,
                Description = GetApiMethodParamDescription(methodName, paramName)
            },
    
            Required = attributes?.OfType<ApiRequiredAttribute>().FirstOrDefault()?.Required ?? false,
            MinValue = validationAttributes?.MinValue,
            MaxValue = validationAttributes?.MaxValue
        };
    }
    
    public ApiMethodDescription GetApiMethodFullDescription(string methodName)
    {
        return (methods[methodName].FirstOrDefault()?.GetCustomAttributes<ApiMethodAttribute>().Any() ?? false) ?
            new ApiMethodDescription
            {
                MethodDescription = new CommonDescription
                {
                    Name = methodName,
                    Description = GetApiMethodDescription(methodName)
                },
                ParamDescriptions = methods[methodName].FirstOrDefault()
                    ?.GetParameters()
                    .Select(p => GetApiMethodParamFullDescription(methodName, p.Name))
                    .ToArray(),
                ReturnDescription = ReturnParamFullDescription(methodName)
            }
            : default;
    }
    
    private ApiParamDescription ReturnParamFullDescription(string methodName)
    {
        var attributes = methods[methodName].FirstOrDefault()?.ReturnParameter?.GetCustomAttributes();
    
        if (!attributes.Any()) return null;
    
        var validationAttributes = attributes?.OfType<ApiIntValidationAttribute>().FirstOrDefault();
    
        return new ApiParamDescription
        {
            ParamDescription = new CommonDescription
            {
                Description = attributes?.OfType<ApiDescriptionAttribute>().FirstOrDefault()?.Description
            },
    
            Required = attributes?.OfType<ApiRequiredAttribute>().FirstOrDefault().Required ?? false,
            MinValue = validationAttributes?.MinValue,
            MaxValue = validationAttributes?.MaxValue
        };
    }
}
```

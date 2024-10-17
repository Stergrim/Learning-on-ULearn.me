# Уровень 1

Имена должны выполнять обещания и не вводить читателя в замешательство.

Исходный код:
```cs
void GetFactory()
{
    var user = Environment.UserName;
    this.factory = FactoryCreator(user);
}

TimeSpan GetTimeout()
{
    return this.timeout;
}

void SetTimeout()
{
    var sectionName = systemName + "/timeout";
    this.timeout = ReadSettings(sectionName).Timeout;
}
```

Исправленный код:
```cs
void InitFactory()
{
    var user = Environment.UserName;
    this.factory = CreateFactory(user);
}

TimeSpan GetTimeout()
{
    return this.timeout;
}

void InitTimeoutFromSettings()
{
    var sectionName = systemName + "/timeout";
    this.timeout = ReadSettings(sectionName).Timeout;
}
```

Объяснения:
- Методы GetXXX, CreateXXX, ReadXXX должны возвращать результат. void-методы, инициализирующие поля класса лучше так не называть.
- Методы — это действия, называйте их глаголами или глагольными фразами.
- Методы SetXXX должны принимать устанавливаемое значение в качестве аргумента. Методы без аргументов лучше так не называть.

# Уровень 3

А что ещё, кроме качественных имен может сделать код понятнее?

Исходный код:
```cs
double GetViewsPerSecond(IEnumerable<PageView> views, DateTime t)
{
    var n = 86400;
    var viewsCount = views.Count(v => v.Timestamp.Date == t);
    return (double)viewsCount / n;
}
```

Исправленный код:
```cs
double GetViewsPerSecond(IEnumerable<PageView> views, DateTime date)
{
    var secondsInDay = 24 * 60 * 60;
    var viewsCount = views.Count(v => v.Timestamp.Date == date);
    return (double)viewsCount / secondsInDay;
}
```

Объяснения:
- В именах стоит отражать существенные особенности. Например, если переменная типа DateTime хранит только дату, можно назвать ее date.
- Иногда, арифметические выражения понятнее, чем значение этого выражения. Запись 24 * 60 * 60 проще проверить на корректность, чем 86400.
- Имя переменной должно отражать семантику. Старайтесь избегать однобуквенных имен. Общее правило: чем больше область видимости — тем подробнее должно быть имя.

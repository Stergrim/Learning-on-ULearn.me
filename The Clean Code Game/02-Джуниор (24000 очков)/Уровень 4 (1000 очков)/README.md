# Уровень 4

Продолжаем охоту на слова без смысла.

Исходный код:
```cs
public interface IPriceManager
{
    Json JsonFromXml(XmlDocument prices);
    XmlDocument XmlFromJson(Json prices);
}
```

Исправленный код:
```cs
public interface IPriceFormatConverter
{
    Json JsonFromXml(XmlDocument prices);
    XmlDocument XmlFromJson(Json prices);
}
```

Объяснения:
- Manager — слово заместитель, не добавляющее смысла. Часто его можно заменить на что-то более осмысленное. Например, менеджер по продажам может стать продавцом консультантом.!

# Уровень 6

В составных именах очень легко случайно продемонстрировать незнание английского :-)

Исходный код:
```cs
private string directoryInput;

private string outputDirectory;

public enum QualityRender 
{
    High,
    Medium,
    Low
}
```

Исправленный код:
```cs
private string inputDirectory;

private string outputDirectory;

public enum RenderQuality 
{
    High,
    Medium,
    Low
}
```

Объяснения:
- directoryInput с английского — это ввод директории. Входная директория — это inputDirectory.
- Нарушение правильного порядка слов в составных именах — частая ошибка программистов со слабым знанием английского. Качество рендера — это QualityOfRender или просто RenderQuality.

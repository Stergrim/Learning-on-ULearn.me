# Уровень 6

Хорошие комментарии должны объяснять намерения программиста в тех случаях, когда их сложно выразить непосредственно кодом.

Исходный код:
```cs
//comparison of this and other object
public int CompareTo(object o)
{
    var other = o as WikiPagePath;
    if(other != null)
    {
        //compares concatenated names of this and others
        string thisNames = string.Join("", this.Names);
        string otherNames = string.Join("", other.Names);
        return thisNames.CompareTo(otherNames);
    }
    return 1; // WikiPagePath should be greater than any other wrong type.
} //end of CompareTo
```

Исправленный код:
```cs

public int CompareTo(object o)
{
    var other = o as WikiPagePath;
    if(other != null)
    {

        string thisNames = string.Join("", this.Names);
        string otherNames = string.Join("", other.Names);
        return thisNames.CompareTo(otherNames);
    }
    return 1; // WikiPagePath should be greater than any other wrong type.
} 
```

Объяснения:
- Бессмысленно писать в комментарии то, что итак понятно из названия метода.
- Комментарии дословно повторяющие код бессмысленны.
- Комментарии вида 'конец цикла', 'конец функции' и подобные бессмысленны. Для коротких функций они не нужны, а длинные функции лучше разбить на несколько более коротких, вместо написания таких комментариев.

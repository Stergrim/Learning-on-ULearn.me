# Уровень 5

Проблемы с именами в этом коде очевидны. Но тут есть кое-что еще!

Исходный код:
```cs
void CopyChars(char[] array1, char[] array2)
{
    //copy arrays item by item.
    for(var i = 0; i < array1.Length; i++)
        array2[i] = array1[i];
}
```

Исправленный код:
```cs
void CopyChars(char[] source, char[] destination)
{
    for(var i = 0; i < source.Length; i++)
        destination[i] = source[i];
}
```

Объяснения:
- Нет смысла писать комментарии, повторяющие код.
- Имена с номерами на конце — это антипаттерн. Вместо добавления номеров старайтесь отразить в именах суть различия.
- Функцию с непонятными именами аргументов неудобно использовать.

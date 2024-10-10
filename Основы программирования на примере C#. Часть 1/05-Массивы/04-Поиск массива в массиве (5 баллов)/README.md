# Поиск массива в массиве

Попробуйте найти в массиве не один элемент, а целый подмассив!

Если подмассив найден в массиве, то вернуть нужно минимальный индекс, с которого начинается подмассив в исходном массиве. Например, поиск подмассива "3,4" в массиве "1,2,3,4,3,4" должен вернуть 2.

Более строго это можно записать, если обозначить массив array, а подмассив subarray: функция должна вернуть такое минимальное k, что array[k+i] == subarray[i] для всех i от 0 до subarray.Length-1.

Если подмассив не найден, то вернуть нужно -1.

Считайте, что пустой подмассив содержится в любом массиве, начиная с индекса 0.

Мы помогли вам и написали FindSubarrayStartIndex, оставив нереализованным всего один вспомогательный метод ContainsAtIndex:

```cs
public static int FindSubarrayStartIndex(int[] array, int[] subArray)
{
    for (var i = 0; i < array.Length - subArray.Length + 1; i++)
        if (ContainsAtIndex(array, subArray, i))
            return i;
    return -1;
}
```

Ваша задача реализовать метод ContainsAtIndex, который в нем используется.

Все тесты пройдены, задача сдана:
```cs
public static bool ContainsAtIndex(int[] array, int[] subArray, int i)
{
    bool a = false;
    int b = 0;
    
    if (subArray.Length > 0)
        if (array[i] == subArray[0])
            for (int j = 0; j < subArray.Length; j++)
                if (array[i+j] == subArray[j]) b++;
    
    if (b == subArray.Length) a = true;
    return a;
}
```

Вывод программы:
```cs
OK
```
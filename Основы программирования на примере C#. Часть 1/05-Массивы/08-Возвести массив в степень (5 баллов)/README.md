# Возвести массив в степень

Помогите Васе написать метод, который принимает массив `int[]`, возводит все его элементы в заданную степень и возвращает массив с результатом этой операции.

Исходный массив при этом должен остаться неизменным.

```cs
public static void Main()
{
    var arrayToPower = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    
    // Метод PrintArray уже написан за вас
    PrintArray(GetPoweredArray(arrayToPower, 1));
    
    // если вы будете менять исходный массив, то следующие два теста сработают неверно:
    PrintArray(GetPoweredArray(arrayToPower, 2));
    PrintArray(GetPoweredArray(arrayToPower, 3));
    
    // не забывайте про частные случаи:
    PrintArray(GetPoweredArray(new int[0], 1));
    PrintArray(GetPoweredArray(new[] { 42 }, 0));
}
```

Все тесты пройдены, задача сдана:
```cs
public static int[] GetPoweredArray(int[] arr, int power)
{
	int[] arrTemp = new int[arr.Length];
	for (int i = 0; i < arr.Length; i++)
		arrTemp[i] = (int)Math.Pow(arr[i], power);
	return arrTemp;
}
```

Вывод программы:
```cs
1, 2, 3, 4, 5, 6, 7, 8, 9
1, 4, 9, 16, 25, 36, 49, 64, 81
1, 8, 27, 64, 125, 216, 343, 512, 729

1
```
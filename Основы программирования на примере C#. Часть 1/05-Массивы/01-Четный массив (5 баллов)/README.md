# Четный массив

Напишите метод, который создает массив длинной `count` элементов, содержащий последовательные четные числа в порядке возрастания.

Например, `GetFirstEvenNumbers(3)` должен вернуть массив `2, 4, 6`

Все тесты пройдены, задача сдана:
```cs
public static int[] GetFirstEvenNumbers(int count)
{
	int[] evenNumbers = new int[count];
	for (int i = 0; i < evenNumbers.Length; i++)
		evenNumbers[i] = (i+1)*2;
	return evenNumbers;
}
```

Вывод программы:
```cs
GetFirstEvenNumbers(3)
2 4 6
GetFirstEvenNumbers(5)
2 4 6 8 10
GetFirstEvenNumbers(30)
2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60
GetFirstEvenNumbers(0)

GetFirstEvenNumbers(1)
2
```
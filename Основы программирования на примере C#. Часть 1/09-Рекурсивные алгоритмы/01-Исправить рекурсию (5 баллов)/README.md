# Исправить рекурсию

Вася решил, что изучать рекурсию нужно на простых примерах и начал c программы, печатающей все элементы массива в обратном порядке.

Как это сделать рекурсивно? Очень просто: сначала решить задачу, для всего массива, кроме первого элемента, а потом вывести первый элемент.

Идея проста, но в реализации что-то пошло не так. Видимо, Вася упустил какую-то важную деталь.

Найдите ошибку Васи и помогите ему исправить программу. Естественно, программа должна остаться рекурсивной, ведь именно в этом смысл упражнения!


Все тесты пройдены, задача сдана:
```cs
public static void WriteReversed(char[] items, int startIndex = 0)
{
	if (startIndex == items.Length) return;
    WriteReversed(items, startIndex + 1);
    Console.Write(items[startIndex]); 
}
```

Вывод программы:
```cs
WriteReversed(new char[]{ '1', '2', '3' })
321
WriteReversed(new char[]{ '1', '2' })
21
WriteReversed(new char[]{ '1' })
1
WriteReversed(new char[]{  })

WriteReversed(new char[]{ '1', '1', '2', '2', '3', '3' })
332211
WriteReversed(new char[]{ '1', '2', '3', '4' })
4321
WriteReversed(new char[]{ 'a', 'b', 'c', 'd' })
dcba
```
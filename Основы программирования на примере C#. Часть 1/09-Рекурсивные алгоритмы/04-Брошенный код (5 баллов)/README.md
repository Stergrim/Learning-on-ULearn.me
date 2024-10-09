# Брошенный код :(

Вечер. Звонок в дверь. Через глазок никого не видно.

Вы открыли дверь, а там, о ужас!!! Брошенный, беспомощный, недописанный код перебора всех перестановок! Какой же бессердечный программист мог бросить свое детище недописанным?! Просто непостижимо! Естественно, вы отложили все ваши дела и решили во что бы то ни стало спасти малыша.

Автор явно очень хотел разделить генерацию всех перестановок и их вывод на консоль. Поэтому метод принимает список result, куда предполагается складывать все сгенерированные перестановки.

Допишите код, следуя задумке автора.

```cs
public static void Main()
{
	TestOnSize(1);
	TestOnSize(2);
	TestOnSize(0);
	TestOnSize(3);
	TestOnSize(4);
}

static void TestOnSize(int size)
{
	var result = new List<int[]>();
	MakePermutations(new int[size], 0, result);
	foreach (var permutation in result)
		WritePermutation(permutation);
}

static void WritePermutation(int[] permutation)
{
	var strings = new List<string>();
	foreach (var i in permutation)
		strings.Add(i.ToString());
	Console.WriteLine(string.Join(" ", strings.ToArray()));
}
```

Все тесты пройдены, задача сдана:
```cs
static void MakePermutations(int[] permutation, int position, List<int[]> result)
{
    if (position == permutation.Length) { WritePermutation(permutation); return; }
    else
        for (int i = 0; i < permutation.Length; i++)
        {
            var index = Array.IndexOf(permutation, i, 0, position);
            if (index == -1)
            { permutation[position] = i; MakePermutations(permutation, position + 1, result); }
        }
}
```

Вывод программы:
```cs
0
0 1
1 0

0 1 2
0 2 1
1 0 2
1 2 0
2 0 1
2 1 0
0 1 2 3
0 1 3 2
0 2 1 3
0 2 3 1
0 3 1 2
0 3 2 1
1 0 2 3
1 0 3 2
1 2 0 3
1 2 3 0
1 3 0 2
1 3 2 0
2 0 1 3
2 0 3 1
2 1 0 3
2 1 3 0
2 3 0 1
2 3 1 0
3 0 1 2
3 0 2 1
3 1 0 2
3 1 2 0
3 2 0 1
3 2 1 0
```

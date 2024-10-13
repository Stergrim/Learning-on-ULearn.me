# Делегаты

1. Какой тип соответствует функции, которая принимает два параметра типов int и string и возвращает double? (1 из 1 балла) 
   * 🔴 **delegate<int, string>**
   * 🔴 **delegate**
   * 🔴 **Action**
   * 🔴 **Func**
   * 🔴 **Action<int, string, double>**
   * 🔴 **Action<int, string>**
   * 🟢 **Func<int, string, double>** (Правильно!)


Вы увидели такой код:

```cs
int result = (f[0](0))[0];
```

2. Какой тип может быть у f? (1 из 1 балла) 
   * 🔴 **Func<int>**
   * 🔴 **Func<int>[]**
   * 🔴 **Func<int[]>[]**
   * 🔴 **Func<int[], int>[]**
   * 🟢 **Func<int, List<int>>[]** (Правильно!)
   * 🔴 **Action<Func<int, List<int>>>**
   * 🔴 **Action<Func<int, List<int>>>[]**


Изучите код ниже:

```cs
var list = new List<Action>();

for (var i = 0; i < 5; i++)
    list.Add(() => Console.WriteLine(i));

foreach (var func in list)
    func();
```

3. Что выведется на консоль в результате этого кода? (1 из 1 балла) 
   * 🔴 **0 1 2 3 4**
   * 🔴 **4 4 4 4 4**
   * 🟢 **5 5 5 5 5** (Правильно! Эффект объясняется на слайде 'Ловушка замыкания')
   * 🔴 **0 0 0 0 0**


Изучите код ниже:

```cs
var dictionary = new Dictionary<char, Action>();
for(char c='A'; c < 'Z'; c++)
    dictionary.Add(c, () => Console.Write(c));
		
dictionary['X']();
```

4. Что выводит этот код? (1 из 1 балла) 
   * 🔴 **символ 'A'**
   * 🔴 **символ 'X'**
   * 🔴 **символ 'Y'**
   * 🟢 **символ 'Z'** (Правильно! Из-за ловушки замыкания переменная 'c' будет общей для всех)
   * 🔴 **ошибку**


Изучите код ниже:

```cs
for (var i = 0; i < 5; i++)
{
    var temp = i;
    list.Add(() => Console.WriteLine(temp));
}
```

5. Что выведется на консоль в результате этого кода? (1 из 1 балла) 
   * 🟢 **0 1 2 3 4** (Правильно! Здесь уже не будет ловушки замыкания и переменная i будет индивидуальной для каждого)
   * 🔴 **4 4 4 4 4**
   * 🔴 **5 5 5 5 5**
   * 🔴 **0 0 0 0 0**


Изучите код ниже:

```cs
var list = new List<Action>();
var numbers = new List<int> { 0, 1, 2, 3, 4 };

foreach (var number in numbers)
{
   list.Add(() => Console.WriteLine(number));
}

foreach (var func in list)
    func();
```

6. Что выведется на консоль в результате этого кода? (1 из 1 балла) 
   * 🟢 **0 1 2 3 4** (Правильно! Начиная с версии NetFramework 4.5 общая переменная создается внутри цикла foreach, благодаря чему ловушки замыкания не происходит)
   * 🔴 **4 4 4 4 4**
   * 🔴 **5 5 5 5 5**
   * 🔴 **0 0 0 0 0**

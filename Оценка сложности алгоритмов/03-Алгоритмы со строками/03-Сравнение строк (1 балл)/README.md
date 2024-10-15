# Сравнение строк

Изучите следующий код. Тут GetAllCyclicShifts — в точности метод из предыдущего задания.

```cs
string[] GetAllCyclicShifts(string s)
{
    int n = s.Length;
    string[] shifts = new string[n];
    for (int i = 0; i < n; i++)
        //первый аргумент метода Substring — startIndex, второй  — count
        shifts[i] = s.Substring(i, n - i) + s.Substring(0, i);
    return shifts;
}

int GetRepeatsCount(string[] items)
{
    int repeatsCount = 0;
    for (int i = 0; i < items.Length; i++)
    {
        for (int j = i + 1; j < items.Length; j++)
            if (items[i] == items[j])
                repeatsCount++;
    }
    return repeatsCount;
}

int Main(string s)
{
   return GetRepeatsCount(GetAllCyclicShifts(s));
}
```

Учитывайте, что в C# оператор == производит сравнение строк по содержимому, а не ограничивается сравнением ссылок (как, например, в языке Java).

1. Оцените сложность Main в зависимости от n = s.Length (1 из 1 балла)
   * 🔴 **Θ(1)**
   * 🔴 **Θ(n)**
   * 🔴 **Θ(n²)**
   * 🟢 **Θ(n³)** (Правильно! Вложенные циклы дают квадрат, но и сравнение строк оператором == посимвольное и выполняется за Θ(n). Так что итоговый ответ Θ(n³). Квадратичная сложность вызова GetCyclicShfts на ответ не влияет.)
   * 🔴 **Θ(n⁵)**

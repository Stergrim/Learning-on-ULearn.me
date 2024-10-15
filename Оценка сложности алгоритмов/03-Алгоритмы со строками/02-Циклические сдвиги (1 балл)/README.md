# Циклические сдвиги

А теперь давайте попрактикуемся!

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
```

1. Оцените сложность этого кода в зависимости от n (1 из 1 балла)
   * 🔴 **Θ(1)**
   * 🔴 **Θ(n)**
   * 🟢 **Θ(n²)** (Правильно! Substring имеет линейную сложность. Внешний цикл повторяет эти вызовы n раз. Итого — квадрат!)
   * 🔴 **Θ(n³)**

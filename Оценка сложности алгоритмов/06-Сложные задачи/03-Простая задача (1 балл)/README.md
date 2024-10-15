# Простая задача

А вот это классический алгоритм для поиска простых чисел — решето Эратосфена.

```cs
ISet<int> GetPrimes(int n)
{
    var primes = new HashSet<int>();
    for (int candidate = 2; candidate < n; candidate++)
        primes.Add(candidate);
   
    for (int candidate = 2; candidate * candidate < n; candidate++)
        if (primes.Contains(candidate))
            for (int multiple = candidate * candidate; multiple < n; multiple += candidate)
                primes.Remove(multiple);
    return primes;
}
```

Можете считать, что методы Add и Remove у HashSet работают за O(1).

Остальная часть кода очень похожа на предыдущую задачу. Существенных изменений два:
- Проверка primes.Contains.
- Внутренний цикл начинается со step².


1. Выберите из списка наиболее точную оценку сложности для GetPrimes. (1 из 1 балла)
   * 🔴 **O(log(n))**
   * 🔴 **O(n sqrt(n))**
   * 🟢 **O(n log(n))** (Правильно! Это было просто! Все изменения только уменьшают сложность, поэтому оценка из предыдущей задачи всё ещё верна, O(log(n)) очевидно не верна, а остальные оценки более грубые.)
   * 🔴 **O(n log²(n))**
   * 🔴 **O(n²)**

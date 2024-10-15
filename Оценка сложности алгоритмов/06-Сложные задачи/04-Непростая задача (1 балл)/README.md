# Непростая задача

Что если объединить решето Эратосфена с ToBinaryString из первой задачи этого раздела?

Теперь оцените целиком сложность метода GetPrimesInBinary.

```cs
string ToBinaryString(int n)
{
   return n == 0 ? "0" : ToBinaryString(n / 2) + (n % 2).ToString();
}

ISet<string> GetPrimesInBinary(int n)
{
    var set = new HashSet<string>();
    for (int i = 2; i < n; i++)
        set.Add(ToBinaryString(i));
    for (int step = 2; step * step < n; step++)
        for (int i = step * step; i < n; i += step)
            set.Remove(ToBinaryString(i));
    return set;
}
```

Опять же постарайтесь не выписывать сложные ряды. Вместо сражения за точную Θ-оценку, постарайтесь оценить составные части алгоритма разумным образом сверху, чтобы быстро получить адекватную O-оценку.

1. Выберите из списка наиболее точную оценку сложности GetPrimesInBinary? (1 из 1 балла)
   * 🔴 **O(n² log(n))**
   * 🔴 **O(n log²(n))**
   * 🔴 **O(n³ log(n))**
   * 🟢 **O(n log³(n))** (Правильно! Оценим сверху все вызовы ToBinaryString как O(log² n) и вспомним про частичные суммы гармонического ряда.)
   * 🔴 **O(n² log³(n))**
   * 🔴 **O(n³ log²(n))**

# Быстрое возведение в степень

Предыдущий код можно ускорить, если не делать рекурсивные вызовы с одинаковыми аргументами. Вот так:

```cs
int FastPower(int x, int n)
{
    if (n == 0) return 1;
    int halfPower = FastPower(x, n / 2);
    int result = halfPower * halfPower;
    if (n % 2 == 0) return result;
    else return result * x;
}
```

1. Какая сложность у FastPower? (1 из 1 балла)
   * 🔴 **Θ(1)**
   * 🟢 **Θ(log(n))** (Правильно! После улучшения, код стал копией другой предыдущей задачи!)
   * 🔴 **Θ(n)**
   * 🔴 **Θ(n log(n))**
   * 🔴 **Θ(n²)**

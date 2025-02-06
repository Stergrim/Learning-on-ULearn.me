# Практика «Счастливые билеты»

Необходимо посчитать количество «счастливых» билетов с заданной суммой цифр, среди тех, номер которых состоит из 2N разрядов. «Счастливым» является билет, у которого сумма первых N цифр равна сумме N последних цифр.

Решение выполните в проекте [Tickets](Tickets.zip)

Источник задачи — [acm.timus.ru](https://acm.timus.ru/problem.aspx?num=1036)

Все тесты пройдены, задача сдана:
```cs
using System;
using System.Numerics;

namespace Tickets;

public class TicketsTask
{
    public static BigInteger Solve(int halfLen, int totalSum)
    {
        var result = BigInteger.Zero;
        if (totalSum % 2 != 0) return result;
    
        var halfSum = totalSum / 2;
        var table = new BigInteger[halfSum + 1, halfLen];
    
        for (var i = 0; i <= Math.Min(halfSum,9); ++i) { table[i, 0] = 1; }
        for (var j = 0; j < halfLen; ++j) { table[0, j] = 1; }
    
        for (var j = 1; j < halfLen; ++j)
            for (var i = 1; i <= halfSum; ++i)
                for (var k = 0; k <= Math.Min(i, 9); k++)
                    table[i, j] += table[i - k, j - 1];
    
        result = table[halfSum, halfLen - 1];
        return result * result;
    }
}
```

# Практика «Контрольный разряд»

Скачайте проект [SRP.ControlDigit](SRP.ControlDigit.zip).

В серийные номера, номера счетов и коды продуктов обычно включают так называемый контрольный разряд, значение которого вычисляется по остальным цифрам номера. Он нужен, чтобы подтверждать отсутствие ошибок при вводе этих номеров вручную или при считывании их с помощью сканеров.

Есть несколько стандартизированных алгоритмов вычисления контрольного разряда. Прочитать их краткое описание с примерами можно в соответствующей [статье википедии](https://en.wikipedia.org/wiki/Check_digit).

В проекте уже реализован один метод UPC. Причем реализован без какой-либо декомпозиции. Вам нужно реализовать две других функции, но это не главное. Главное, провести декомпозицию имеющегося кода, разбив его на небольшие повторно используемые функции. Весь код логически разделите на общий код и специфичный.

Общий код поместите в класс Extensions в виде методов расширения. Там должны оказаться только методы с понятными логичными сигнатурами, которые имеют шанс быть использованными в другом контексте.

Специфичный код — это код, который использует специфику задачи, например содержит название алгоритма в имени метода. Весь специфичный код оставьте в классе ControlDigitAlgo в виде приватных методов (ведь мы все равно не ожидаем, что они будут повторно использованы каким-то кодом извне).

Постарайтесь минимизировать количество специфичного кода и максимизировать количество общего кода.

Считайте, что эти методы не будут вызываться слишком часто, поэтому не нужно пытаться их оптимизировать, вместо этого сосредоточьтесь на простоте, понятности и возможности повторного использования.

В этой задаче действует ограничение на длину методов — не более 10 строк. Но не надо пытаться записывать сразу много операторов в одной строке — это дурной тон в программировании. Вместо этого постарайтесь выделить из длинных методов самодостаточные примитивы в отдельные методы.

После окончания ещё раз посмотрите на сигнатуры всех методов. Точно ли они будут понятны другому программисту без подглядывания в их код?

Все тесты пройдены, задача сдана:
```cs
namespace SRP.ControlDigit;

public static class Extensions
{
    public static List<int> ToDigits(this long number)
    {
        var result = new List<int>();
        while (number > 0)
        {
            result.Add((int)(number % 10));
            number /= 10;
        }
        return result;
    }
    
    public static int CalculateProgression(this List<int> digits, int factor, int coefficient, bool dash)
    {
        var result = 0;
        foreach (var digit in digits)
        {
            result += factor * digit;
            factor = coefficient + (dash ? -1 : 1) * factor;
        }
        return result;
    }
}

public static class ControlDigitAlgo
{
    public static int Upc(long number)
    {
        var sum = number.ToDigits().CalculateProgression(3, 4, true);
        var result = sum % 10 != 0 ? 10 - sum % 10 : 0;
        return result;
    }
    
    public static int Isbn10(long number)
    {
        var sum = number.ToDigits().CalculateProgression(2, 1, false);
        var result = sum % 11 != 0 ? 11 - sum % 11 : 0;
        return result == 10 ? 'X' : result.ToString()[0];
    }
    
    public static int Luhn(long number)
    {
        var sum = number.ToDigits()
            .Select((num, index) => index % 2 != 0 ? num : num * 2)
            .Select(num => num > 9 ? num - 9 : num)
            .Sum();
        var result = sum % 10 != 0 ? 10 - sum % 10 : 0;
        return result;
    }
}
```

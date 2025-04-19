# Практика «Рациональные числа»

При работе над математическими или геометрическими задачами часто приходится создавать "фундаментальные" классы, подобные int или double, для комплексных чисел, кватернионов и т.д. Скачайте [проект Incapsulation.RationalNumbers](Incapsulation.RationalNumbers.zip) и напишите класс рационального числа.

Ваше решение должно проходить тесты, содержащиеся в проекте.

Все тесты пройдены, задача сдана:
```cs
namespace Incapsulation.RationalNumbers;

public readonly struct Rational
{
    public readonly int Numerator;
    public readonly int Denominator;
    public bool IsNan => Denominator == 0;
    
    public Rational(int numerator, int denominator = 1)
    {
        var reduced = Reduce(numerator, denominator);
        Numerator = reduced.numerator;
        Denominator = reduced.denominator;
    }
    
    private static (int numerator, int denominator) Reduce(int numerator, int denominator)
    {
        if (denominator == 0) return (1, denominator);
        if (numerator == 0) return (0, 1);
        var start = Math.Max(Math.Abs(numerator),Math.Abs(denominator));
        var divider = Math.Min(Math.Abs(numerator),Math.Abs(denominator));
        while (start % divider != 0)
        {
            var result = start % divider;
            start = divider;
            divider = result;
        }
    
        return denominator < 0
            ? (-numerator / divider, -denominator / divider)
            : (numerator / divider, denominator / divider);
    }
    
    private static bool IsValid(Rational a, Rational b) => !a.IsNan && !b.IsNan;
    private static Rational GetNanRational => new(1, 0);
    
    public static Rational operator +(Rational a, Rational b)
    {
        if (!IsValid(a, b)) return GetNanRational;
        var reduced = Reduce(
            a.Numerator * b.Denominator + b.Numerator * a.Denominator,
            a.Denominator * b.Denominator
        );
        
        return new Rational(reduced.numerator,reduced.denominator);
    }
    
    public static Rational operator -(Rational a, Rational b)
    {
        if (!IsValid(a, b)) return GetNanRational;
        var reduced = Reduce(
            a.Numerator * b.Denominator - b.Numerator * a.Denominator,
            a.Denominator * b.Denominator
        );
    
        return new Rational(reduced.numerator, reduced.denominator);
    }
    
    public static Rational operator *(Rational a, Rational b)
    {
        if (!IsValid(a, b)) return GetNanRational;
        var reduced = Reduce(
            a.Numerator * b.Numerator,
            a.Denominator * b.Denominator
        );
    
        return new Rational(reduced.numerator, reduced.denominator);
    }
    
    public static Rational operator /(Rational a, Rational b)
    {
        if (!IsValid(a, b)) return GetNanRational;
        var reduced = Reduce(
            a.Numerator * b.Denominator,
            a.Denominator * b.Numerator
        );
    
        return new Rational(reduced.numerator, reduced.denominator);
    }
    
    public static implicit operator double(Rational a)
    => a.IsNan ? double.NaN : (double)a.Numerator / a.Denominator;
    
    public static explicit operator int(Rational a)
    {
        if (a.IsNan) return 0;
        if (a.Denominator != 1) throw new Exception();
        return a.Numerator / a.Denominator;
    }
    
    public static implicit operator Rational(int value) => new(value);
}
```

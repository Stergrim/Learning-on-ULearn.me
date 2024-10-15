# Составление словаря

Текст задан массивом строк. Вам нужно составить лексикографически упорядоченный список всех встречающихся в этом тексте слов.

Слова нужно сравнивать регистронезависимо, а выводить в нижнем регистре.

```cs
public static void Main()
{
    var vocabulary = GetSortedWords(
        "Hello, hello, hello, how low",
        "",
        "With the lights out, it's less dangerous",
        "Here we are now; entertain us",
        "I feel stupid and contagious",
        "Here we are now; entertain us",
        "A mulatto, an albino, a mosquito, my libido...",
        "Yeah, hey"
    );
    foreach (var word in vocabulary)
        Console.WriteLine(word);
}
```


Все тесты пройдены, задача сдана:
```cs
public static string[] GetSortedWords(params string[] textLines)
{
    return (Regex.Split(string.Join(" ", textLines).ToLower(), @"\W+"))
           .Distinct()
           .OrderBy(word => word)
           .ToArray();
}
```

Вывод программы:
```cs
a
albino
an
and
are
contagious
dangerous
entertain
feel
hello
here
hey
how
i
it
less
libido
lights
low
mosquito
mulatto
my
now
out
s
stupid
the
us
we
with
yeah
```

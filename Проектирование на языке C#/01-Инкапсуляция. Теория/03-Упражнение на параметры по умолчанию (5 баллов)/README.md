# Упражнение на параметры по умолчанию

С очень важного сайта скачали текстовый файл с важными данными. Но вместо текста там кракозябры! Вот примерно такие:

╨£╤ï ╨╛╤ç╨╡╨╜╤î ╨╜╨░╨┤╨╡╨╡╨╝╤ü╤Å, ╤ç╤é╨╛ ╤é╨╡╨▒╨╡ ╨╜╤Ç╨░╨▓╨╕╤é╤ü╤Å ╨╕╨╖╤â╤ç╨░╤é╤î C# ╨╜╨░ ╨╜╨░╤ê╨╡╨╝ ╨║╤â╤Ç╤ü╨╡!

Так часто бывает, когда перепуталась кодировка файла. Текст получен из байт с помощью неправильной кодировки. Нужно преобразовать текст обратно в байты, а потом снова из байтов в текст, но с правильной кодировкой.

У нас уже есть код, решающий эту задачу, но чтобы им было удобнее пользоваться, есть перегрузки методов со значениями по умолчанию для неправильной кодировки и правильной кодировки.

Отрефакторите код так, чтобы возможность не указывать одну или обе кодировки осталась, но метод стал ровно один вместо трёх.

```cs
public static void Main()
{
    CodePagesEncodingProvider.Instance.GetEncoding(437);
    Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    
    var texts = GetTextsWithWrongEncoding();
    var extendedAscii = Encoding.GetEncoding(437);
    Console.WriteLine(MyFile.Decode(texts[0]));
    Console.WriteLine(MyFile.Decode(texts[1], Encoding.UTF8));
    Console.WriteLine(MyFile.Decode(texts[2], Encoding.UTF8, extendedAscii));
    SecretTests();
}
```

Все тесты пройдены, задача сдана:
```cs
class MyFile
{   
    public static string Decode(string textWithWrongEncoding, Encoding rightEncoding = null, Encoding wrongEncoding = null)
    {
        rightEncoding ??= Encoding.UTF8;
        wrongEncoding ??= Encoding.GetEncoding(437);
        return rightEncoding.GetString(wrongEncoding.GetBytes(textWithWrongEncoding));
    }
}
```

Вывод программы:
```cs
Привет, студент!
Мы очень надеемся, что тебе нравится изучать C# на нашем курсе!
Если нашел ошибку или опечатку пиши на почту: support@ulearn.me
Успехов и удачи!
Test 1: Ok!
Test 2: Ok!
Test 3: Ok!
Methods count: 1
```

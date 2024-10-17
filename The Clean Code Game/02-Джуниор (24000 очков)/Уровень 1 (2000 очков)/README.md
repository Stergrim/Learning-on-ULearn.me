# Уровень 1

Осторожнее — начиная с этого уровня, за каждый неверный клик и каждую подсказку вы будете терять один балл!

Исходный код:
```cs
var rstr = Console.ReadLine();
var flag = false;
for(var charIndex = 0; charIndex < rstr.Length; charIndex++)
{
    if (flag || rstr[charIndex] != '\\')
        Console.Write(rstr[charIndex]);
    flag = rstr[charIndex] == '\\';
}
```

Исправленный код:
```cs
var line = Console.ReadLine();
var escaped = false;
for(var charIndex = 0; charIndex < line.Length; charIndex++)
{
    if (escaped || line[charIndex] != '\\')
        Console.Write(line[charIndex]);
    escaped = line[charIndex] == '\\';
}
```

Объяснения:
- Избегайте труднпрзнсимых имен и кодирования, понятного лишь вам.
- Не называйте булевы переменные flag, f и подобными именами. У каждого "флага" есть смысл, который и нужно отразить в имени.

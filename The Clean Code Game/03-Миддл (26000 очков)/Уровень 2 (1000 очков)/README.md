# Уровень 2

Имена переменных и методов тут в порядке. Но не только это делает код хорошим!

Исходный код:
```cs
void Main(string[] args)
{
    string[] inputLines = File.ReadAllLines(args[0]);
    string outputPath = args[1];
    if (Directory.Exists(outputPath))
        outputPath = Path.Combine(outputPath, Path.GetFileName(args[0]));
    //Convert file
    var escapedLines = inputLines.Select(s => s.Replace(@"", @"\"));
    var outputText = string.Join("\n", escapedLines);
    File.WriteAllText(outputPath, outputText);
    Console.WriteLine("{0} characters", outputText.Length);
}
```

Исправленный код:
```cs
void Main(string[] args)
{
    string[] inputLines = File.ReadAllLines(args[0]);
    string outputPath = args[1];
    if (Directory.Exists(outputPath))
        outputPath = Path.Combine(outputPath, Path.GetFileName(args[0]));
    ConvertFile(inputLines, outputPath);
}

void ConvertFile(IEnumerable<string> inputLines, string outputPath)
{
    var escapedLines = inputLines.Select(s => s.Replace(@"", @"\"));
    var outputText = string.Join("\n", escapedLines);
    File.WriteAllText(outputPath, outputText);
    Console.WriteLine("{0} characters", outputText.Length);
}
```

Объяснения:
- Вместо комментария, разделяющего код на две фазы, стоит сделать настоящее разделение кода на методы. Каждый метод должен делать ровно одну вещь. Если вы можете естественным образом разбить один метод на два — сделайте это!

# Практика «Парсер полей»

[Скачайте проект TableParser](TableParser.zip).

Наконец, вы готовы к тому, чтобы реализовать всю задачу, поставленную два слайда назад!

В классе FieldsParserTask реализуйте метод ParseLine, для которого вы создавали тесты в предыдущей задаче.

Создайте модульные тесты на это решение и перенесите разработанные в прошлой задаче тестовые случаи в модульные тесты.

Решение получится более простым, если ваши вспомогательные методы будут использовать `Token` в качестве возвращаемого значения.

В качестве вспомогательных методов могут быть методы, читающие разные виды полей (у вас уже реализован метод `ReadQuotedField`), а также метод пропускающий пробелы между полями.

Обратите внимание на метод `GetIndexNextToToken` в классе `Token`. Он возвращает позицию, с которой нужно продолжить анализ строки.


Все тесты пройдены, задача сдана:
```cs
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;

namespace TableParser
{
    [TestFixture]
    public class FieldParserTaskTests
    {
        public static void Test(string input, string[] expectedResult)
        {
            var actualResult = FieldsParserTask.ParseLine(input);
            Assert.AreEqual(expectedResult.Length, actualResult.Count);
            for (int i = 0; i < expectedResult.Length; ++i)
                Assert.AreEqual(expectedResult[i], actualResult[i].Value);
        }
    
        [TestCase("text", new[] { "text" })]
        [TestCase("hello      world", new[] { "hello", "world" })]
        [TestCase("' ", new[] { " " })]
        [TestCase(@"'a\' b'", new[] { "a' b" })]
        [TestCase("a 'b' 'c' d", new[] { "a", "b", "c", "d" })]
        //[TestCase("s 'b' 'c' dd", new[] { "s", "b", "c", "dd" })]
        [TestCase("'' \"bcd ef\" 'x y'", new[] { "", "bcd ef", "x y" })]
        [TestCase("a\"b", new[] { "a", "b" })]
        [TestCase("'\"'", new[] { "\"" })]
        [TestCase(" df ", new[] { "df" })]
        [TestCase("b \"a'\"", new[] { "b", "a'" })]
        [TestCase(@"""\\""", new[] { "\\" })]
        [TestCase("'\"\"\"\"'", new[] { "\"\"\"\"" })]
        [TestCase(@"""\""", new[] { "\"" })]
        [TestCase("", new string[0])]
    
        public static void RunTests(string input, string[] expectedOutput)
        { Test(input, expectedOutput); }
    }
    
    public class FieldsParserTask
    {
        public static List<Token> ParseLine(string line)
        {
            int startIndex = 0;
            var list = new List<Token> { };
            while(startIndex < line.Length)
                startIndex = GetToken(list, line, startIndex);
            return list;
        }
        
        public static int GetToken(List<Token> list, string line, int startIndex)
        {
            if (line[startIndex] == '\'') {
                list.Add(QuotesFieldOne(line, startIndex));
                startIndex = list[list.Count - 1].GetIndexNextToToken();
            }
            else if (line[startIndex] == '\"') {
                list.Add(QuotesFieldTwo(line, startIndex));
                startIndex = list[list.Count - 1].GetIndexNextToToken();
            }
            else if (line[startIndex] == ' ') startIndex = Space(line, startIndex);
            else {
                list.Add(SimpleField(line, startIndex));
                startIndex = list[list.Count - 1].GetIndexNextToToken();
                 }
            return startIndex;
        }
    
        private static Token SimpleField(string line, int startIndex)
        {
            StringBuilder str = new StringBuilder();
            int len = line.Length;
            for (int i = startIndex; i < len; i++)
            {
                if ((line[i] == ' ') || (line[i] == '\'') || (line[i] == '\"')) break;
                str.Append(line[i]);
            }
            line = str.ToString();
            return new Token(line, startIndex, line.Length);
        }
    
        private static Token QuotesFieldOne(string line, int startIndex)
        {
            StringBuilder str = new StringBuilder();
            int len = line.Length;
            int j = 0;
    
            for (int i = startIndex + 1; i < len; i++)
            {
                if (line[i] == '\'') { j++; break; }
                if (line[i] == '\\') { j++; i++; }
                str.Append(line[i]);
            }
            line = str.ToString();
            return new Token(line, startIndex, line.Length + 1 + j);
        }
    
        private static Token QuotesFieldTwo(string line, int startIndex)
        {
            StringBuilder str = new StringBuilder();
            int len = line.Length;
            int j = 0;
            for (int i = startIndex + 1; i < len; i++)
            {
                if (line[i] == '\"') { j++; break; }
                if (line[i] == '\\') { j++; i++; }
                str.Append(line[i]);
            }
            line = str.ToString();
            return new Token(line, startIndex, line.Length + 1 + j);
        }
    
        private static int Space(string line, int startIndex)
        {
            int len = line.Length;
            for (int i = startIndex; i < len; i++)
            {
                if (line[i] == ' ') startIndex = i + 1;
                else break;
            }
            return startIndex;
        }
    
        private static Token ReadField(string line, int startIndex)
        {
            return new Token(line, 0, line.Length);
        }
    
        public static Token ReadQuotedField(string line, int startIndex)
        { 
            return QuotedFieldTask.ReadQuotedField(line, startIndex);
        }
    }
}
```

**Проект со всеми внесенными решениями.**
[TableParser Edit](TableParser_Edit.zip)

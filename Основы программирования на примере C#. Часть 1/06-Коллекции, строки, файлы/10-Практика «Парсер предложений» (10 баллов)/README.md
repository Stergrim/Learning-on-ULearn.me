# Практика «Парсер предложений»

[Скачайте проект TextAnalysis](TextAnalysis.zip).

В этом задании нужно реализовать метод в классе `SentencesParserTask`. Метод должен делать следующее:
 - Разделять текст на предложения, а предложения на слова.
    - a. Считайте, что слова состоят только из букв (используйте метод `char.IsLetter`) или символа апострофа `'` и отделены друг от друга любыми другими символами.
    - b. Предложения состоят из слов и отделены друг от друга одним из следующих символов `.!?;:()`
 - Приводить символы каждого слова в нижний регистр.
 - Пропускать предложения, в которых не оказалось слов.
Метод должен возвращать список предложений, где каждое предложение — это список из одного или более слов в нижнем регистре


Все тесты пройдены, задача сдана:
```cs
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;

namespace TextAnalysis
{
    static class SentencesParserTask
    {
        public static List<List<string>> ParseSentences(string text)
        {
            var sentencesList = new List<List<string>>();
            string[] temp = text.Split(new char[] { '.', '!', '?', ';', ':', '(', ')' });
            var length = temp.GetLength(0);
            StringBuilder word = new StringBuilder();
            for (int i = 0; i < length; i++)
            {
                temp[i] = temp[i].ToLower();
                temp[i] = Regex.Replace(temp[i], @"([^A-Za-z'])", " ");
                var list = new List<string>();
                for (int j = 0; j < temp[i].Length; j++)
                {
                    if (char.IsLetter(temp[i][j]) || (temp[i][j] == '\''))
                        word.Append(temp[i][j]);
                    BuildString(list, temp, word, i, j);
                }
                if (list.Count != 0) sentencesList.Add(list);
            }
            return sentencesList;
		}
		
		public static void BuildString(List<string> list, string[] temp, StringBuilder word, int i, int j)
		{
			if (!char.IsLetter(temp[i][j]) &&
				(temp[i][j] != '\'') &&
				(word.Length > 0) ||
				(temp[i][j] == ','))
			{
				list.Add(word.ToString());
				word.Clear();
			}
			else if ((j == temp[i].Length - 1) && (word.Length > 0))
			{
				list.Add(word.ToString());
				word.Clear();
			}
		}
    }
}
```

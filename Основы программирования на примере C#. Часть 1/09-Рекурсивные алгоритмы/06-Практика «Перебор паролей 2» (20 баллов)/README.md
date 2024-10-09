# Практика «Перебор паролей 2»

Вася сменил пароль на новый и забыл его!

На этот раз он точно помнит, что он сконструировал пароль из старого пароля, поменяв регистр нескольких букв. Он, конечно, не хочет вам говорить старый пароль, поэтому просит написать программу, которая по заданному слову перебирает все возможные пароли, полученные из этого слова заменой регистра.

Для удобства мы создали для вас [проект](Passwords.zip), который будет тестировать вашу программу. Вам лишь остается дописать класс CaseAlternatorTask в одноименном файле.

Для удобства Вася просит, чтобы пароли появлялись в лексикографическом порядке, считая, что маленькие буквы меньше больших. Естественно, регистр нужно менять только у букв.

Например, для входного слова `'ab42'` результат должен быть такой: `'ab42', 'aB42', 'Ab42', 'AB42'`

На вход подается слово в нижнем регистре. В результирующем списке не должно быть повторений слов.

Помните, что у вас в распоряжении есть методы `char.IsLetter`, `char.ToLower` и `char.ToUpper`.


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Linq;
using System.Globalization;

namespace Passwords
{
    public class CaseAlternatorTask
    {
        public static List<string> AlternateCharCases(string lowercaseWord)
        {
            var result = new List<string>();
            AlternateCharCases(lowercaseWord.ToCharArray(), 0, result);
            return result;
        }

        static void AlternateCharCases(char[] word, int startIndex, List<string> result)
        {
            if (startIndex >= word.Length) result.Add(new string(word));
            else
            {
                char[] subword = word.ToArray();
        
                if ((!char.IsLetter(word[startIndex])) || (word[startIndex] == 'ß') ||
                    (char.GetUnicodeCategory(word[startIndex]) != UnicodeCategory.LowercaseLetter))
                    AlternateCharCases(subword, startIndex + 1, result);
                else
                {
                    AlternateCharCases(subword, startIndex + 1, result);
                    word[startIndex] = char.ToUpper(word[startIndex]);
                    result.Add(new string(word));
                    result.Remove(result.Last());
                    AlternateCharCases(word, startIndex + 1, result);
                }
            }
        }
    }
}
```

# Больше словарей

Увидев какой некрасивый текст находится внутри ячеек, Васе дали задание убрать весь мусор из строк и реализовать хранение данных в более читаемой сущности.

Ему нужно дописать решение прошлой задачи. Необходимо преобразовать каждый вложенный список в словарь, где ключом является название столбца csv, а значением пересечение столбца с текущей строкой.

Дополнительные требования:
- Если значение содержит `\n` (перенос строки), то его нужно разбить на список
- У всех значений (внутри вложенных списков тоже) должны удаляться пробелы (в начале, в конце строки и лишние по середине). А так же они должны быть очищены от HTML тегов.
- Вывод всех полей в формате `key: value`
- Если значение поля пустое, то в значении выводить "Нет данных"
- Вложенные списки должны выводиться через точку с запятой
- Между отдельными объектами одна пустая строка (подробнее в Формате вывода)

Прочитав все требования, Вася изрядно понервничал, но решил, что доведёт дело до конца!

**Формат ввода**

В csv-файле используется следующая структура:

```
name,key_skills,premium,employer_name,salary_from,salary_to,area_name
Программист,"Информационные технологии
Автоматизированное рабочее место (АРМ)",FALSE,Контур,70000,110000,Москва
Инженер,"Ответственность
Работа в команде
Умение работать руками
пунктуальность
Ответственный подход к работе",FALSE,Сервисный центр ЭКСПЕРТ,30000,60000,Санкт-Петербург
```

[Скачать пример файла](example_vacancies.zip)

**Формат вывода**

```
name: Программист
key_skills: Информационные технологии; Автоматизированное рабочее место (АРМ)
premium: FALSE
employer_name: Контур
salary_from: 70000
salary_to: 110000
area_name: Москва

name: Инженер
key_skills: Ответственность; Работа в команде; Умение работать руками; пунктуальность; Ответственный подход к работе
premium: FALSE
employer_name: Сервисный центр ЭКСПЕРТ
salary_from: 30000
salary_to: 60000
area_name: Санкт-Петербург
```

|                         |                        |
|:------------------------|:-----------------------|
| **Ограничение времени** | **1 секунда**          |
| **Ограничение памяти**  | **64.0 Мб**            |
| **Ввод**                | **название CSV файла** |
| **Вывод**               | **стандартный вывод**  |


Все тесты пройдены, задача сдана:
```py
import csv
import re

def normalize_html(text):
    text = re.sub(r"<[^>]+>", "", text).strip()
    text = re.sub(r"\s+", " ", text)
    return text

def normalize_cell(value):
    if not value.strip():
        return 'Нет данных'

    parts = [normalize_html(part) for part in value.split('\n') if part.strip()]
    return '; '.join(parts) if parts else 'Нет данных'

def parse_csv(filename):
    with open(filename, 'r', encoding='utf-8-sig', newline='') as file:
        csv_reader = csv.reader(file)
        headers = next(csv_reader)

        normalized = []

        for lines in csv_reader:
            if sum([1 for cell in lines if cell]) * 2 >= len(headers):
                record = {}
                for index, line in enumerate(lines):
                    record[headers[index]] = normalize_cell(line)
                normalized.append(record)
    return normalized

data = parse_csv(input())
for i, record in enumerate(data):
    for key, value in record.items():
        print(f"{key}: {value}")

    if i != len(data) - 1:
        print()
```

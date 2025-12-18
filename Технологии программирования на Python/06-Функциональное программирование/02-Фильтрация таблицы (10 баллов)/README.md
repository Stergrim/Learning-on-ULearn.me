# Фильтрация таблицы

Следующей на очереди за дополнительной функциональностью была Вероника.

Веронике, вместе с той функциональностью потребовалась фильтрация данных по любым полям в вакансиях.

Васе пришлось не спать ночами, прежде чем он смог порадовать аналитиков новой версией своей программки. Теперь программа безумно нравилась самому Василию и он ее долго тестировал и проверял различные пограничные ситуации.

Итак, вот какой вариант своей программы предложил Вася, который нравился и Васе и Веронике:
1. Формат вывода всех данные в виде таблички сохраняется.
2. Первая строка входных данных - название CSV с вакансиями для обработки.
3. Во второй строке ввода ожидается параметр фильтрации например: ’Компания: Аконит-Урал’.
   - Поля Название, Описание, Навыки, Опыт работы, Премиум вакансия, Компания, Название региона, Дата публикации вакансии проверяются на наличие значения ввёденного параметра.
   - У поля оклад два фильтра: "Оклад" и "Идентификатор валюты оклада"
   - Введённый оклад должен входить в диапазон `salary_from <= input <= salary_to`
   - Если параметра фильтрации нет, то осуществлять фильтрацию не требуется
4. Третья строка порядковые номера вакансий - нумерация в пределах отфильтрованных данных.
5. Четвертая строка Названия столбцов, которые нужно выводить в таблице - функционал прежний.
6. При написании программы пользоваться функциональным подходом (использовать декораторы, лямбда-функциями, генераторами и т.д.)


**Формат ввода**

```
vacancies.csv
Навыки: CSS
1 8
Название, Навыки, Опыт работы
```

[Скачать пример файла](example.zip)

**Формат вывода**

```
+---+----------------------+----------------------+--------------------+
| № | Название             | Навыки               | Опыт работы        |
+---+----------------------+----------------------+--------------------+
| 1 | HTML-верстальщик     | CSS3, HTML5, jQuery, | От 1 года до 3 лет |
|   |                      | Less, Bootstrap,     |                    |
|   |                      | Vuetify, Vue, Jest   |                    |
|   |                      |                      |                    |
|   |                      |                      |                    |
|   |                      |                      |                    |
|   |                      |                      |                    |
+---+----------------------+----------------------+--------------------+
| 2 | HTML-верстальщик     | CSS3, HTML5, jQuery, | От 1 года до 3 лет |
|   | (remote)             | Less, Bootstrap,     |                    |
|   |                      | Vuetify, Vue         |                    |
|   |                      |                      |                    |
|   |                      |                      |                    |
|   |                      |                      |                    |
+---+----------------------+----------------------+--------------------+
| 3 | Web-разработчик PHP  | MySQL, PHP, CMS      | От 1 года до 3 лет |
|   | (full stack)         | Wordpress, Sass,     |                    |
|   |                      | Figma, Веб-          |                    |
|   |                      | программирование,    |                    |
|   |                      | REST API, HTML-      |                    |
|   |                      | верстка, CSS-        |                    |
|   |                      | верстка, С...        |                    |
+---+----------------------+----------------------+--------------------+
| 4 | Ночной специалист    | HTML, CSS, Грамотная | От 1 года до 3 лет |
|   | технической          | речь, Обучение       |                    |
|   | поддержки (удаленно) | персонала, CMS       |                    |
|   |                      | Wordpress            |                    |
|   |                      |                      |                    |
|   |                      |                      |                    |
+---+----------------------+----------------------+--------------------+
| 5 | Web-разработчик /    | MySQL, PHP, HTML5,   | От 3 до 6 лет      |
|   | Специалист по        | jQuery, CMS          |                    |
|   | интернет-продвижению | Wordpress, Figma,    |                    |
|   |                      | SEO, SEO             |                    |
|   |                      | оптимизация, CSS,    |                    |
|   |                      | Yii2, Продвижение    |                    |
|   |                      | сайтов...            |                    |
|   |                      |                      |                    |
+---+----------------------+----------------------+--------------------+
| 6 | Ассистент            | Работа в команде,    | От 1 года до 3 лет |
|   | руководителя         | Управление временем, |                    |
|   | технического отдела  | Управление интернет- |                    |
|   |                      | проектами, CMS       |                    |
|   |                      | Wordpress,           |                    |
|   |                      | 1С-Битрикс, HTM...   |                    |
|   |                      |                      |                    |
+---+----------------------+----------------------+--------------------+
| 7 | Ассистент            | Работа в команде,    | От 1 года до 3 лет |
|   | руководителя         | Управление временем, |                    |
|   | технического отдела  | Управление интернет- |                    |
|   |                      | проектами, CMS       |                    |
|   |                      | Wordpress,           |                    |
|   |                      | 1С-Битрикс, HTM...   |                    |
|   |                      |                      |                    |
+---+----------------------+----------------------+--------------------+
| 8 | Ассистент            | Работа в команде,    | От 1 года до 3 лет |
|   | руководителя         | Управление временем, |                    |
|   | технического отдела  | Управление интернет- |                    |
|   |                      | проектами, CMS       |                    |
|   |                      | Wordpress,           |                    |
|   |                      | 1С-Битрикс, HTM...   |                    |
|   |                      |                      |                    |
+---+----------------------+----------------------+--------------------+
```

**Примечания**

Примеры параметров для фильтрации данных:
- Навыки: Git
- Оклад: 100000
- Дата публикации вакансии: 17/07/2022
- Опыт работы: От 3 до 6 лет
- Премиум-вакансия: Нет
- Идентификатор валюты оклада: Рубли
- Название: Программист
- Название региона: Екатеринбург
- Компания: Аконит-Урал


|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **1 секунда** |
| **Ограничение памяти**  | **64.0 Мб**   |


Все тесты пройдены, задача сдана:
```py
import csv
import re
from prettytable import PrettyTable, ALL

def csv_reader(file_name):
    with open(file_name, 'r', encoding='utf-8-sig', newline='') as file:
        reader = csv.DictReader(file)
        titles = reader.fieldnames
        vacancies = list(reader)
    return titles, vacancies

def range_detector(range_string):
    if range_string:
        parts = range_string.split()

        if not parts:
            return None, None
        start = int(parts[0]) if len(parts) > 0 else None
        end = int(parts[1]) if len(parts) > 1 else None

        return start, end
    return None, None

def apply_filter(key, value, vacancies):
    pattern = re.compile(r'(\d[\d\s]*)\s*-\s*(\d[\d\s]*)\s*\(([^)]+)\)\s*\(([^)]+)\)')
    salary_key = 'Оклад'

    if key == 'Оклад':
        value = int(value)

    def check(vacancy):
        if key == 'Оклад':
            parts = pattern.search(vacancy[salary_key])
            salary_from, salary_to = (int(parts.group(1).replace(' ', '')),
                                      int(parts.group(2).replace(' ', '')))
            return salary_from <= value <= salary_to
        elif key == 'Идентификатор валюты оклада':
            parts = pattern.search(vacancy[salary_key])
            if not parts:
                return False
            currency = parts.group(3)
            return currency == value
        else:
            return value in vacancy[key]
    return [vacancy for vacancy in vacancies if check(vacancy)]

def formatter(vacancy):
    formatted = {}
    for title in vacancy:
        value = vacancy[title]
        value = value.strip()
        if len(value) > 100:
            value = value[:100] + '...'
        formatted[title] = value
    return formatted

def print_table(vacancies, titles, selected_titles=None, start=None, end=None):
    if start is not None:
        vacancies = vacancies[start - 1 :]
    if end is not None:
        vacancies = vacancies[: end - start + 1] if start else vacancies[:end]

    columns = (['№'] + selected_titles) if selected_titles else (['№'] + titles)

    table = PrettyTable()
    table.field_names = ['№'] + titles
    table.max_width = 20
    table.hrules = ALL
    table.align = "l"

    for i, vacancy in enumerate(vacancies, start=start or 1):
        formatted = formatter(vacancy)
        row = [i]
        for title in titles:
            value = formatted.get(title, '')
            row.append(value)
        table.add_row(row)

    print(table.get_string(fields=columns))

def main():
    file_name = input()
    filter_string = input()
    selected_range = input()
    selected_titles = input()

    titles, vacancies = csv_reader(file_name)
    start, end = range_detector(selected_range)
    if filter_string:
        parts = filter_string.split(': ')
        key, value = parts
        vacancies = apply_filter(key, value, vacancies)

    if not selected_titles:
        selected_titles = None
    else:
        selected_titles = [col.strip() for col in selected_titles.split(',')]

    print_table(vacancies, titles, selected_titles, start, end)

if __name__ == '__main__':
    main()
```

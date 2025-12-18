# Функции в ООП

После формирования основных объектов в программе, Василий решил перенести функции в классы, таким образом, сделать их методами.

Кроме того у Васи сформировались еще новые вспомогательные классы, при обработке данных и при выводе их на печать.

В результате у Василия получилась следующая архитектура программы:
- `class Vacancy` - устанавливает все основные поля вакансии
- `class Salary` - устанавливает все основные поля зарплаты
- `class DataSet` - отвечает за чтение и подготовку данных из CSV-файла (универсальный парсер CSV)
- `class Utils` - отвечает за различные утилиты. В вашем случае, здесь должна содержаться метод, ответственный за создание таблицы

Необходимо написать программу, которая выводит данные о вакансиях в виде таблицы `PrettyTable`. При написании программы необходимо пользоваться классами

**Формат ввода**

```
vacancies.csv
```

[Скачать пример файла](example_vacancies.zip)

**Формат вывода**

<p float="left"> <img src="example.png" width="1200" /> </p>

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **2 секунды** |
| **Ограничение памяти**  | **128.0 Мб**  |


Все тесты пройдены, задача сдана:
```py
import csv
from var_dump import var_dump
import prettytable

class DataSet:
    def __init__(self, filename):
        self.filename = filename

    def csv_reader(self):
        with open(self.filename, 'r', encoding='utf-8-sig', newline='') as file:
            reader = csv.DictReader(file)
            titles = reader.fieldnames
            vacancies = list(reader)

        return titles, vacancies

class Utils:
    def __init__(self, titles, vacancies):
        self.titles = titles
        self.vacancies = vacancies

    @staticmethod
    def formatter(value):
        if len(value) > 100:
            value = value[:100] + '...'
        return value

    def print_table(self):
        table = prettytable.PrettyTable()
        table.hrules = True
        table.vrules = True
        table.field_names = ["№"] + self.titles
        for col in table.field_names:
            table.max_width[col] = 20
        table.align = "l"

        for index, vacancy in enumerate(self.vacancies, start=1):
            row_data = vacancy.to_dict()
            row = [index]
            for t in self.titles:
                row.append(self.formatter(row_data.get(t, "")))
            table.add_row(row)

        print(table)

class Vacancy:
    def __init__(self, vacancy, salary_obj):
        self.name = vacancy.get('name', '')
        self.description = vacancy.get('description', '')
        self.key_skills = vacancy.get('key_skills', '')
        self.experience_id = vacancy.get('experience_id', '')
        self.premium = vacancy.get('premium', '')
        self.employer_name = vacancy.get('employer_name', '')
        self.salary = salary_obj
        self.area_name = vacancy.get('area_name', '')
        self.published_at = vacancy.get('published_at', '')

    def to_dict(self):
        data = {}
        for key, value in self.__dict__.items():
            if key == 'salary':
                for s_key, s_value in value.__dict__.items():
                    data[s_key] = s_value
            else:
                data[key] = value
        return data

class Salary:
    def __init__(self, vacancy):
        self.salary_from = vacancy.get('salary_from', '')
        self.salary_to = vacancy.get('salary_to', '')
        self.salary_gross = vacancy.get('salary_gross', '')
        self.salary_currency = vacancy.get('salary_currency', '')

def main():
    filename = input()
    dataset = DataSet(filename)
    titles, vacancies = dataset.csv_reader()
    result_vacancies = []
    for vacancy in vacancies:
        salary_obj = Salary(vacancy)
        vacancy_obj = Vacancy(vacancy, salary_obj)
        result_vacancies.append(vacancy_obj)

    utils = Utils(titles, result_vacancies)
    utils.print_table()

if __name__ == '__main__':
    main()
```

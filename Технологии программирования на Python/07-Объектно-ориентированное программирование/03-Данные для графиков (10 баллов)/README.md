# Данные для графиков

“Теперь уже все...” - с облегчением сказал Вася, когда его программу полностью одобрили старшие коллеги программисты, после его рефакторинга кода с ООП.

И, правда, все. Васю приняли в штат сотрудников компании и прикрепили к отделу Аналитики для разработки следущих аналитических программ.

Дальше пошла размеренная жизнь, где Васю уже воспринимали как полноценного члена команды, постоянно прислушиваясь к его мнению.

Из следующих работ, в продолжение печати отчетов по вакансиям, где аналитики просматривали вакансии в разных разрезах и готовили предложения для отдела HR Васю попросили улучшить функциональность его программы теперь уже для обработки статистических данных.

Для программы по обработке статистических данных Василию дали подготовленный CSV-файл содержащий нужные столбцы для анализа (файл может содержать данные HH с `2003` по `2024` год):
- name
- salary_from
- salary_to
- salary_currency
- area_name
- published_at

К программе Василия написали следующие требования в задаче:
1. Вводится два параметра:
   - Название csv-файла
   - Название профессии
2. Название файла - корректное название файла содержащего неоходимые данные
3. Название профессии - произвольное название профессии, для вывода подбираются только вакансии содержащие в названии указанное значение (включение, но не жесткое сравнение)
4. Программа должна выводить следующие данные:
   - Динамика уровня зарплат по годам (по возрастанию года, с округлением зарплаты до целого)
   - Динамика количества вакансий по годам (по возрастанию года)
   - Динамика уровня зарплат по годам для выбранной профессии (по возрастанию года, с округлением зарплаты до целого)
   - Динамика количества вакансий по годам для выбранной профессии (по возрастанию года)
   - Уровень зарплат по городам (в порядке убывания) - только первые 10 значений (с округлением зарплаты до целого)
   - Доля вакансий по городам (в порядке убывания) - только первые 10 значений (с округлением до 4 знаков после запятой)
5. Зарплата считается как среднее арифметическое между `salary_from` и `salary_to`
6. Если зарплата указана в другой валюте, то необходимо произвести перерасчет по указанному курсу.


**Формат ввода**

```
vacancies.csv
разработчик
```

[Скачать пример файла](example_vacancies.zip)

**Формат вывода**

```
Средняя зарплата по годам: {2003: 81875, 2005: 106000, 2006: 86700, 2007: 100367, 2008: 107500, 2009: 54962, 2010: 85000, 2011: 120000, 2012: 22600, 2013: 75225, 2014: 203774, 2015: 55000, 2016: 105504, 2019: 72500, 2020: 88833, 2022: 78547, 2023: 97500, 2024: 100495}
Количество вакансий по годам: {2003: 4, 2005: 4, 2006: 5, 2007: 3, 2008: 2, 2009: 3, 2010: 1, 2011: 1, 2012: 1, 2013: 2, 2014: 3, 2015: 1, 2016: 8, 2019: 1, 2020: 3, 2022: 3, 2023: 3, 2024: 2}
Средняя зарплата по годам для профессии 'разработчик': {2003: 90000, 2005: 48500, 2007: 37500, 2011: 120000, 2014: 27297, 2016: 210000}
Количество вакансий по годам для профессии 'разработчик': {2003: 1, 2005: 1, 2007: 1, 2011: 1, 2014: 1, 2016: 1}
Средняя зарплата по городам: {'Санкт-Петербург': 108260, 'Москва': 108122, 'Новосибирск': 90000, 'Минск': 75056, 'Екатеринбург': 75000, 'Красногорск': 72500, 'Рощино (Ленинградская область)': 65000, 'Нижний Новгород': 60450, 'Петрозаводск': 59000, 'Саратов': 58500}
Доля вакансий по городам: {'Москва': 0.54, 'Санкт-Петербург': 0.2, 'Минск': 0.06, 'Новосибирск': 0.02, 'Рощино (Ленинградская область)': 0.02, 'Екатеринбург': 0.02, 'Нижний Новгород': 0.02, 'Нур-Султан': 0.02, 'Сочи': 0.02, 'Феодосия': 0.02}
```

**Примечания**

Для конвертации других валют в рубли для правильной сортировки данных использовать следующие курсы валют:

```
currency_to_rub = {
    "Манаты": 35.68,
    "Белорусские рубли": 23.91,
    "Евро": 59.90,
    "Грузинский лари": 21.74,
    "Киргизский сом": 0.76,
    "Тенге": 0.13,
    "Рубли": 1,
    "Гривны": 1.64,
    "Доллары": 60.66,
    "Узбекский сум": 0.0055,
}
```

|                         |              |
|:------------------------|:-------------|
| **Ограничение времени** | **8 секунд** |
| **Ограничение памяти**  | **128.0 Мб** |


Все тесты пройдены, задача сдана:
```py
import csv
from collections import defaultdict
from datetime import datetime

currency_to_rub = {
    "Манаты": 35.68,
    "Белорусские рубли": 23.91,
    "Евро": 59.90,
    "Грузинский лари": 21.74,
    "Киргизский сом": 0.76,
    "Тенге": 0.13,
    "Рубли": 1,
    "Гривны": 1.64,
    "Доллары": 60.66,
    "Узбекский сум": 0.0055,
}

class Salary:
    def __init__(self, salary_from, salary_to, salary_currency):
        self.salary_from = float(salary_from) if salary_from else 0
        self.salary_to = float(salary_to) if salary_to else 0
        self.salary_currency = salary_currency

    def to_rub(self):
        average_salary = (self.salary_from + self.salary_to) / 2 if self.salary_from and self.salary_to else 0
        return average_salary * currency_to_rub.get(self.salary_currency, 0)

class Vacancy:
    def __init__(self, name, salary, area_name, published_at):
        self.name = name
        self.salary = salary
        self.area_name = area_name
        self.published_at = datetime.strptime(published_at, '%H:%M:%S %d/%m/%Y').year

class DataSet:
    def __init__(self, filename):
        self.filename = filename
        self.vacancies = self._read_csv()

    def _read_csv(self):
        vacancies = []
        with open(self.filename, encoding='utf-8-sig') as file:
            reader = csv.DictReader(file)
            for row in reader:
                salary = Salary(
                    salary_from=row['salary_from'],
                    salary_to=row['salary_to'],
                    salary_currency=row['salary_currency']
                )
                vacancy = Vacancy(
                    name=row['name'],
                    salary=salary,
                    area_name=row['area_name'],
                    published_at=row['published_at']
                )
                vacancies.append(vacancy)
        return vacancies

class Statistics:
    def __init__(self, dataset):
        self.dataset = dataset

    def get_stats(self, profession_name):
        salary_by_year = defaultdict(list)
        count_by_year = defaultdict(int)
        salary_by_profession = defaultdict(list)
        count_by_profession = defaultdict(int)
        salary_by_city = defaultdict(list)
        count_by_city = defaultdict(int)

        for vacancy in self.dataset.vacancies:
            year = vacancy.published_at
            city = vacancy.area_name
            salary_rub = vacancy.salary.to_rub()

            salary_by_year[year].append(salary_rub)
            count_by_year[year] += 1

            if profession_name.lower() in vacancy.name.lower():
                salary_by_profession[year].append(salary_rub)
                count_by_profession[year] += 1

            salary_by_city[city].append(salary_rub)
            count_by_city[city] += 1

        average_salary_by_year = {year: int(round(sum(salaries) / len(salaries))) for year, salaries in salary_by_year.items()}
        average_salary_by_profession = {year: int(round(sum(salaries) / len(salaries))) for year, salaries in salary_by_profession.items()}
        average_salary_by_city = {city: int(round(sum(salaries) / len(salaries))) for city, salaries in salary_by_city.items()}
        total_vacancies = sum(count_by_city.values())
        share_by_city = {city: round(count / total_vacancies, 4) for city, count in count_by_city.items()}

        average_salary_by_year = dict(sorted(average_salary_by_year.items(), key=lambda x: x[0]))
        count_by_year = dict(sorted(count_by_year.items(), key=lambda x: x[0]))
        average_salary_by_profession = dict(sorted(average_salary_by_profession.items(), key=lambda x: x[0]))
        count_by_profession = dict(sorted(count_by_profession.items(), key=lambda x: x[0]))
        top_cities_salary = dict(sorted(average_salary_by_city.items(), key=lambda x: x[1], reverse=True)[:10])
        top_cities_share = dict(sorted(share_by_city.items(), key=lambda x: x[1], reverse=True)[:10])

        print(f"Средняя зарплата по годам: {average_salary_by_year}")
        print(f"Количество вакансий по годам: {count_by_year}")
        print(f"Средняя зарплата по годам для профессии '{profession_name}': {average_salary_by_profession}")
        print(f"Количество вакансий по годам для профессии '{profession_name}': {count_by_profession}")
        print(f"Средняя зарплата по городам: {top_cities_salary}")
        print(f"Доля вакансий по городам: {top_cities_share}")

def main():
    filename = input()
    profession_name = input()
    dataset = DataSet(filename)
    statistics = Statistics(dataset)
    statistics.get_stats(profession_name)

if __name__ == '__main__':
    main()
```

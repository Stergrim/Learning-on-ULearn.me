# Chaining

Вася вспомнил ту жуткую и большую задачу из раздела "Объектно-ориентированное программирование", которая называлась "Данные для графиков". И ему пришла воистину гениальная идея! Ведь всё, что Вася уже написал, есть в Pandas. И тем, что написано в Pandas можно (и даже нужно) пользоваться!

Да и на работе как раз немного изменили условие задачи, чтобы Василию просто не жилось:
1. Входными данными служит название профессии.
2. Название профессии - произвольное название профессии, для вывода подбираются только вакансии содержащие в названии указанное значение (включение, но не жесткое сравнение).
3. Программа должна выводить следующие данные:
   - Динамика уровня зарплат по годам.
   - Динамика количества вакансий по годам.
   - Динамика уровня зарплат по годам для выбранной профессии (Если за год нет данных о выбранной вакансии, то указывать для этого года значение "0").
   - Динамика количества вакансий по годам для выбранной профессии (Если за год нет данных о выбранной вакансии, то указывать для этого года значение "0").
   - Уровень зарплат по городам для выбранной профессии (в порядке убывания) - только первые 10 значений.
   - Доля вакансий по городам для выбранной профессии (в порядке убывания) - только первые 10 значений.
4. Для статистики по городам, учитывать только те города, в которых кол-во вакансий больше или равно `1%` от общего числа вакансий `(при вычислении 1% применять округление вниз)`.
5. Рассматриваются только те вакансии, у которых валюта зарплаты указана в российских рублях (RUR). Вакансии с другой валютой не рассматриваются.
6. Если же у городов совпадают значения, будь то это "Уровень зарплат по городам" или "Доля вакансий по городам" - нужно сортировать их в алфавитном порядке.
7. Решение реализовать с максимальным использованием возможностей библиотеки Pandas.
8. При решении задач учитывать [антипаттерны работы с Pandas](https://habr.com/ru/companies/wunderfund/articles/682388/).
9. Обязательным условием для получения максимального балла является использование функционального стиля программирования.
10. Учитывать только вакансии, опубликованные за последние 5 лет (в качестве отсчёта брать последнюю дату, представленную в таблице).

Для этой задачи будет использоваться [эта таблица](example_vacancies.zip).

**Формат ввода**

```
аналитик
```

**Формат вывода**

```
Динамика уровня зарплат по годам: {2019: 68415, 2020: 71505, 2021: 86799, 2022: 89302, 2023: 99932}
Динамика количества вакансий по годам: {2019: 336, 2020: 440, 2021: 735, 2022: 596, 2023: 110}
Динамика уровня зарплат по годам для выбранной профессии: {2019: 54333, 2020: 83857, 2021: 129306, 2022: 99846, 2023: 107338}
Динамика количества вакансий по годам для выбранной профессии: {2019: 6, 2020: 7, 2021: 22, 2022: 24, 2023: 8}
Уровень зарплат по городам для выбранной профессии (в порядке убывания): {'Москва': 147900, 'Красноярск': 121206, 'Санкт-Петербург': 111511, 'Воронеж': 93750, 'Клин': 92235, 'Екатеринбург': 82250, 'Казань': 76666, 'Новосибирск': 75000, 'Пермь': 72500, 'Самара': 71250}
Доля вакансий по городам для выбранной профессии (в порядке убывания): {'Москва': 0.4478, 'Санкт-Петербург': 0.0746, 'Казань': 0.0448, 'Владивосток': 0.0299, 'Воронеж': 0.0299, 'Екатеринбург': 0.0299, 'Омск': 0.0299, 'Самара': 0.0299, 'Саратов': 0.0299, 'Белгород': 0.0149}
```

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **3 секунды** |
| **Ограничение памяти**  | **512.0 Мб**  |


Все тесты пройдены, задача сдана:
```py
import re
import pandas as pd

def get_medium(x):
    if (x['salary_from'] == x['salary_to']).all():
        return x['salary_from']
    return (x['salary_from'] + x['salary_to']) / 2

def get_vac_by_city(data):
    k = data.shape[0]
    one_percent = k // 100

    d = (data.groupby(['area_name']).agg({
        'count': 'count',
        'medium_salary': 'mean'
    })
         .assign(vacancy_count=lambda x: x['count'])
         .assign(count=lambda x: round(x['count'] / k, 4))
         .query(f'vacancy_count >= {one_percent}')
         .astype({'medium_salary': 'int'}))

    d = d.drop(columns=['vacancy_count'])
    return (d.sort_values(['medium_salary', 'area_name'], ascending=(False, True))[:10].to_dict()['medium_salary'],
            d.sort_values(['count', 'area_name'], ascending=(False, True))[:10].to_dict()['count'])

def get_vac_by_years(data, all_years_range=None):
    if data.empty:
        if all_years_range is not None:
            salary_dict = {year: 0 for year in all_years_range}
            count_dict = {year: 0 for year in all_years_range}
            return salary_dict, count_dict
        return {}, {}

    grouped = data.groupby('published_at').agg({
        'medium_salary': 'mean',
        'count': 'sum'
    }).reset_index()

    if all_years_range is not None:
        years_range = all_years_range
    else:
        min_year = data['published_at'].min()
        max_year = data['published_at'].max()
        years_range = range(min_year, max_year + 1)

    salary_dict = {}
    count_dict = {}
    for year in years_range:
        year_data = grouped[grouped['published_at'] == year]
        if not year_data.empty:
            salary_dict[year] = int(year_data['medium_salary'].iloc[0])
            count_dict[year] = int(year_data['count'].iloc[0])
        else:
            salary_dict[year] = 0
            count_dict[year] = 0

    return salary_dict, count_dict

def get_year_vacancy(s):
    for j in re.findall(r'\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\+\d{4}', s):
        s = s.replace(j, j[0:4])
    return int(s)

def print_data(name_vac, name_file):
    vacancies = pd.read_csv(name_file)
    vacancies = vacancies[vacancies['salary_currency'] == 'RUR']
    vacancies['published_at'] = vacancies['published_at'].apply(get_year_vacancy)

    last_year = vacancies['published_at'].max()
    start_year = last_year - 4
    vacancies = vacancies[vacancies['published_at'] >= start_year]
    
    vacancies['salary_from'] = vacancies['salary_from'].fillna(vacancies['salary_to'])
    vacancies['salary_to'] = vacancies['salary_to'].fillna(vacancies['salary_from'])
    vacancies['medium_salary'] = get_medium(vacancies)
    vacancies['count'] = 1

    level_sal, count_sal = get_vac_by_years(vacancies)
    print(f'Динамика уровня зарплат по годам: {level_sal}')
    print(f'Динамика количества вакансий по годам: {count_sal}')

    vacancies_filtered = vacancies[vacancies['name'].str.contains(name_vac)]
    level_sal_job, count_sal_job = get_vac_by_years(vacancies_filtered, range(start_year, last_year + 1))
    print(f'Динамика уровня зарплат по годам для выбранной профессии: {level_sal_job}')
    print(f'Динамика количества вакансий по годам для выбранной профессии: {count_sal_job}')

    level_sal_city, count_sal_city = get_vac_by_city(vacancies_filtered)
    print(f'Уровень зарплат по городам для выбранной профессии (в порядке убывания): {level_sal_city}')
    print(f'Доля вакансий по городам для выбранной профессии (в порядке убывания): {count_sal_city}')

file_name = 'vacancies_for_learn.csv'
vac_name = input()
print_data(vac_name, file_name)
```

# Работа с Matplotlib

**Продолжайте работу в том же проекте.**

Отдел аналитики не перестает эксплуатировать Василия. Теперь от него требуют графики для визуализации данных, полученных на предыдущем этапе с Excel-таблицами.

Аналитики поставили следующие требования к графикам:
1. Предыдущая функциональность сохраняется.
2. Графики должны строиться по собранной статистике.
3. На одном изображении должно быть размещено 4 варианта графиков.
4. Для всех графиков выводить соответствующие заголовки.
5. Первый вариант графика: диаграмма - уровень зарплат по годам для вывода динамики уровня зарплат по годам как общий, так и для выбранной профессии:
   - По оси Ox: года с 2007 по 2022. Подписи оси Ox расположить вертикально.
   - По оси Oy: уровень зарплат по годам общий и для выбранной профессии на одном графике. Для оси Oy выводить сетку.
   - На графике выводить легенду для используемых цветов диаграммы.
   - Размер шрифта для всех подписей (в т.ч. и для легенды): 8.
   - Цвета по умолчанию.
6. Второй вариант графика: диаграмма - количество вакансий по годам как общий, так и для выбранной профессии:
   - По оси Ox: года с 2007 по 2022. Подписи оси Ox расположить вертикально.
   - По оси Oy: уровень зарплат по годам общий и для выбранной профессии на одном графике. Для оси Oy выводить сетку.
   - На графике выводить легенду для используемых цветов диаграммы.
   - Размер шрифта для всех подписей (в т.ч. и для легенды): 8.
   - Цвета по умолчанию.
7. Третий вариант графика: горизонтальная диаграмма - уровень зарплат по городам:
   - По оси Ox: шкала зарплат.
   - По оси Oy: города сверху вниз в порядке убывания зарплат.
   - Для вместимости подписей шкалы Oy названия городов состоящих из двух слов (через пробел или дефис) использовать перенос слов с помощью \n. Горизонтальное выравнивание подписей по правому краю, вертикальное выравнивание подписей по центру. Размер шрифта: 6.
   - Размер шрифта для всех остальных подписей 8.
   - Цвет по умолчанию.
   - Данный график строится только по общей статистике (см. пример).
8. Четверый вариант графика: круговая диаграмма - количество вакансий по городам.
   - Для построения диаграммы рассчитать долю других городов не входящих в ТОП-10 городов с подписью "Другие".
   - Выводить подписи для долей выводить 6 размером шрифта.
   - Также как и предыдущий, график строится только по общей статистике.
9. Для выставления автоматических оступов использовать встроенную функцию matplotlib.
10. Для формирования графиков используется функция create_plot, которая использует данные, сформированные при помощи функций из предыдущей задачи.

Для выполнения этого задания создайте функцию `create_plot`, которая создаёт необходимый график и возвращает переменную подграфиков, которая и используется для наполнения графика данными.

На вход подаётся название выбранной профессии.

**Формат ввода**

```
Python-разработчик
```

**Формат вывода**

```
def create_plot():
    fig, sub = plt.subplots(2, 2)
    ...
    return sub
```

**Формат графика**

<p float="left"> <img src="graph_example.png" width="800" /> </p>

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **1 секунда** |
| **Ограничение памяти**  | **512.0 Мб**  |


Все тесты пройдены, задача сдана:
```py
import re
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from matplotlib.axes import Axes
from openpyxl.workbook import Workbook
from openpyxl.utils.dataframe import dataframe_to_rows

def get_medium(x):
    if x['salary_from'].equals(x['salary_to']):
        return x['salary_from']
    return (x['salary_from'] + x['salary_to']) / 2

def get_year_vacancy(s):
    for j in re.findall(r'\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\+\d{4}', s):
        s = s.replace(j, j[0:4])
    return int(s)

def get_vac_by_years(data):
    d = data.groupby(['published_at']).agg({
        'medium_salary': 'mean',
        'count': 'count',
    }).assign(medium_salary=lambda x: np.round(x['medium_salary']))
    return d

def get_vac_by_city(data):
    k = data.shape[0]
    d = (data.groupby(['area_name']).agg({
        'medium_salary': 'mean',
        'count': 'count',
    })
         .assign(count=lambda x: round(x['count'] / k * 100, 2))
         .query('count >= 1')
         .assign(medium_salary=lambda x: np.round(x['medium_salary'])))
    return (d.sort_values(['medium_salary', 'area_name'], ascending=(False, True))[:10]['medium_salary'],
            d.sort_values(['count', 'area_name'], ascending=(False, True))[:10]['count'])

def wrap_city_name(city):
    parts = city.replace('-', ' ').split()
    return "\n".join(parts)

def create_report():
    csv = 'vacancies.csv'
    vacancies = parse_csv(csv)

    d = get_vac_by_years(vacancies)
    wb = Workbook()
    wb.create_sheet(title='Статистика по годам', index=0)
    wb.create_sheet(title='Статистика по городам', index=1)
    sheetnames = wb.sheetnames
    wb.remove(wb[sheetnames[2]])
    sheet = wb[sheetnames[0]]
    sheet.append(['Год', 'Средняя зарплата', 'Количество вакансий'])
    for i, r in enumerate(dataframe_to_rows(d, index=True, header=True)):
        if i >= 2:
            sheet.append(r)

    salary_vac, count_vac = get_vac_by_city(vacancies)
    sheet = wb[sheetnames[1]]
    sheet.append(['Город', 'Уровень зарплат'])
    for i, r in enumerate(dataframe_to_rows(pd.DataFrame(salary_vac), index=True, header=True)):
        if i >= 2:
            sheet.append(r)

    sheet.append(['Город', 'Доля вакансий, %'])
    for i, r in enumerate(dataframe_to_rows(pd.DataFrame(count_vac), index=True, header=True)):
        if i >= 2:
            sheet.append(r)
    sheet.move_range("A12:B22", rows=-11, cols=3)
    wb.save('student_works/report.xlsx')

def parse_csv(csv):
    names = ['name', 'salary_from', 'salary_to', 'salary_currency', 'area_name', 'published_at']
    vacancies = (pd.read_csv(csv, names=names)
                 .assign(salary_from=lambda x: x['salary_from'].fillna(x['salary_to']))
                 .assign(salary_to=lambda x: x['salary_to'].fillna(x['salary_from']))
                 .assign(published_at=lambda x: x['published_at'].apply(get_year_vacancy))
                 .assign(count=0)
                 .assign(medium_salary=get_medium))
    return vacancies

def create_plot():
    csv = 'vacancies.csv'
    vac = input()
    vacancies = parse_csv(csv)

    fig, sub = plt.subplots(2, 2, figsize=(14, 8))
    vac_by_years = get_vac_by_years(vacancies)['medium_salary'].to_dict()
    vac_by_years_count = get_vac_by_years(vacancies)['count'].to_dict()
    vacancies_filtered = vacancies[vacancies['name'].str.contains(vac, na=False, case=False)]
    vac_by_years_filtered = get_vac_by_years(vacancies_filtered)['medium_salary'].to_dict()
    vac_by_years_filtered_count = get_vac_by_years(vacancies_filtered)['count'].to_dict()

    ax: Axes = sub[0, 0]
    years = sorted(vac_by_years.keys())
    x = np.arange(len(years))
    width = 0.3
    y_all = [vac_by_years.get(year, 0) for year in years]
    y_filtered = [vac_by_years_filtered.get(year, 0) for year in years]
    ax.bar(x - width / 2, y_all, width, label='средняя з/п')
    ax.bar(x + width / 2, y_filtered, width, label=f'з/п {vac}')
    ax.set_title('Уровень зарплат по годам', fontsize=8)
    ax.set_xticks(x)
    ax.set_xticklabels(years, rotation=90, fontsize=8)
    ax.tick_params(axis='y', labelsize=8)
    ax.legend(fontsize=8)
    ax.grid(True, alpha=0.3)

    ax = sub[0, 1]
    y_all_count = [vac_by_years_count.get(year, 0) for year in years]
    y_filtered_count = [vac_by_years_filtered_count.get(year, 0) for year in years]
    ax.bar(x - width / 2, y_all_count, width, label='Количество вакансий')
    ax.bar(x + width / 2, y_filtered_count, width, label=f'Количество вакансий {vac}')
    ax.set_title('Количество вакансий по годам', fontsize=8)
    ax.set_xticks(x)
    ax.set_xticklabels(years, rotation=90, fontsize=8)
    ax.legend(fontsize=8)
    ax.grid(True, alpha=0.3)

    ax = sub[1, 0]
    level_by_cities, count_by_cities = get_vac_by_city(vacancies)
    cities = list(level_by_cities.index)[:10]
    salaries = list(level_by_cities.values)[:10]
    y_pos = np.arange(len(cities))
    ax.barh(y_pos, salaries)
    ax.set_yticks(y_pos)
    ax.set_yticklabels(
        [wrap_city_name(c) for c in cities],
        fontsize=6,
        horizontalalignment='right',
        verticalalignment='center'
    )
    ax.tick_params(axis='x', labelsize=8)
    ax.set_title('Уровень зарплат по городам', fontsize=8)
    ax.invert_yaxis()
    ax.grid(True, alpha=0.3)

    ax = sub[1, 1]
    cities_count = list(count_by_cities.index)[:10]
    counts = list(count_by_cities.values)[:10]
    cities_count.append("Другие")
    counts.append(max(0.0, 100.0 - sum(counts)))
    ax.pie(counts, labels=cities_count, autopct='%1.1f%%', startangle=90, textprops={'fontsize': 6})
    ax.set_title('Доля вакансий по городам', fontsize=8)

    plt.tight_layout()
    plt.show()
    return sub
```

# Поиск и сортировка

Вася, как и любой уважающий себя питонист, обязан знать основы [Pandas](https://pandas.pydata.org/docs/). Поэтому Вася решил отработать свои скиллы на знакомой нам [vacancies_small.csv](vacancies_small.zip) Он захотел сделать самый настоящий фильтр вакансий! На вход должны подаваться:
- Имя столбца.
- Подстрока, которая может содержаться в столбце. (может быть в любом регистре, может быть неполной).
- Имя столбца, по которому будет происходить сортировка.
- Тип сортировки (asc, если сортировка по возрастанию; desc, если сортировка по убыванию)

На выходе должен получиться список из имён вакансий, соответствующий заданным параметрам. В случае равенства значений строк после сортировки, необходимо дополнительно отсортировать эти строки по их индексам в порядке возрастания.

**Формат ввода**

```
area_name
Екатеринбург
salary_from
asc
```

**Формат вывода**

```
['Ассистент руководителя технического отдела', 'Менеджер проектов / менеджер по продажам (IT-решений)', 'Аналитик (Контур.Фокус)', 'Программист 1С', 'Бизнес аналитик 1С']
```

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **3 секунды** |
| **Ограничение памяти**  | **512.0 Мб**  |


Все тесты пройдены, задача сдана:
```py
import pandas as pd

def filter_csv(df, column, sort_by, keyword, sort_type):
    ascending = True if sort_type == 'asc' else False
    filtered_df = df[df[column].str.contains(keyword, case=False, na=False)].copy()
    filtered_df = filtered_df.sort_index()
    filtered_df = filtered_df.sort_values(
        by=sort_by,
        ascending=ascending,
        kind='mergesort'
    )
    return filtered_df['name'].tolist()

def main():
    vacancies = pd.read_csv('vacancies_small.csv')
    column = input()
    keyword = input()
    sort_by = input()
    sort_type = input()
    filtered = filter_csv(vacancies, column, sort_by, keyword, sort_type)
    print(filtered)

if __name__ == "__main__":
    main()
```

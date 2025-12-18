# Counter

Вася решил на этом не останавливаться. Теперь ему интересно узнать самые популярные скиллы в той или иной профессии. На вход должно подаваться `Имя профессии` (может быть в любом регистре, может быть неполным). На выходе должен получиться список из ПЯТИ самых популярных скиллов, с количеством упоминаний того или иного скилла. Как и в предыдущей задаче, здесь указывается тип сортировки (asc, если сортировка по возрастанию; desc, если сортировка по убыванию)

**Формат ввода**

```
программист
asc
```

**Формат вывода**

```
[('PHP', 3), ('1С: Документооборот', 3), ('1С: Управление Производственным Предприятием', 4), ('1С: Зарплата и управление персоналом', 5), ('1С программирование', 5)]
```

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **3 секунды** |
| **Ограничение памяти**  | **512.0 Мб**  |


Все тесты пройдены, задача сдана:
```py
import pandas as pd
from collections import Counter

vacancies = pd.read_csv('vacancies_small.csv')
name = input()
sort_type = False if input() == 'asc' else True

def format_cell(x):
    if isinstance(x, str):
        return x.split('\n')

df = vacancies[vacancies['name'].str.contains(name, case=False, na=False)]
skills = df[df['key_skills'].notna()]
skills = skills['key_skills'].apply(lambda x: format_cell(x)).tolist()
counter = Counter(sum(skills, []))
print(sorted(counter.most_common(5), key=lambda x: x[1], reverse=sort_type))
```

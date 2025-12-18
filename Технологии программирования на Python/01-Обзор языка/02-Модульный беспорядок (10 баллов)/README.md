# Модульный беспорядок

В порыве энтузиазме Вася рванулся писать сложное приложение на Python, использующее много сторонних модулей. Однако он забыл про описание зависимостей, и теперь у него код не запускается!

Помогите Васе исправить ситуацию! Не меняя кода Васи, исправьте ошибки, подключив все нужные модули!

Все тесты пройдены, задача сдана:
```py
import json
import enum
import datetime
import itertools

exam_tickets_variations = list(map(lambda x: f'{x[0]}, {x[1]}',
                                   itertools.combinations(['1. Overview', '2. Built-in types', '3. Control structures'], 2)))
exam_start_time = datetime.time(12, 00, 00)
exam_start_date = datetime.date(2023, 10, 1)


class Subject(enum.Enum):
    python = 1
    project_practicum = 2
    databases = 3
    history = 4
    pe = 5
    php = 6


exam_data = {
    'tickets': exam_tickets_variations,
    'start': str(exam_start_time) + ' ' + str(exam_start_date),
    'subject': Subject.python.value
}

print(json.dumps(exam_data))
```

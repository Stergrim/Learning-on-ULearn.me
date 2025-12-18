# GET

Васе на работе дали очень интересную задачку. Из какого-то непонятного API, для которого нет никакой документации, необходимо собрать все данные о вакансиях по заданному городу. Известно лишь, что на GET-запрос "http://127.0.0.1:8000/vacancies/1" API вернёт следующую информацию:

```
{
  "id": 1,
  "name": "Software Engineer for Windows applications, COM-components development",
  "salary": "",
  "area_name": "Москва",
  "published_at": "2003-10-07T00:00:00+0400"
}
```

Помогите Васеньке написать программу, которая бы вывела все вакансии по заданному городу.

**Формат ввода**

```
Челябинск
```

**Формат вывода**

```
{'id': 42, 'name': 'Руководитель представительства', 'salary': '33640.0', 'area_name': 'Челябинск', 'published_at': '2003-08-28T00:00:00+0400'}
```

|                         |              |
|:------------------------|:-------------|
| **Ограничение времени** | **5 секунд** |
| **Ограничение памяти**  | **256 Мб**   |


Все тесты пройдены, задача сдана:
```py
import requests

area_name = input()
BASE_URL = "http://127.0.0.1:8000/vacancies"
vac_id = 1

while True:
    response = requests.get(f'{BASE_URL}/{vac_id}')
    if "error" in response.text:
        break
    result = response.json()
    if result['area_name'] == area_name:
        print(result)
    vac_id += 1
```

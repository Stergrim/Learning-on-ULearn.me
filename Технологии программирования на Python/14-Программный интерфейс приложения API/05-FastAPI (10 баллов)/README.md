# FastAPI

У Васи получилось выудить данные из центробанка и наложить их на данные о вакансиях. Но начальству и этого мало! Теперь от Васи хотят собственное API, которое позволяет взаимодействовать с полученными ранее данными.

Необходимо написать на [FastAPI](https://devdocs.io/fastapi/) API, к которому можно делать GET, POST, DELETE запросы.

Модель вакансии состоит из следующих полей:
1. name - Название вакансии.
2. salary - Зарплата.
3. area_name - Место, в котором размещается вакансия.

**GET**: Так, GET-запрос должен по /vacancies/{vacancy_id} возвращать примерно следующие данные (пример для vacancy_id = 1):

```
{
  "id": 1,
  "name": "Software Engineer for Windows applications, COM-components development",
  "salary": "",
  "area_name": "Москва",
  "published_at": "2003-10-07T00:00:00+0400"
}
```

**POST**: POST-запрос принимает следующие данные:

```
vacancy = {
    "name": "Имя вакансии",
    "salary": "Зарплата",
    "area_name": "Место, в котором размещается вакансия"
}
```

и по /vacancies создает следующую вакансию:

```
{
  "id": 103,
  "name": "Имя вакансии",
  "salary": "Зарплата",
  "area_name": "Место, в котором размещается вакансия",
  "published_at": "2023-07-23T21:31:51"
}
```

*Значение поля "published_at" не проверяется*

Пользователю, который сделал post-запрос, должно вернуться:

```
{
  "message": "vacancy posted successfully"
}
```

**DELETE**: DELETE-запрос должен по /vacancies/{vacancy_id} возвращать:

```
{
  "message": "vacancy deleted successfully"
}
```

Если вакансия не найдена, то возвращайте:

```
{
  "error": "vacancy not found"
}
```

Ранее вы создавали [базу данных](vacancies.zip) из CSV-таблицы, воспользуйтесь ей.

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **10 секунд** |
| **Ограничение памяти**  | **512 Мб**    |


Все тесты пройдены, задача сдана:
```py
import sqlite3
from fastapi import FastAPI

app = FastAPI()

@app.get("/vacancies/{vacancy_id}")
def read_data(vacancy_id: int):
    conn = sqlite3.connect('vacancies.db')
    cursor = conn.execute("SELECT * FROM vacancies WHERE id = ?", (vacancy_id,))
    result = cursor.fetchone()
    conn.close()
    
    if result is None:
        return {'error': 'vacancy not found'}
    
    return {
        'id': result[0],
        'name': result[1],
        'salary': result[2],
        'area_name': result[3],
        'published_at': result[4]
    }

@app.post("/vacancies")
def write_data(vac: dict):
    conn = sqlite3.connect('vacancies.db')
    
    cursor = conn.execute("SELECT MAX(id) FROM vacancies")
    max_id = cursor.fetchone()[0]
    new_id = (max_id or 0) + 1
    
    query = """
        INSERT INTO vacancies (id, name, salary, area_name, published_at)
        VALUES (?, ?, ?, ?, '2023-07-23T21:31:51')
    """
    conn.execute(query, (new_id, vac['name'], vac['salary'], vac['area_name']))
    conn.commit()
    conn.close()
    return {"message": "vacancy posted successfully"}

@app.delete("/vacancies/{vacancy_id}")
def delete_data(vacancy_id: int):
    conn = sqlite3.connect('vacancies.db')
    query = f"DELETE FROM vacancies WHERE id = {vacancy_id}"
    cursor = conn.execute(query)
    conn.commit()
    conn.close()
    if cursor.rowcount > 0:
        return {"message": "vacancy deleted successfully"}
    return {"error": "vacancy not found"}
```

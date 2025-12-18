# Вакансии в БД

Пора была приступать к сохранению вакансий в базе данных.

Вася написал ТЗ на вторую часть работ:
1. Необходимо считать [данные о вакансиях](vacancies.zip) из csv, выполнить предобработку (преобразовать валюту в рубли) заработной платы используя таблицу с курсами валют, находящуюся в БД из прошлой задачи, и найти её среднее значение.
2. При выполнении предобработки необходимо извлекать значение коэффициента из БД, при помощи SQL запроса.
3. Необходимо соблюдать, чтобы тип для хранения данных в SQLite совпадал со значением записанным в ячейку.
4. При расчете средней зарплаты необходимо оставить только целую часть результата.

Обратите внимание, что значения полей salary_from и salary_to могут быть равны NULL. Если оба значения полей - NULL, значение итогового поля также должно быть равно NULL,

На вход подаются: название базы данных, название csv-файла с вакансиями, название таблицы в которую нужно перенести данные и название таблицы с курсами валют.

**Формат ввода**

```
example_database
example_vacancies.csv
answer_table
currency_table
```

**Формат таблицы с курсами валют**

| **date**    | **BYR**       | **USD**     | **EUR**     | **KZT**      | **UAH**     | **AZN**     | **KGS**      | **UZS**       |
|:------------|:--------------|:------------|:------------|:-------------|:------------|:------------|:-------------|:--------------|
| **2010-08** | **0.0101434** | **30.1869** | **39.4694** | **0.204352** | **3.82316** | **37.5646** | **0.647093** | **0.0188016** |
| **2010-09** | **0.0102446** | **30.8669** | **39.0127** | **0.209487** | **3.91364** | **38.4108** | **0.66167**  | **0.0191362** |

**Формат итоговой таблицы**

| **name**                              | **salary** | **area_name**       | **published_at**              |
|:--------------------------------------|:-----------|:--------------------|:------------------------------|
| **Руководитель группы разработчиков** | **90000**  | **Санкт-Петербург** | **2022-07-17T18:23:06+03:00** |
| **Senior Python Developer (Crypto)**  | **273202** | **Москва**          | **2022-07-05T18:23:15+03:00** |


|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **3 секунды** |
| **Ограничение памяти**  | **512.0 Мб**  |


Все тесты пройдены, задача сдана:
```py
import pandas as pd
import sqlite3

def preproccessing(x):
    salary, currency, date = x["salary"], x["salary_currency"], x["published_at"][:7]
    if salary > 0 and currency is not None:
        query = f"SELECT {currency} FROM {currency_table} WHERE date LIKE '{date}%'"
        try:
            coef = conn.execute(query).fetchall()[0][0]
        except sqlite3.OperationalError:
            coef = 1
        if pd.isna(coef):
            return pd.NA
        return int(salary * coef)

database_name = input()
csv_file = input()
table_name = input()
currency_table = input()

conn = sqlite3.connect(database_name)

df = pd.read_csv(csv_file, encoding="utf-8-sig")
df["salary"] = df[["salary_from", "salary_to"]].mean(axis=1)
df["salary"] = df.apply(preproccessing, axis=1)
df["published_at"] = pd.to_datetime(df["published_at"]).apply(lambda x: pd.to_datetime(x).isoformat())
df = df[["name", "salary", "area_name", "published_at"]]

df.to_sql(table_name, conn, index=False)
conn.close()
```

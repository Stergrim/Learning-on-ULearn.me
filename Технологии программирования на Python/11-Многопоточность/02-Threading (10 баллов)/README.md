# Threading

Сеньор Дон Эладио дал новое имя нашему Васе. Теперь он - Маэстро Оптимизатор. А вместе с новым именем он дал и новую задачу. Есть один замечательный код, который выкачивает данные с кучи разных сайтов. URL адреса лежат в листе `urls`. Суть в том, что этот код выкачивает данные слишком медленно. И по-хорошему надо бы оптимизировать всё это дело.

Вася не знает что такое *requests*, и как с ним работать. Зато он знает что есть такой замечательный инструмент как [threading](https://docs.python.org/3/library/threading.html). Помогите Васе оптимизировать код с помощью [threading](https://docs.python.org/3/library/threading.html).

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **25 секунд** |
| **Ограничение памяти**  | **512.0 Мб**  |


Все тесты пройдены, задача сдана:
```py
import requests
import threading
from bs4 import BeautifulSoup
from concurrent.futures import ThreadPoolExecutor

def get_currencies(url, id, currencies):
    response = requests.get(url)
    if response.status_code == 200:
        secret_code()
        soup = BeautifulSoup(response.text, 'html.parser')
        currency = str(soup.find_all('valute')[id])
        if currency not in currencies:
            currencies.append(currency)

if __name__ == '__main__':
    currencies = []
    id = int(input())

    for url in urls:
        with ThreadPoolExecutor(max_workers=10) as executor:
            executor.submit(get_currencies, url, id, currencies)

    currencies_string = ''.join(currencies)
    print(currencies_string)
```

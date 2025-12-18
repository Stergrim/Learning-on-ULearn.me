# Вакансии

Вася узнал, что есть удобная библиотека для парсинга html-страниц! Теперь ему не надо пользоваться той неудобной библиотекой из задачи "Единый формат"!

Вася решил заново пройтись по всем вакансиям.

Бывают случаи, когда на сайте много рекламы и ненужной информации, хочется просто выделить самое важное. Сейчас вы и потренируетесь это делать.

Вам вместе с Васей предстоит научиться парсить HTML-страницы при помощи библиотеки BeautifulSoup, которая как раз для этого и предназначена. Тренироваться будете на страницах вакансий сайта [HH.ru](https://hh.ru/search/vacancy).

Вам будет дана страница сайта в формате HTML, из неё вам будет необходимо выделить самое важное и поместить в словарь под следующими ключами:

`vacancy`: Название вакансии

`salary`: Зарплата, в шаблоне кода ниже представлены курсы валют, используйте их для перевода в рубли. Если дана вилка, выведите её нижнюю и вернуюю границы, соединенные стрелочкой (`->`)

`experience`: Требуемый опыт работы, оставьте только числа - сколько лет (например, `1-3`)

`company`: Название компании

`description`: Описание вакансии

`skills`: Необходимые навыки, соедените их точкой с запятой с пробелом (например `Python; regex; BeautifulSoup`)

`created_at`: Когда была создана вакансия. Замените все символы NO-BREAK SPACE (\xa0) на обычные пробелы.

Ответ необходимо представить в виде словаря, закодированном в JSON формате.

Рекомендуется самим поизучать структуру страниц вакансий сайта, но если у вас возникли затруднения, вы можете посмотреть подсказки за спойлерами ниже.

**Формат ввода**

```
<div class="####">
    <p class="####">Очень крутой разработчик</p>
    <div class="####">
        <span class="####">30000</span>
        <span class="####">₽</span>
    </div>
    <div data-qa="####">
        <span>от</span>
        <span>3</span>
        <span>до</span>
        <span>6</span>
        <span>лет</span>
    </div>
</div>
<div class="####">
    <p class="####">Очень крутая компания</p>
    <p class="####">Крутости пер., д. 1337</p>
</div>
<div class="####">
    <b>Бла-бла-бла</b>
    <p>бла-бла</p>
    <span>бла-бла-бла</span>
    <strong>БЛА-БЛА</strong>
    <i>бла</i>
    <div>Бла-бла-бла</div>
</div>
<div class="####">
    <p>Python</p>
    <p>HTML</p>
    <p>Django</p>
    <p>Docker</p>
</div>
<div class="####">
    <span>18 авг. 2023</span>
</div>
```

**Формат вывода**

```
{
    "vacancy": "Очень крутой разработчик",
    "salary": "30000.0",
    "experience": "3-6",
    "company": "Очень крутая компания",
    "description": "Бла-бла-бла бла-бла бла-бла-бла БЛА-БЛА бла Бла-бла-бла",
    "skills": "Python; HTML; Django; Docker",
    "created_at": "18 авг. 2023"
}
```

**Подсказки**

`vacancy`: Название вакансии можно найти в теге с классом "vacancy-title".

`salary`: Зарплату можно найти в теге с классом "vacancy-title".

`experience`: Требуемый опыт работы можно найти в теге с классом "vacancy-description-list-item".

`company`:Название компании находится в теге с классом "vacancy-company-name".

`description`: Описание находится в теге с аттрибутом **data-qa** равным "vacancy-description". Чтобы получить весь текст без тегов, можно воспользоваться параметром .text.

`skills`: Ключевые навыки находятся в одном из тегов div с классом "vacancy-section" внутри тега с классом "vacancy-description".

`created_at`: Информация о том, когда была создана вакансия, находится внизу страницы, в теге с классом "vacancy-creation-time-redesigned".

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **1 секунда** |
| **Ограничение памяти**  | **64.0 Мб**   |


Все тесты пройдены, задача сдана:
```py
import json
import bs4
import re

exchange = {
    '₽': 1.0,
    '$': 100.0,
    '€': 105.0,
    '₸': 0.210,
    'Br': 30.0,
}

result = {
    'vacancy': None,
    'salary': None,
    'experience': None,
    'company': None,
    'description': None,
    'skills': None,
    'created_at': None,
}

def parse_vacancy(file_name, result):
    with open(file_name) as f:
        page = bs4.BeautifulSoup(f, "html.parser")

    result["vacancy"] = page.find(attrs={"data-qa": "vacancy-title"}).text
    salary_tag = page.find(attrs={"data-qa": "vacancy-salary"}).text
    salary = re.findall(r"\d{1,3}(?:\s\d{3})*", salary_tag)
    currency = re.findall(r"[₽$€₸]|Br", salary_tag)[0]
    result["salary"] = "->".join(str(float(s.replace("\xa0", "")) * exchange[currency]) for s in salary)

    experience_tag = page.find(attrs={"data-qa": "vacancy-experience"}).text
    experience_years = [e for e in experience_tag if e.isdigit()]
    if experience_years:
        result["experience"] = "-".join(experience_years)

    result["company"] = page.find(attrs={"data-qa": "vacancy-company-name"}).text
    result["description"] = page.find(attrs={"data-qa": "vacancy-description"}).text
    skills_tag = page.find_all(attrs={"data-qa": "bloko-tag bloko-tag_inline skills-element"})
    result["skills"] = "; ".join(s.text for s in skills_tag)
    result["created_at"] = page.find(class_="vacancy-creation-time-redesigned").find("span").text.replace("\xa0", " ")

    print(json.dumps(result))

html = input()
parse_vacancy(html, result)
```

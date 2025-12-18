# Urls в Django

Скачайте [проект](exercise.zip).

Ребята решили организовать свой бизнес по разработке приложений в области анализа данных. Для этого они начали разработку собственного веб проекта на Django. Ребята уже продумали и сверстали несколько html-страниц. Кроме того, они продумали план развития приложения:
1. Продумать маршруты приложения.
2. Создать маршрутизацию приложения в файле `urls.py`.
3. Создать модели в файле `models.py` для работы с базой данных.
4. Реализовать функции в файле `views.py`, которые будут вызываться при переходе пользователя по конкретным маршрутам.

Ребята уже создали болванки для того, чтобы приложение запускалось без ошибок: пустые модели и функции с заготовленными данными. Кроме того, продумали всю маршрутизацию приложения:
- `hello/` - система должна приветствовать пользователя и выводить его имя (функция hello)
- `vacancies/` - вывод всех данных о вакансиях из базы данных (функция all_vacancies)
- `vacancies/filter/` - вывод данных о вакансиях из базы данных с фильтрацией (функция filter_vacancies)
- `vacancies/dynamic/salary-year/` - вывод динамики уровня зарплат по годам (функция get_salary_year_dynamic)
- `vacancies/dynamic/count-year/` - вывод динамики количества вакансий по годам (функция get_count_year_dynamic)
- `vacancies/statistic/top10-salary-city/` - вывод топ 10 зарплат по городам (функция get_top_10_salary_city)
- `vacancies/statistic/top10-vac-city/` - вывод доли вакансий по городам (топ 10) (функция get_top_10_vac_city)

Все функции находятся в файле views.py.

Реализуйте маршрутизацию в заранее заготовленной переменной urlpatterns.

Все тесты пройдены, задача сдана:
```py
from django.contrib import admin
from django.urls import path
from . import views

urlpatterns = [
    path('hello/', views.hello),
    path('vacancies/', views.all_vacancies),
    path('vacancies/filter/', views.filter_vacancies),
    path('vacancies/dynamic/salary-year/', views.get_salary_year_dynamic),
    path('vacancies/dynamic/count-year/', views.get_count_year_dynamic),
    path('vacancies/statistic/top10-salary-city/', views.get_top_10_salary_city),
    path('vacancies/statistic/top10-vac-city/', views.get_top_10_vac_city),
]
```

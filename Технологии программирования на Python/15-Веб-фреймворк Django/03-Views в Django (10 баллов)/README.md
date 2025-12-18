# Views в Django

Продолжайте в том же проекте.

Пришло время реализовать основную логику нашего приложения!

Используя Django ORM и созданные вами в прошлой задаче модели реализуйте следующие функции в файле views.py:
1. `hello`:
   - если запрос типа GET: вернуть страницу hello.html с с параметром name равным "пользователь"
   - если запрос типа POST: получить из запроса значение параметра "id", используя модель SiteUser найти пользователя с данным id и получить его имя через функцию get_name, вернуть страницу hello.html с именем пользователя в параметре name.
2. `all_vacancies`: вернуть страницу vacancies_table.html, заполненную данными, полученными из базы данных через модель Vacancy. Данные необходимо передать в параметр "data".
3. `filter_vacancies`: вернуть страницу vacancies_table.html, заполненную данными, передав их в параметр "data", получив их через модель Vacancy, с учетом следующих возможных фильтров, передаваемых в GET запросе:
   - name_start - начало названия вакансии
   - salary - зарплата
   - city_start - начало названия города
4. `get_salary_year_dynamic`: вернуть страницу dynamics_table.html со следующими параметрами:
   - first_parameter и second_parameter оставьте неизменными ("Avg salary" и "Year" соответственно)
   - data: динамика уровня зарплат по годам, где значения зарплаты необходимо представить под псевдонимом "first", а годов под псевдонимом "second"
   
   При подсчете вакансий необходимо учитывать только те вакансии, у которых указана зарплата (т.е. не равна NULL).
5. `get_count_year_dynamic` - аналогично предыдущему пункту вернуть динамику количества ванкансий по годам. "first" - количество вакансий, "second" - год. При подсчете вакансий необходимо учитывать только те вакансии, у которых указана зарплата (т.е. не равна NULL).
6. `get_top_10_salary_city` - вернуть страницу dynamics_table.html аналогично последним двум пунктам. В параметре "data" передайте уровень зарплат по городам в порядке неубывания и только 10 первых значений. "first" - зарплата, "second" - город. Необходимо выводить только те города, у которых процент вакансий больше 1. При подсчете вакансий необходимо учитывать только те вакансии, у которых указана зарплата (т.е. не равна NULL).
7. `get_top_10_vac_city` - аналогично предыдущему пункту передать через параметр "data" долю вакансий по городам в убывающем порядке, первые 10 значений. При подсчете вакансий необходимо учитывать только те вакансии, у которых указана зарплата (т.е. не равна NULL).

Все тесты пройдены, задача сдана:
```py
from django.shortcuts import render
from django.views.decorators.csrf import csrf_exempt
from django.db.models.functions import Substr, Cast
from django.db.models import IntegerField, FloatField, Count, Avg
from .models import *

@csrf_exempt
def hello(request):
    username = 'пользователь'
    if request.method == "POST":
        id = request.POST.get('id')
        user = SiteUser.objects.get(id=id)
        username = user.get_name()
    return render(request, 'hello.html', {'name': username})

def all_vacancies(request):
    data = Vacancy.objects.all()
    return render(request, 'vacancies_table.html', {'data': data})

def filter_vacancies(request):
    data = Vacancy.objects.filter()
    name_start = request.GET.get('name_start', '')
    salary = request.GET.get('salary', '')
    city_start = request.GET.get('city_start', '')
    if name_start:
        data = data.filter(name__startswith=name_start)
    if salary:
        data = data.filter(salary__gte=int(salary))
    if city_start:
        data = data.filter(area_name__startswith=city_start)
    return render(request, 'vacancies_table.html', {'data': data})

def get_salary_year_dynamic(request):
    dynamic_data = Vacancy.objects.exclude(salary=None).annotate(
        year=Cast(Substr('published_at', 1, 4), IntegerField())
    ).values('year').annotate(
        avg_salary=Avg('salary')
    ).order_by('year')

    data = [{'first': str(round(item['avg_salary'], 1)), 'second': str(item['year'])}
            for item in dynamic_data]

    return render(request, 'dynamics_table.html',
                  {'first_parameter': 'Avg salary', 'second_parameter': 'Year', 'data': data})

def get_count_year_dynamic(request):
    dynamic_data = Vacancy.objects.exclude(salary=None).annotate(
        year=Cast(Substr('published_at', 1, 4), IntegerField())
    ).values('year').annotate(
        count=Count('id')
    ).order_by('year')

    data = [{'first': str(item['count']), 'second': str(item['year'])}
            for item in dynamic_data]

    return render(request, 'dynamics_table.html',
                  {'first_parameter': 'Vacancies count', 'second_parameter': 'Year', 'data': data})

def get_top_10_salary_city(request):
    total_vacancies = Vacancy.objects.exclude(salary=None).count()

    if total_vacancies == 0:
        return render(request, 'dynamics_table.html',
                      {'first_parameter': 'Avg salary', 'second_parameter': 'City', 'data': []})

    city_stats = Vacancy.objects.exclude(salary=None).exclude(area_name=None).values('area_name').annotate(
        avg_salary=Avg('salary'),
        city_count=Count('id'),
        percentage=Cast(Count('id') * 100.0 / total_vacancies, FloatField())
    ).filter(
        percentage__gt=1.0
    ).order_by('avg_salary')

    top_10 = city_stats[:10]
    data = [{'first': str(round(item['avg_salary'], 1)), 'second': str(item['area_name'])}
            for item in top_10]

    return render(request, 'dynamics_table.html',
                  {'first_parameter': 'Avg salary', 'second_parameter': 'City', 'data': data})

def get_top_10_vac_city(request):
    total_vacancies = Vacancy.objects.exclude(salary=None).count()

    if total_vacancies == 0:
        return render(request, 'dynamics_table.html',
                      {'first_parameter': 'Vacancy rate', 'second_parameter': 'City', 'data': []})

    city_stats = Vacancy.objects.exclude(salary=None).exclude(area_name=None).values('area_name').annotate(
        city_count=Count('id'),
        percentage=Cast(Count('id'), FloatField()) / Cast(total_vacancies, FloatField())
    ).order_by('-percentage')

    top_10 = city_stats[:10]
    data = [{'first': str(round(item['percentage'], 4)), 'second': str(item['area_name'])}
            for item in top_10]

    return render(request, 'dynamics_table.html',
                  {'first_parameter': 'Vacancy rate', 'second_parameter': 'City', 'data': data})
```

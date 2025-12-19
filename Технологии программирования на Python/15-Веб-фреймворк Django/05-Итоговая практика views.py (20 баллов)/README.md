# Итоговая практика: views.py

Продолжайте в том же [проекте](task.zip).

В этой задаче вам предстоит реализовать бизнес-логику этого проекта. Работайте в файле djangotask/views.py.

Вам необходимо реализовать методы, работающие с моделями:
- `add_user`: Создать нового пользователя (класс MyUser). Принимает POST-запрос со следующими полями:
  - first_name: Имя
  - last_name: Фамилия
  - age: Возраст
  - email: Электронная почта
  - password: Пароль.

    Возраст должен проверяться статическим методом verify_age класса MyUser: если метод возвращает False, должна подниматься ошибка со следующим текстом: `Age must be greater than 18 and lower than 122!`.

    Пароль перед созданием должен захэшироваться статическим методом hash_password.

    Ответ представьте в виде рендера страницы answer.html, передав в качестве переменной answer в неё id созданного пользователя.
- `delete_user`: Удалить пользователя. Принимает POST-запрос со следующими полями:
  - email: Электронная почта пользователя
  - password: Пароль пользователя.

    Пароль должен быть валидирован функцией verify_password, если значение функции False - вернуть ошибку с текстом `Wrong password`

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer True, если пользователь был успешно удалён.
- `authorise`: Авторизация пользователя. Принимает POST-запрос со следующими полями:
  - email: Электронная почта пользователя
  - password: Пароль пользователя.

    Пароль должен быть валидирован функцией verify_password, если значение функции False - вернуть ошибку с текстом `Wrong password`

    Ответ представьте в виде страницы user_info.html, передав в качестве переменной user полученный экземпляр класса MyUser, и список навыков (свойство skills класса MyUser) в качестве перменной skills.
- `add_vacancy`: Создать новую вакансию (модель Vacancy). Принимает POST-запрос со следующими полями:
  - name: Название вакансии
  - salary: Зарплата
  - area_name: Город.

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer id созданной вакансии.
- `get_vacancy`: Получение вакансии. Принимает GET-запрос со следующими параметрами:
  - id: Идентификатор вакансии.

    Ответ представьте в виде страницы vacancy.html, передав в неё следующие параметры:
    - vacancy: Полученный экземпляр модели Vacancy
    - skills: Список навыков, необходимых для устройства на данную вакансию (используйте свойство skills модели)
    - responses: QuerySet откликов на данную вакансию (используйте модель UserResponse).
- `delete_vacancy`: Удаление вакансии. Принимает POST-запрос со следующими параметрами:
  - id: Идентификатор вакансии.

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer True, если вакансия была успешно удалёна.
- `add_skill`: Добавление нового навыка (модель Skill). Принимает POST-запрос со следующими параметрами:
  - name: Название навыка.

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer id созданного навыка.
- `get_skill`: Получение навыка. Принимает POST-запрос со следующими параметрами:
  - id: Идентификатор навыка.

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer название полученного навыка.
- `delete_skill`: Удаление навыка. Принимает POST-запрос со следующими параметрами:
  - id: Идентификатор навыка.

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer True, если навык был успешно удалён.
- `get_all_skills`: Получение всех навыков.

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer строку, в которой перечисляются существующие скиллы. Например: "скилл 1, скилл 2, скилл 3".
- `add_skill_to_vacancy`: Привяка навыка к вакансии (модель VacancySkill). Принимает POST-запрос со следующими параметрами:
  - vacancy: Идентификатор вакансии
  - skill: Идентификатор навыка.

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer True, если навык был успешно добавлен к вакансии.
- `add_skill_to_user`: Добавление навыка пользователю (модель UserSkill). Принимает POST-запрос со следующими параметрами:
  - user: Идентификатор пользователя
  - skill: Идентификатор навыка.

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer True, если навык был успешно добавлен пользователю.
- `remove_skill_from_user`: Удаление навыка у пользователя. Принимает POST-запрос со следующими параметрами:
  - user: Идентификатор пользователя
  - skill: Идентификатор навыка.

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer True, если навык был успешно удалён у пользователя.
- `remove_skill_from_vacancy`: Удаление требуемого навыка из вакансии. Принимает POST-запрос со следующими параметрами:
  - vacancy: Идентификатор вакансии
  - skill: Идентификатор навыка.

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer True, если навык был успешно удалён.
- `add_response`: Добавление отклика к вакансии (модель UserResponse). Принимает POST-запрос со следующими параметрами:
  - user: Идентификатор пользователя
  - vacancy: Идентификатор вакансии
  - message: Сопроводительное письмо к отклику.

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer id созданного отклика.
- `get_response`: Получение отклика. Принимает GET-запрос со следующими параметрами:
  - id: Идентификатор отклика.

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer сопроводительное письмо отклика.
- `delete_response`: Удаление отклика. Принимает POST-запрос со следующими параметрами:
  - id: Идентификатор отклика.

    Ответ представьте в виде страницы answer.html, передав в качестве переменной answer True, если отклик был успешно удалён.

**Формат вывода**

Формат вывода для каждого метода представлен в описании к нему. Если во время выполнения метода возникает ошибка, её необходимо вернуть в виде страницы error.html, передав в качестве параметра error тестовое представление ошибки.

К каждому методу добавьте декоратор `@csrf_exempt`, так как проверка данного задания проводится с помощью функций python, а не из браузера.

Все тесты пройдены, задача сдана:
```py
from django.shortcuts import render
from django.views.decorators.csrf import csrf_exempt
from .models import *

@csrf_exempt
def add_user(request):
    try:
        data = request.POST
        first_name = data.get('first_name')
        last_name = data.get('last_name')
        age = data.get('age')
        email = data.get('email')
        password = data.get('password')
        user = MyUser()
        user.first_name = first_name
        user.last_name = last_name
        if MyUser.verify_age(age):
            user.age = int(age)
        else:
            return render(request, 'error.html', {'error': 'Age must be greater than 18 and lower than 150!'})

        user.email = email
        user.password = MyUser.hash_password(password)
        user.save()
        saved_user = MyUser.objects.get(email=email)
        return render(request, 'answer.html', {'answer': saved_user.id})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def delete_user(request):
    try:
        data = request.POST
        email = data.get('email')
        password = data.get('password')
        user = MyUser.objects.get(email=email)
        if user.verify_password(password):
            user.delete()
            return render(request, 'answer.html', {'answer': True})
        else:
            return render(
                request,
                'error.html',
                {'error': 'Wrong password'})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def authorise(request):
    try:
        data = request.POST
        email = data.get('email')
        password = data.get('password')
        user = MyUser.objects.get(email=email)
        if user.verify_password(password):
            return render(request, 'user_info.html', {'user': user, 'skills': user.skills})
        else:
            return render(
                request,
                'error.html',
                {'error': 'Wrong password'})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def add_vacancy(request):
    try:
        data = request.POST
        name = data.get('name')
        salary = int(data.get('salary'))
        area_name = data.get('area_name')
        vacancy = Vacancy()
        vacancy.name = name
        vacancy.salary = salary
        vacancy.area_name = area_name
        vacancy.save()
        saved_vacancy = Vacancy.objects.get(name=name, salary=salary, area_name=area_name)
        return render(request, 'answer.html', {'answer': saved_vacancy.id})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def get_vacancy(request):
    try:
        vacancy_id = request.GET.get('id')
        vacancy = Vacancy.objects.get(id=vacancy_id)
        return render(request, 'vacancy.html', {
            'vacancy': vacancy,
            'skills': vacancy.skills,
            'responses': UserResponse.objects.filter(vacancy=vacancy)
        })
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def delete_vacancy(request):
    try:
        data = request.POST
        vacancy_id = data.get('id')
        vacancy = Vacancy.objects.get(id=vacancy_id)
        vacancy.delete()
        return render(request, 'answer.html', {'answer': True})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def add_skill(request):
    try:
        data = request.POST
        name = data.get('name')
        skill = Skill()
        skill.name = name
        skill.save()
        saved_skill = Skill.objects.get(name=name)
        return render(request, 'answer.html', {'answer': saved_skill.id})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def get_skill(request):
    try:
        skill_id = request.GET.get('id')
        skill = Skill.objects.get(id=skill_id)
        return render(request, 'answer.html', {'answer': skill.id})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def delete_skill(request):
    try:
        data = request.POST
        skill_id = data.get('id')
        skill = Skill.objects.get(id=skill_id)
        skill.delete()
        return render(request, 'answer.html', {'answer': True})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def get_all_skills(request):
    try:
        all_skills = Skill.objects.all()
        skills_list = [skill.name for skill in all_skills]
        return render(request, 'answer.html', {'answer': ', '.join(skills_list)})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def add_skill_to_vacancy(request):
    try:
        data = request.POST
        vacancy_id = data.get('vacancy')
        skill_id = data.get('skill')
        vacancy = Vacancy.objects.get(id=vacancy_id)
        skill = Skill.objects.get(id=skill_id)
        vacancy_skill = VacancySkill()
        vacancy_skill.vacancy = vacancy
        vacancy_skill.skill = skill
        vacancy_skill.save()
        return render(request, 'answer.html', {'answer': True})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def remove_skill_from_vacancy(request):
    try:
        data = request.POST
        vacancy_id = data.get('vacancy')
        skill_id = data.get('skill')
        vacancy = Vacancy.objects.get(id=vacancy_id)
        skill = Skill.objects.get(id=skill_id)
        vacancy_skill = VacancySkill.objects.get(vacancy=vacancy, skill=skill)
        vacancy_skill.delete()
        return render(request, 'answer.html', {'answer': True})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def add_skill_to_user(request):
    try:
        data = request.POST
        user_id = data.get('user')
        skill_id = data.get('skill')
        user = MyUser.objects.get(id=user_id)
        skill = Skill.objects.get(id=skill_id)
        user_skill = UserSkill()
        user_skill.user = user
        user_skill.skill = skill
        user_skill.save()
        user_skill.save()
        return render(request, 'answer.html', {'answer': True})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def remove_skill_from_user(request):
    try:
        data = request.POST
        user_id = data.get('user')
        skill_id = data.get('skill')
        user = MyUser.objects.get(id=user_id)
        skill = Skill.objects.get(id=skill_id)
        user_skill = UserSkill.objects.get(user=user, skill=skill)
        user_skill.delete()
        return render(request, 'answer.html', {'answer': True})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def add_response(request):
    try:
        data = request.POST
        user_id = data.get('user')
        vacancy_id = data.get('vacancy')
        message = data.get('message')
        user = MyUser.objects.get(id=user_id)
        vacancy = Vacancy.objects.get(id=vacancy_id)
        user_response = UserResponse()
        user_response.user = user
        user_response.vacancy = vacancy
        user_response.message = message
        user_response.save()
        saved_user_response = UserResponse.objects.get(user=user, vacancy=vacancy, message=message)
        return render(request, 'answer.html', {'answer': saved_user_response.id})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def get_response(request):
    try:
        data = request.POST
        user_response_id = data.get('id')
        user_response = UserResponse.objects.get(id=user_response_id)
        return render(request, 'answer.html', {'answer': user_response.message})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})

@csrf_exempt
def delete_response(request):
    try:
        data = request.POST
        user_response_id = data.get('id')
        user_response = UserResponse.objects.get(id=user_response_id)
        user_response.delete()
        return render(request, 'answer.html', {'answer': True})
    except Exception as e:
        return render(request, 'error.html', {'error': str(e)})
```

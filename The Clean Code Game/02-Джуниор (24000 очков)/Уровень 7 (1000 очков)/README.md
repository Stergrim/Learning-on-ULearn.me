# Уровень 7

Автору будет приятно, если вы поделитесь ссылкой на эту игру с коллегами.<br>
Заранее спасибо! :-)

Исходный код:
```cs
if (you.Like(this.Game))
{
    you.Tweet();
    you.Post();
    you.Share();
}
else
{
    you.H4Te_AUth0R();
}
```

Исправленный код:
```cs
if (you.Like(this.Game))
{
    you.Tweet();
    you.Post();
    you.Share();
}
else
{
    you.EmailAuthor("pe@kontur.ru");
}
```

Объяснения:
- Ненависть — плохое чувство! :-)

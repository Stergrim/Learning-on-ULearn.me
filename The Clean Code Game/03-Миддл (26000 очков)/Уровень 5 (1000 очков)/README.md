# Уровень 5

Но не спешите удалять все комментарии из вашего кода. Бывают и полезные!

Исходный код:
```cs
//format matched: hh:mm:ss, MMM dd, yyyy
private Regex timeRegex = new Regex(@"\d*:\d*:\d*, \w* \d*, \d*");

Responder GetResponder(); //Returns the Responder being tested.
```

Исправленный код:
```cs
//format matched: hh:mm:ss, MMM dd, yyyy
private Regex timeRegex = new Regex(@"\d*:\d*:\d*, \w* \d*, \d*");

Responder GetTestResponder();
```

Объяснения:
- Если появляется желание написать поясняющий комментарий к методу, стоит вместо этого постараться придумать более удачное имя методу.

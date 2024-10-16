# Quiz

1. Стеганография (1 из 1 балла)
   * ❌ **позволяет передавать только те секретные сообщения, которые похожи на что-то обычное — картинки, аудио-файлы, текстовые документы**
   * ✅ **скрывает факт наличия секретной информации** (Правильно!)
   * ✅ **использует нечувствительные для невооружённого человека изменения контейнера** (Правильно!)
   * ❌ **не совместима с криптографией**


2. Следующие способы текстовой компьютерной стеганографии оставляют контейнер неотличимым для человеческого глаза во всех или в большинстве случаев: (1 из 1 балла)
   * ❌ **удаление некоторых букв из текста**
   * ✅ **замена некоторых русских букв о на латинские o** (Правильно!)
   * ✅ **замена некоторых переводов строк в стиле Windows (‘\r\n’) на переводы строк в стиле Unix (‘\n’): https://ru.wikipedia.org/wiki/%D0%9F%D0%B5%D1%80%D0%B5%D0%B2%D0%BE%D0%B4_%D1%81%D1%82%D1%80%D0%BE%D0%BA%D0%B8#.D0.92_ASCII** (Правильно!)
   * ❌ **замена некоторых прописных букв на строчные**


3. Мы прячем секретное сообщение в изображение, сохранённое в формате PNG, в самых младших битах красного и синего каналов в изображении. Изображение имеет размер 200x200 пикселей. Сколько байт информации мы сможем спрятать? (1 из 1 балла)
   * 🔴 **80 000**
   * 🟢 **10 000** (Правильно!)
   * 🔴 **15 000**
   * 🔴 **5 000**


4. Секретное сообщение с помощью стеганографии можно спрятать в контейнеры в следующих форматах: (1 из 1 балла)
   * ✅ **WAV** (Правильно!)
   * ✅ **JPEG** (Правильно!)
   * ✅ **PNG** (Правильно!)
   * ✅ **BMP** (Правильно!)
   * ✅ **TIFF** (Правильно!)


5. При частоте дискретизации 44 100 Гц (используется, например, в аудио-дисках) и изменении амплитудных значений максимум на единицу для передачи мегабайта текста потребуется запись длины (1 из 1 балла)
   * 🔴 **около 95 сек**
   * 🔴 **около 24 сек**
   * 🟢 **около 190 сек** (Правильно!)
   * 🔴 **около 48 сек**


6. Следующие утверждения про цифровые водяные знаки верны: (1 из 1 балла)
   * ❌ **цифровой водяной знак всегда невидим (неслышим) для человека**
   * ✅ **цифровые водяные знаки невозможно снять без повреждения контейнера** (Правильно!)
   * ✅ **цифровые водяные знаки используется для защиты авторских прав** (Правильно!)
   * ❌ **при малейшем искажении контейнера цифровой водяной знак становится невозможно прочитать**


7. Пусть a — число от 0 до 255. Тогда присваивание a = a & 7 делает следующее: (1 из 1 балла)
   * 🔴 **обнуляет последние три бита числа a**
   * 🟢 **оставляет только последние три бита, все остальные биты обнуляет** (Правильно!)
   * 🔴 **изменяет на противоположные последние три бита числа a: нули на единицы, а единицы на нули**
   * 🔴 **обнуляет последние семь бит числа a**

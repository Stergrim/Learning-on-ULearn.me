# Простые случаи

Опытный инженер умеет оценивать сложность таких примеров в уме, ничего на самом деле не вычисляя. Этот навык как раз и превращает оценку сложности в быстрый и дешёвый процесс.

Попробуйте и вы свои силы.

1. Сопоставьте коду оценку сложности. (1 из 1 балла)

| ❔    | ✅              |
|------|----------------|
| for (int i=0; i*i<n; i++) z++;   | **Θ(sqrt(n))** |
| for (int i=0; i<n*n; i++) z++;   | **Θ(n²)**      |
| for (int i=1; i%7!=0; i++) z++;   | **Θ(1)**       |
| for (int i=1; i<n*n; i*=10) z++;   | **Θ(log n)**   |

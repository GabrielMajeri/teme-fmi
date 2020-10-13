# Laborator 2 DAW

[Laboratorul 2](https://cs.unibuc.ro/~asipos/daw/Laborator-2.pdf), implementat în ASP.NET Core. Exercițiile se bazează pe conceptele de [routing](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/routing?view=aspnetcore-3.1) și [views](https://docs.microsoft.com/en-us/aspnet/core/mvc/views/overview?view=aspnetcore-3.1).

Toate rutele sunt implementate în controller-ul [Home](Controllers/HomeController.cs). View-urile asociate se află în [Views/Home](Views/Home).

## Rulare

Rulați
```sh
dotnet run
```
Server-ul este disponibil la adresa `http://localhost:5000`

## Exerciții

1. Primește doi parametrii în URL, și trebuie să vadă dacă primul îl conține pe al doilea.

   **Exemplu**: `/Route1/ana/a`: afișează că șirul `ana` conține subșirul `a`
2. La fel ca 1, dar al doilea parametru e opțional. Dacă nu-l primește, afișează un mesaj de eroare.

   **Exemplu**: `/Route2/ana/`
3. Acceptă doar numere pare, de 3-7 cifre.

   **Exemplu**: `/Route3/1234`

4. Primește o listă de numere, și folosind un view parțial, afișează pentru fiecare dacă sunt sau nu divizibile cu 7.

   **Exemplu**: `/Route4?values=3&values=7&values=14&values=5`

5. Acceptă doar șirurile de forma `a^n b^n` cu `n > 0`.

   **Exemplu**: `/Route5/aabb`
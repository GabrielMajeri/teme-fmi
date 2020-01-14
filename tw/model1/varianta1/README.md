# Model Examen TW Varianta 1

## Cerință

Se considera un document HTML fara continut.

1. Scrieti cod CSS astfel incat :

   - In cazul in care pagina are latimea mai mare de 900px, divul cu id-ul "parinte" sa aiba ambele dimensiuni de 800px si un border negru de 1px. Clasa "celula" va avea culoarea de background roz. Divurile din clasa "celula" vor avea alternativ border rosu respectiv mov (primul div are border rosu, urmatorul div are border mov, etc ) de 1px.

   - Daca se ajunge cu mouse-ul pe un div din clasa "celula", acesta isi va schimba culoarea de background, lent (timp de 1secunda ) la culoarea albastra. Daca pagina are latimea mai mica de 900px, elementele cu clasa "celula" dispar din document fara a li se pastra spatiul, iar divul cu id-ul "parinte" va avea latimea egala cu latimea paginii, fara sa mai aiba border ci doar culoare de background verde.

2. Scrieti cod JavaScript pentru a realiza urmatoarele cerinte:

   - La incarcarea paginii se va crea dinamic un div cu id-ul "parinte" si un grup de butoane radio avand atasate etichete cu valori de la 4 la 8, initial fiind selectat butonul din mijloc.

   - La selectarea unui buton radio, se pastreaza in N valoarea etichetei atasate lui iar grupul de butoane radio se sterge din arborele documentului. Valoarea lui N va fi pastrata in localStorage iar la reincarcarea paginii va fi selectat butonul radio cu eticheta N.

   - La click pe div, in interiorul acestuia, dupa 5 secunde se vor genera N x N divuri copil de forma patrata, avand latimi egale si clasa "celula" si fiind dispuse pe N linii si N coloane. La dublu-click pe un div copil din clasa "celula", generat anterior, se va afisa in divul copil perechea (i,j) reprezentand linia si coloana pe care e pozitionat divul copil in cadrul parintelui (i, j incep de la 0). De asemenea la finalul lui body se vor scrie coordonatele dublu-clickului in document. Atentie, dublul-click pe un div copil nu trebuie sa genereze click si asupra divului parinte.

   - Doar la prima apasare a tastei "c" toate divurile copil vor primi pe rand (circular), la fiecare 3 secunde o culoare de background aleatoare, pana cand se apasa tasta "s", ceea ce va duce la revenirea la culoarea de background initiala (cea setata din CSS) a tuturor divurilor copil. Urmatoarele apasari de taste nu vor mai avea efect.

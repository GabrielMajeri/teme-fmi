#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIN(a, b) (((a) < (b)) ? (a) : (b))

typedef struct {
    int id;
    int cumparate;
    int efectuate;
} Client;

int main() {
    FILE* fin = fopen("roata.in", "r");

    int n;
    fscanf(fin, "%d", &n);

    Client* roata = (Client*)calloc(n, sizeof(Client));

    int p;
    fscanf(fin, "%d", &p);

    Client* clienti = (Client*)calloc(p, sizeof(Client));
    int nr_cl = 0;

    int suma = 0;

    for (int i = 0; i < p; ++i) {
        int rotiri;
        fscanf(fin, "%d", &rotiri);

        clienti[nr_cl++] = (Client){
            .id = i + 1,
            .cumparate = rotiri,
            .efectuate = 0,
        };

        // 1 rotire = 1 euro
        suma += rotiri;
    }

    fclose(fin);
    FILE* fout = fopen("roata.out", "w");

    // urmatorul client care trebuie pus in roata
    int k = 0;

    // ultima cabina din care a iesit un client
    int ultima = 0;

    // pun toti clientii initiali
    for (int i = 0; i < MIN(p, n); ++i) {
        roata[i] = clienti[k++];
    }

    fprintf(fout, "%d\n", suma);

    bool mai_sunt_clienti = true;
    while (mai_sunt_clienti) {
        // presupunem ca am terminat deja
        mai_sunt_clienti = false;

        // simulez rotirea pentru toti clientii
        for (int i = 0; i < n; ++i) {
            if (roata[i].id == 0) {
                continue;
            }

            mai_sunt_clienti = true;

            roata[i].efectuate += 1;

            if (roata[i].efectuate >= roata[i].cumparate) {
                fprintf(fout, "%d ", roata[i].id);

                if (k < nr_cl) {
                    // incercam sa punem urmatorul client
                    roata[i] = clienti[k++];
                } else {
                    // lasam locul liber
                    memset(roata + i, 0, sizeof(Client));
                }

                ultima = i + 1;
            }
        }
    }

    fprintf(fout, "\n%d\n", ultima);

    fclose(fout);

    free(clienti);
    free(roata);
}

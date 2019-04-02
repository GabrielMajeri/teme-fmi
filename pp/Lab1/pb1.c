#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

typedef struct {
    double a, b, c;
} Ecuatie;

typedef struct {
    bool are_solutie;
    double x1, x2;
} Rezultat;

Rezultat rezolva_ec(Ecuatie e) {
    Rezultat r = {0};
    
    double delta = 0.0;
    delta += e.b * e.b;
    delta -= 4.0 * e.a * e.c;
    
    if (delta < 0.0) {
        r.are_solutie = false;
    } else {
        r.are_solutie = true;
        
        delta = sqrt(delta);
       
        r.x1 = -e.b;
        r.x1 += delta;
        r.x1 /= 2.0 * e.a;
        
        r.x2 = -e.b;
        r.x2 -= delta;
        r.x2 /= 2.0 * e.a;
    }
    
    return r;
}

int main() {
    Ecuatie e;
    scanf("%lf %lf %lf", &e.a, &e.b, &e.c);
    
    printf("Ecuatia este %.2lf x^2 + %.2lf x + %.2lf\n", e.a, e.b, e.c);
    
    Rezultat rez = rezolva_ec(e);
    
    if (rez.are_solutie) {
        printf("x1=%.4lf\nx2=%.4lf\n", rez.x1, rez.x2);
    } else {
        printf("Nu are solutie\n");
    }	
}

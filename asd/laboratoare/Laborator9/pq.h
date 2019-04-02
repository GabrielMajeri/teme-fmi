#pragma once

typedef struct {
    int prio, val;
} Element;

typedef struct {
    Element* elem;
    int n, cap;
} PriorityQueue;

PriorityQueue pq_nou(int capacitate);
void pq_free(PriorityQueue* pq);

Element pq_min(const PriorityQueue* pq);
void pq_afisare(const PriorityQueue* pq);

void pq_sift_up(PriorityQueue* pq, int idx);
void pq_sift_down(PriorityQueue* pq, int idx);

void pq_insert(PriorityQueue* pq, int prio, int nr);
Element pq_pop(PriorityQueue* pq);
int pq_cauta(const PriorityQueue* pq, int nr);

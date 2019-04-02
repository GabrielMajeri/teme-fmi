#ifndef DEQUE_H
#define DEQUE_H

#include "../dubla/dubla.h"

typedef struct {
    Lista l;
} Deque;

Deque deque_nou(void);
void deque_free(Deque* d);

void deque_push_front(Deque* d, int info);
int deque_pop_front(Deque* d);

void deque_push_back(Deque* d, int info);
int deque_pop_back(Deque* d);

#endif

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>

void msleep(int millis) {
    struct timespec ts = {
        .tv_sec = 0,
        .tv_nsec = millis * 1000,
    };
    nanosleep(&ts, NULL);
}

const int MAX_RESOURCES = 5;
int available_resources = MAX_RESOURCES;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

int decrease_count(int count) {
    pthread_mutex_lock(&mutex);

    if (available_resources < count) {
        pthread_mutex_unlock(&mutex);
        return -1;
    }

    available_resources -= count;

    pthread_mutex_unlock(&mutex);

    return 0;
}

int increase_count(int count) {
    pthread_mutex_lock(&mutex);

    available_resources += count;

    pthread_mutex_unlock(&mutex);

    return 0;
}

void* use_resources(void* input) {
    int amount = 1 + rand() % MAX_RESOURCES;

    if (decrease_count(amount) == 0) {
        printf("Got %d resources, remaining %d\n", amount, available_resources);

        msleep(100);

        increase_count(amount);
        printf("Released %d resources, remaining %d\n", amount, available_resources);

        return NULL;
    } else {
        printf("Failed to acquire resources\n");
    }

    return NULL;
}

int main() {
    int num_threads = 8;

    pthread_t* worker_ids = malloc(sizeof(pthread_t) * num_threads);
    for (int i = 0; i < num_threads; ++i) {
        pthread_create(&worker_ids[i], NULL, use_resources, NULL);
    }

    msleep(500);

    for (int i = 0; i < num_threads; ++i) {
        pthread_join(worker_ids[i], NULL);
    }
}

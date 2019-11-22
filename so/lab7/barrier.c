#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>
#include <semaphore.h>

int current, max_threads;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
sem_t barrier;

void init(int n) {
    max_threads = n;
    current = 0;
    sem_init(&barrier, 0, 0);
}

void barrier_point() {
    pthread_mutex_lock(&mutex);
    current += 1;
    pthread_mutex_unlock(&mutex);

    if (current == max_threads) {
        sem_post(&barrier);
    }

    // Turnstile
    sem_wait(&barrier);
    sem_post(&barrier);
}

void* worker_thread(void* input) {
    const pid_t tid = syscall(SYS_gettid);

    printf("Thread %d reached the barrier\n", tid);

    barrier_point();

    printf("Thread %d passed the barrier\n", tid);

    return NULL;
}

int main() {
    int num_threads = 4;
    init(num_threads);

    pthread_t* worker_ids = malloc(sizeof(pthread_t) * num_threads);

    for (int i = 0; i < num_threads; ++i) {
        pthread_create(&worker_ids[i], NULL, worker_thread, NULL);
    }

    for (int i = 0; i < num_threads; ++i) {
        pthread_join(worker_ids[i], NULL);
    }

    // Clean up
    sem_destroy(&barrier);

    printf("Main thread finished\n");
}

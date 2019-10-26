#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>

int max_threads = 0;
int current = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cond_var = PTHREAD_COND_INITIALIZER;

pid_t gettid() {
    return syscall(SYS_gettid);
}

void init(int n) {
    max_threads = n;
    current = 0;
}

void barrier_point() {
    printf("Thread %d is entering the barrier\n", gettid());

    pthread_mutex_lock(&mutex);

    current++;
    printf("Now there are %d threads at the barrier\n", current);

    while (current < max_threads) {
        pthread_cond_wait(&cond_var, &mutex);
    }

    pthread_cond_broadcast(&cond_var);
    pthread_mutex_unlock(&mutex);
}

void* worker_thread(void* input) {
    barrier_point();
    printf("Thread %d finished\n", gettid());

    return NULL;
}

int main() {
    int num_threads = 4;
    init(5);

    pthread_t* worker_ids = malloc(sizeof(pthread_t) * num_threads);

    for (int i = 0; i < num_threads; ++i) {
        pthread_create(&worker_ids[i], NULL, worker_thread, NULL);
    }

    barrier_point();

    for (int i = 0; i < num_threads; ++i) {
        pthread_join(worker_ids[i], NULL);
    }

    printf("Main thread finished\n");
}

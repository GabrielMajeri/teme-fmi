#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>

/// Worker routine which reverses the string received as input
void* reverse_string(void* input) {
    char* input_str = input;
    size_t input_len = strlen(input_str);
    char* output_str = malloc(sizeof(char) * (input_len + 1));

    for (size_t i = 0; i < input_len; ++i) {
        output_str[i] = input_str[input_len - i - 1];
    }

    output_str[input_len] = 0;

    return output_str;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Expected one argument, the string to reverse\n");
        printf("Usage: %s 'some string'\n", argv[0]);
        return 1;
    }

    char* input_string = argv[1];

    // Launch a new worker thread
    pthread_t worker_id = 0;

    if (pthread_create(&worker_id, NULL, reverse_string, input_string) != 0) {
        perror("Failed to create worker thread");
        return 1;
    }

    // Wait for the worker to finish its job
    char* reversed = NULL;
    pthread_join(worker_id, (void**)&reversed);

    printf("Reversed: %s\n", reversed);

    free(reversed);
}

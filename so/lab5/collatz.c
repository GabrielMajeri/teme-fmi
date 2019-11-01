#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>

const char COLLATZ_SHM_NAME[] = "/collatz_shm";

// Macro for common cleanup code
#define CLEANUP_SHM \
    close(shm_fd); \
    shm_unlink(COLLATZ_SHM_NAME);

int main(int argc, char* argv[]) {
	// Check if we got the right number of arguments
    if (argc < 2) {
        printf("Expected at least one number");
        printf("Usage: %s n_1 ... n_k\n", argv[0]);
        return 1;
    }

	// Open the shared memory object
    int shm_fd = shm_open(COLLATZ_SHM_NAME, O_RDWR | O_CREAT | O_EXCL, S_IRWXU);
    if (shm_fd < 0) {
        perror("Failed to open shared memory handle");
        return 1;
    }

	// Allocate a page for each worker
    const int k = argc - 1;
    const size_t page_size = getpagesize();

    if (ftruncate(shm_fd, k * page_size) != 0) {
        perror("Failed to allocate shared memory");
        CLEANUP_SHM
        return 1;
    }

    int parent_pid = getpid();
    printf("Starting parent %d\n", parent_pid);

	// Map the data for the children
    int* results = mmap(NULL, k * page_size, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);

    for (int i = 0; i < k; ++i) {
        pid_t child_pid = fork();
        if (child_pid < 0) {
            perror("Failed to create child");
            CLEANUP_SHM
            return 1;
        } else if (child_pid == 0) {
            int* buf = results + i * page_size / sizeof(int);

            if (buf == MAP_FAILED) {
                perror("Failed to map memory");
                return 1;
            }

            int value = 0;
            sscanf(argv[1 + i], "%d", &value);

            int len = 0;

            if (value <= 1) {
                return 1;
            }

			// Compute the Collatz sequence
            do {
                buf[len++] = value;
                if (value % 2 == 0) {
                    value /= 2;
                } else {
                    value = value * 3 + 1;
                }
            } while (value != 1);
            buf[len] = value;

            buf[0] = len;

            return 0;
        }
    }

    for (int i = 0; i < k; ++i) {
        int status = 0;
        pid_t child_pid = wait(&status);
        printf("Done child %d exited with code %d\n", child_pid, status);
    }

    for (int i = 0; i < k; ++i) {
        int* buf = &results[i * page_size / 4];
        int size = buf[0];
        printf("%d\n", size);
        for (int j = 1; j <= size; ++j) {
            printf("%d ", buf[j]);
        }
        printf("\n");
    }

    munmap(results, k * page_size);

    CLEANUP_SHM
}

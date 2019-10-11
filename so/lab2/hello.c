#include <unistd.h>

int main() {
    const char message[] = "Hello world!\nThis is a message!\n";

    ssize_t result = write(STDOUT_FILENO, message, sizeof(message));

    if (result < 0) {
        return 1;
    }
}

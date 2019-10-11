#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define BUFFER_SIZE 8
char buffer[BUFFER_SIZE];

#define MINIMUM(a, b) ((a) < (b) ? (a) : (b))

// Computes the length of a nul-terminated string
size_t strlen(const char* s) {
    size_t len = 0;
    while (*s++) {
        ++len;
    }
    return len;
}

// Prints an error message
void print_error(const char* message) {
    write(STDERR_FILENO, message, strlen(message));
    write(STDERR_FILENO, "\n", 1);
}

int main(int argc, char** argv) {
    // Check arguments
    if (argc != 3) {
        print_error("Wrong parameter number");
        return 1;
    }

    const char* in_path = argv[1];
    const char* out_path = argv[2];

    // Open the original input file
    int in_fd = open(in_path, O_RDONLY);
    if (in_fd < 0) {
        print_error("Cannot open input file");
        return 1;
    }

    // Create a new output file
    int out_fd = creat(out_path, S_IRUSR | S_IWUSR);
    if (out_fd < 0) {
        print_error("Could not create output file");
        return 1;
    }

    // Determine how big the file is
    struct stat info;
    if (fstat(in_fd, &info) < 0) {
        print_error("Could not get file size");
        return 1;
    }

    const off_t file_size = info.st_size;

    // Data copy loop
    off_t xferred = 0;
    ssize_t read_result = 0;
    int has_error = 0;

    while (xferred < file_size) {
        const size_t left_to_read = file_size - xferred;
        const size_t to_read_now = MINIMUM(BUFFER_SIZE, left_to_read);

        // Try to read into the buffer
        read_result = read(in_fd, buffer, to_read_now);
        if (read_result < 0) {
            print_error("Error reading file");
            has_error = 1;
            break;
        }

        size_t written = 0;
        ssize_t write_result = 0;
        while (written < read_result) {
            const size_t left_to_write = read_result - written;
            write_result = write(out_fd, buffer, left_to_write);

            if (write_result < 0) {
                break;
            }

            written += write_result;
        }

        if (write_result < 0) {
            print_error("Error writing buffer to disk");
            has_error = 1;
            break;
        }

        xferred += read_result;
    }

    // Close file descriptors
    if (close(in_fd) < 0) {
        print_error("Could not close input file descriptor");
        has_error = 1;
    }

    if (close(out_fd) < 0) {
        print_error("Could not close output file descriptor");
        has_error = 1;
    }

    if (has_error) {
        print_error("File not fully written");
        return 1;
    }
}

use libc::{exit, write, EXIT_FAILURE, STDOUT_FILENO};

fn main() {
    let message = "Hello world!\nThis is a message!\n";

    // Print the UTF-8 message to the console by
    // writing it to the standard output file descriptor.
    let result = unsafe {
        let fd = STDOUT_FILENO;
        let buf = message.as_ptr() as *const _;
        let count = message.len();
        write(fd, buf, count)
    };

    // Check the result of the system call
    // and return a non-zero exit code on failure.
    if result < 0 {
        unsafe {
            exit(EXIT_FAILURE);
        }
    }
}

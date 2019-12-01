use std::{cmp, env, mem, result};

type Result<T> = result::Result<T, libc::c_int>;
type FileDescriptor = libc::c_int;

enum FileMode {
    ReadOnly,
    Create,
}

struct File {
    fd: FileDescriptor,
}

impl File {
    /// Opens a file and returns a descriptor to it
    fn open(path: &str, mode: FileMode) -> Result<Self> {
        let path = path.as_ptr() as *const libc::c_char;

        let result = unsafe {
            match mode {
                FileMode::ReadOnly => libc::open(path, libc::O_RDONLY),
                FileMode::Create => libc::creat(path, libc::S_IRUSR | libc::S_IWUSR),
            }
        };

        match result {
            error if result < 0 => Err(error),
            fd => Ok(Self { fd }),
        }
    }

    /// Returns the size, in bytes, of a given file.
    fn size(&self) -> Result<u64> {
        let mut info = mem::MaybeUninit::uninit();
        let result = unsafe { libc::fstat(self.fd, info.as_mut_ptr()) };

        match result {
            error if result < 0 => Err(error),
            _ok => unsafe { Ok(info.assume_init().st_size as u64) },
        }
    }

    /// Reads from a given file into a in-memory buffer
    fn read(&self, buffer: &mut [u8], how_much_to_read: usize) -> Result<u64> {
        let buffer = buffer.as_mut_ptr() as *mut _;
        let result = unsafe { libc::read(self.fd, buffer, how_much_to_read) };

        match result {
            error if result < 0 => Err(error as i32),
            ok => Ok(ok as u64),
        }
    }

    /// Writes from an input buffer to the file
    fn write(&self, buffer: &[u8], how_much_to_write: usize) -> Result<u64> {
        let buffer = buffer.as_ptr() as *const _;
        let result = unsafe { libc::write(self.fd, buffer, how_much_to_write) };

        match result {
            error if result < 0 => Err(error as i32),
            ok => Ok(ok as u64),
        }
    }
}

impl Drop for File {
    fn drop(&mut self) {
        if unsafe { libc::close(self.fd) } != 0 {
            panic!("Failed to close file");
        }
    }
}

fn main() {
    let (src, dst) = {
        let mut args = env::args();
        let program_name = args.next().unwrap();

        if args.len() != 2 {
            panic!(
                "Wrong number of parameters passed!\nUsage:\n\t{} input_path output_path",
                program_name
            );
        }

        // Open the original input file
        let src_path = args.next().unwrap();
        let src = File::open(&src_path, FileMode::ReadOnly).expect("Failed to open input file");

        // Create a new output file
        let dst_path = args.next().unwrap();
        let dst = File::open(&dst_path, FileMode::Create).expect("Failed to create output file");

        (src, dst)
    };

    const BUFFER_SIZE: usize = 8;
    let mut buffer = [0u8; BUFFER_SIZE];

    let file_size = src.size().expect("Could not determine input file size");

    let mut xferred = 0u64;

    // Data copy loop
    while xferred < file_size {
        let left_to_copy = file_size - xferred;
        let max_read_size = cmp::min(BUFFER_SIZE, left_to_copy as usize);

        // Fill the buffer
        let read_size = src
            .read(&mut buffer, max_read_size)
            .expect("Failed to read from input file");

        let mut written = 0;
        while written < read_size {
            let left_to_write = read_size - written;
            let write_size = dst
                .write(&buffer, left_to_write as usize)
                .expect("Failed to write to destination file");

            written += write_size;
        }

        xferred += read_size;
    }
}

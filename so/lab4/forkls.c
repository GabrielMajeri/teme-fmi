#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int main() {
	pid_t parent_pid = getpid();

	pid_t child_pid = fork();
	if (child_pid < 0) {
		fprintf(stderr, "Failed to fork process\n");
		return 1;
	} else if (child_pid == 0) {
		char ls_path[] = "/usr/bin/ls";
		char* argv[] = {ls_path, NULL};
		char* envp[] = {NULL};
		execve(ls_path, argv, envp);
	} else {
		printf("Parent PID = %d, Child PID = %d\n", parent_pid, child_pid);

		int status = 0;
		waitpid(child_pid, &status, 0);
		printf("Child process %d exited with status %d\n", child_pid, status);

		return 0;
	}
}

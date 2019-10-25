#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

void collatz(int num) {
	if (num <= 0) {
		return;
	}

	printf("%d: ", num);
	while (num != 1) {
		printf("%d ", num);
		if (num % 2 == 0) {
			num /= 2;
		} else {
			num = num * 3 + 1;
		}
	}
	printf("\n");
}

int main(int argc, char* argv[]) {
	if (argc < 2) {
		fprintf(stderr, "Not enough arguments:\n\t%s N_1 ... N_k\n", argv[0]);
		return 1;
	}

	int num_args = argc - 1;
	int* nums = (int*)malloc(num_args * sizeof(int));
	for (int i = 0; i < num_args; ++i) {
		const char* argument = argv[1 + i];
		if (sscanf(argument, "%d", nums + i) != 1) {
			fprintf(stderr, "Unable to convert input argument '%s' to number\n", argument);
			return 1;
		}
	}

	pid_t parent_pid = getpid();
	printf("Starting parent %d\n", parent_pid);

	pid_t* pids = (pid_t*)malloc(sizeof(pid_t) * num_args);

	for (int i = 0; i < num_args; ++i) {
		pid_t child_pid = fork();
		if (child_pid < 0) {
			fprintf(stderr, "Failed to fork process\n");
			return 1;
		} else if (child_pid == 0) {
			collatz(nums[i]);

			free(nums);
			free(pids);

			return 0;
		}
		pids[i] = child_pid;
	}

	for (int i = 0; i < num_args; ++i) {
		int status = 0;
		waitpid(pids[i], &status, 0);
		printf("Child process %d exited with status %d\n", pids[i], status);
	}

	free(nums);
	free(pids);
}

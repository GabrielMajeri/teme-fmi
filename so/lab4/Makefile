.SILENT:

all: build run

build: forkls collatz

run:
	printf "Running forkls\n"
	./forkls
	printf "\nRunning collatz\n"
	./collatz 24 9 16 25 36

clean:
	-rm forkls collatz
	-rm *.o

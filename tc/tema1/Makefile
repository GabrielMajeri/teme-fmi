.PHONY: run compile clean

CC := cc
CFLAGS := -Werror

run: build/main
	./$< Main.hs

compile: build/main

clean:
	-rm -r build

build:
	mkdir -p build

build/main: build/haskell.yy.o | build
	$(CC) $(CFLAGS) -o $@ $^ -lfl

build/haskell.yy.o: build/haskell.yy.c | build
	$(CC) $(CFLAGS) -o $@ -c $^

build/haskell.yy.c: haskell.yy | build
	flex -o $@ $^

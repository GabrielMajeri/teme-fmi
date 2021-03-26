CXXFLAGS := -Wall -Wextra
LDFLAGS :=
BISONFLAGS :=

.PHONY: compile clean

run: build/java-parser
	./$< HelloWorld.java

compile:
	build/java-parser

clean:
	-rm -r build

build:
	mkdir -p build

build/common.hpp: common.hpp
	cp $< $@

build/java-parser: build/java-parser.o build/java-lexer.o | build
	c++ $(LDFLAGS) -o $@ $^

build/java-parser.o: build/java-parser.cpp build/common.hpp | build
	c++ $(CXXFLAGS) -c -o $@ $<

build/java-parser.cpp: java-parser.ypp | build
	bison $(BISONFLAGS) -d -o $@ $<

build/java-lexer.o: build/java-lexer.cpp build/common.hpp | build
	cc $(CFLAGS) -c -o $@ $<

build/java-lexer.cpp: java-lexer.ypp | build
	flex -o $@ $<

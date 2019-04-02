CFLAGS += -Wall -Wextra -Werror -g -std=c99

NAME := pb4

.SILENT:

all: $(NAME)
	./$(NAME)

$(NAME): $(NAME).o dubla/dubla.o pq/pq.o deque/deque.o

$(NAME).o:

dubla/dubla.o: dubla/dubla.h

pq/pq.o: pq/pq.h

deque/deque.o: deque/deque.h

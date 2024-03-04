CC=gcc
CFLAGS=-Wall -Wextra

.PHONY: all clean

all: test_runtime

test_runtime: runtime.o test_runtime.o
	$(CC) $(CFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

run: test_runtime
	./test_runtime

clean:
	rm -f test_runtime *.o
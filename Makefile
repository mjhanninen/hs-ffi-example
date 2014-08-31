CC=clang

headers=$(wildcard *.h)
bins=TestFrobnicator test_frobnicator

.PHONY: all
all: $(bins)

.PHONY: clean
clean:
	rm *.o
	rm *.hi
	rm $(bins)

TestFrobnicator: Main.hs Frobnicator.hs frobnicator_lib.c $(headers)
	ghc -o $@ --make Main.hs frobnicator_lib.c

test_frobnicator: test_frobnicator.o frobnicator_lib.o
	$(CC) -o $@ $+

%.o: %.c $(headers)
	$(CC) -c -o $@ $<

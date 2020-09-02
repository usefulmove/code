# cointoss -- coin toss pattern match program

CC = g++
CFLAGS = -Wall
OWNER = bin
GROUP = bin
BINDIR = /usr/local/bin

cointoss: cointoss.cpp
	$(CC) $(CFLAGS) -o $@ $<

install: cointoss
	install -c -o $(OWNER) -g $(GROUP) -m 755 cointoss $(BINDIR)

clean:
	rm cointoss

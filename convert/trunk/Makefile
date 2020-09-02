# cnv -- ascii file converter

CC = g++
CFLAGS = -Wall
OWNER = bin
GROUP = bin
BINDIR = /usr/local/bin

cnv: cnv.cpp
	$(CC) $(CFLAGS) -o $@ $<

install: cnv
	install -c -o $(OWNER) -g $(GROUP) -m 755 cnv $(BINDIR)
	cp cnv.1.gz /usr/share/man/man1/

clean:
	rm cnv

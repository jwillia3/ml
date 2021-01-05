test: boot
	./boot test.ml
boot: boot.c
	$(CC) -g -Wswitch -oboot boot.c

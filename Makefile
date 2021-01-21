test: boot
	./boot ml.ml test.ml
boot: boot.c
	$(CC) -g -Wswitch -oboot boot.c

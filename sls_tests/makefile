
SOURCES = $(wildcard *.c)
ASM = $(patsubst %.c, %.s, $(SOURCES))
$(info $$var is [${SOURCES}])
$(ASM): $(SOURCES)
	$(CC) -O0 -S -c $^ 
clean: 
	rm -rf *.s

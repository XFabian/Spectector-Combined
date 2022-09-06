
SOURCES = $(wildcard *.c)
ASM = $(patsubst %.c, %.s, $(SOURCES))
$(info $$var is [${SOURCES}])
$(ASM): $(SOURCES)
	#$(CC) -S -c $^ 
	$(CC) -O0 -m32 -march=i386 -fno-stack-protector -static -no-pie -fno-pic -S -c $^ 
clean: 
	rm -rf *.s

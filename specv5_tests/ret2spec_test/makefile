SOURCES = $(wildcard *.c)
ASM = $(patsubst %.c, %.s, $(SOURCES))
$(info $$var is [${SOURCES}])
$(ASM): $(SOURCES)
	$(CC) -S -fcf-protection=branch -mmanual-endbr -c $^
clean:
	rm -rf *.s.*
# -fcf-protection=branch -mmanual-endbr disables CET feature of CPU for inidrect branches
# Since spectector does not parse endbr instructions and they look annoying

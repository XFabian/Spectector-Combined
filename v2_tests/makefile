

RET = retpoline_defense/
SOURCES = $(wildcard *.c)
ASM = $(patsubst %.c, %.s, $(SOURCES))
RETPO = $(patsubst %.c, retpo_%.s, $(SOURCES))
$(info $$var is [${SOURCES2}])

all: $(ASM) retpo
$(ASM): $(SOURCES)
	$(CC) -O0 -S -c $^ 

retpo : 
	cd $(RET) && $(MAKE)
clean:
	rm -f *.s
	rm -f $(RET)*.s
info :
	$(info $$var is [${RETPO}])

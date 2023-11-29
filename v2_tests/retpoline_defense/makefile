

SOURCES = $(wildcard *.c)
ASM = $(patsubst %.c, %.s, $(SOURCES))

all: $(ASM)

$(ASM): $(SOURCES)
	$(CC) -O0 -S -fcf-protection=none -mindirect-branch=thunk $^

clean:
	rm -rf *.s
info :
	$(info $$var is [${RETPO}])

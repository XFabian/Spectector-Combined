#include <stdio.h>
#include <stdint.h>
extern char afterspeculation[];

uint8_t secretarray[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
uint8_t publicarray2[256 * 2] = {0};

uint8_t idx = 0;
volatile uint8_t temp = 0;

void UnwindStack() {
asm volatile(
      "addq $8, %%rsp\n"
      "addq $8, %%rsp\n"
      "clflush (%%rsp)\n"
      "mfence\n"
      "lfence\n"
      "ret\n":::);
}

void speculation() {
    UnwindStack();
    temp &= publicarray2[secretarray[idx]];
}



int main() {
    speculation();
    asm volatile("afterspeculation:");
    return 0;
}

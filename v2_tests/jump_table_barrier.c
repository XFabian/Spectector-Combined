#include <stdio.h>
#include <stdint.h>
//int publicarray[16];
//int secretarray[256 * 512];

uint8_t publicarray[16] = { 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 };
// The attacker's goal in all of these examples is to learn any of the secret data in secretarray
uint32_t secretarray_size = 16;
uint8_t secretarray[16] = { 10,21,32,43,54,65,76,87,98,109,110,121,132,143,154,165 };

volatile uint8_t temp = 0;
void leaky(int idx) {
  asm("lfence");
  uint8_t val = secretarray[idx];
  temp &= publicarray[val];
  asm("lfence");
}


void main() {
    asm("lfence");
    asm("movl $5, %eax");
    int season = 1;
    static const void* table[] = {&&winter, &&spring, &&summer, &&fall};
    goto *table[season];

    winter:
        printf("Freezing\n");
        return;
    spring:
        printf("Dirty\n");
        return;
    summer:
        printf("Dry\n");
        return;
    fall:
        printf("Windy\n");
        return;
}

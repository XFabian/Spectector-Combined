#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef void (*Handler)(void);    /* A pointer to a handler function */

uint8_t publicarray[16] = { 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 };
// The attacker's goal in all of these examples is to learn any of the secret data in secretarray
uint32_t secretarray_size = 18;
uint8_t secretarray[18] = { 10,21,32,43,54,65,76,87,98,109,110,121,132,143,154,165,176,187 };

volatile uint8_t temp = 0;
void leaky(int idx) {
  uint8_t val = secretarray[idx];
  temp &= publicarray[val];
  asm("lfence");
}

void func3 (void) { printf( "Winter" ); asm("lfence");}
void func2 (void) { printf( "Fall" ); asm("lfence");}
void func1 (void) { printf( "Summer" ); asm("lfence");}
void func0 (void) { printf( "Spring" ); asm("lfence");}


Handler jump_table[4] = {func0, func1, func2, func3};



int main(int idx) {
    jump_table[1]();
}

#include <stdint.h>
#include <stddef.h>

uint32_t publicarray_size = 16;
uint8_t publicarray[16] = { 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 };
uint8_t publicarray2[512 * 1] = { 20 };

// The attacker's goal in all of these examples is to learn any of the secret data in secretarray
uint32_t secretarray_size = 16;
uint8_t secretarray[16] = { 10,21,32,43,54,65,76,87,98,109,110,121,132,143,154,165 };

// this is mostly used to prevent the compiler from optimizing out certain operations
volatile uint8_t temp = 0;

uint32_t case7_mask = UINT32_MAX;
void case_7(uint32_t idx) {  // INSECURE
  case7_mask = (secretarray_size - 1); // Bypassed store

  uint8_t toleak = publicarray[idx & case7_mask];
  temp &= publicarray2[toleak * 512];
}




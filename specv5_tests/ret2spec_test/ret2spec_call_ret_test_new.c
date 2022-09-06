#include <stdio.h>
// Call a "UnwindStackAndSlowlyReturnTo" function which unwinds the stack
// jumping back to the "afterspeculation" label in the "LeakByte" function
// never executing the code that follows.
extern char afterspeculation[];
int secretarray[16] = {};
int publicarray2[256 * 2] = {0};
int temp = 0;
int idx = 0;

void UnwindStackAndSlowlyReturnTo(const void *address) {

  asm volatile(
      "addq $8, %%rsp\n"
      "popstack:\n"
      "addq $8, %%rsp\n"
      "cmpq %0, (%%rsp)\n"
      "jne popstack\n"
      "clflush (%%rsp)\n"
      "mfence\n"
      "lfence\n"
      "ret\n"::"r"(address));
}
static void Speculation() {

  const void *return_address = afterspeculation;


  UnwindStackAndSlowlyReturnTo(return_address); // Never returns back here.

  // Everything that follows this is architecturally dead code. Never reached.
  // However, the first two statements are executed speculatively.
  temp &= publicarray2[secretarray[idx]];
  printf("Should not be printed");
}

static char LeakByte() {

    // Yields two "call" instructions, one "ret" instruction, speculatively
    // accesses the oracle and ends up on the afterspeculation label below.
    Speculation();

    // Return target for the UnwindStackAndSlowlyReturnTo function.
    asm volatile(
        "_afterspeculation:\n" // For MacOS.
        "afterspeculation:\n"); // For Linux.
    printf("After afterspeculation");  
}

void main() {
  LeakByte();
}

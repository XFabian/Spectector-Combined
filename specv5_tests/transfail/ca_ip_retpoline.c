#include <stdio.h>
#include <string.h>
#include <stdint.h>


size_t secret = 0; // we put the variable globally so spectector finds them correctly.
int real_secret = 83; // similar to original but now this secret is used by the victim Instead of assuming that 83 is a secret.

void __attribute__((noinline)) in_place()  {
    asm volatile("pop %%rax\n" : : : "rax");
    asm("jmp return_label");
}

  
void __attribute__((noinline)) attacker()  {
  while(1) {
    in_place();
    asm volatile("movq (%0), %%rax\n" : : "c"(secret) : "rax");
  }
}

void __attribute__((noinline)) victim()  {
    asm("return_label:");
    secret = real_secret; // Real secret only know to victim. We delayed the assignment of secret
    asm ("popq %rbp"); // only ret instruction is replaced by retpoline, we need to correctly pop the stack frame before
    asm("call return_new"); // The modified retpoline defense in retspec trapping speculation in infinte loop
    asm volatile("speculate:");
    asm volatile("nop");
    asm volatile ("jmp speculate");
    asm volatile("return_new:");
    asm volatile ("add $8, %rsp\t\n");
    asm volatile ("ret");

}

int main(int argc, char **argv) {
  attacker();
}

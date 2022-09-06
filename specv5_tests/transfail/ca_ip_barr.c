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
    asm("mfence"); // fence defense as described in retpoline
    asm volatile("movq (%0), %%rax\n" : : "c"(secret) : "rax");
  }
}

void __attribute__((noinline)) victim()  {
    asm("return_label:");
    secret = real_secret; // Real secret only know to victim. We delayed the assignment of secret 
    return; // This return is mispredicted to attacker

}

int main(int argc, char **argv) {
  attacker();
}

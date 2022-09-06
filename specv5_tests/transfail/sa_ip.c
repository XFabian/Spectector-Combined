#include <stdio.h>
#include <string.h>
#include <stdint.h>


size_t secret = 0; // we put the variable globally so spectecto finds them correctly. Otherwise just defintion makes the program insecure 

int real_secret = 83; // similar to original but now this secret is used by the victim Instead of assuming that 83 is a secret.

void __attribute__((noinline)) in_place()  {
    asm volatile("pop %%rax\n" : : : "rax");
    asm("jmp return_label");
}

void cache_encode(void *p) {
  asm volatile("movq (%0), %%rax\n" : : "c"(p) : "rax"); // Defintion of cache_encode without offsets and defintion of maccess
}
  
void __attribute__((noinline)) attacker()  {
    asm volatile("movq $65, %r14\t\n");
    in_place();
    //size_t secret = 0; // just this assignment makes the program unsafe. Because secret is speculatively accessed, since it is never executed normally.
    asm volatile("movq %%r14, %0\n\t": "=r"(secret));
    asm volatile("movq (%0), %%rax\n" : : "c"(secret) : "rax");
}

void __attribute__((noinline)) victim()  {
    asm("return_label:");
    //asm volatile("movq $83, %r14\t\n") old thing
    asm volatile("movq %0, %%r14\t\n" : :  "c"(real_secret)); 
    return; // This return is mispredicted to attacker

}

int main(int argc, char **argv) {
  attacker();
}

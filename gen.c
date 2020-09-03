#include <stdio.h>
#include <stdlib.h>
int main() {
char* ptr = (char*) malloc(0x4096);
*ptr += 8;
while (*ptr) {
ptr += 1;
*ptr += 4;
while (*ptr) {
ptr += 1;
*ptr += 2;
ptr += 1;
*ptr += 3;
ptr += 1;
*ptr += 3;
ptr += 1;
*ptr += 1;
ptr -= 4;
*ptr -= 1;}
ptr += 1;
*ptr += 1;
ptr += 1;
*ptr += 1;
ptr += 1;
*ptr -= 1;
ptr += 2;
*ptr += 1;
while (*ptr) {
ptr -= 1;}
ptr -= 1;
*ptr -= 1;}
ptr += 2;
putchar(*ptr);
ptr += 1;
*ptr -= 3;
putchar(*ptr);
*ptr += 7;
putchar(*ptr);
putchar(*ptr);
*ptr += 3;
putchar(*ptr);
ptr += 2;
putchar(*ptr);
ptr -= 1;
*ptr -= 1;
putchar(*ptr);
ptr -= 1;
putchar(*ptr);
*ptr += 3;
putchar(*ptr);
*ptr -= 6;
putchar(*ptr);
*ptr -= 8;
putchar(*ptr);
ptr += 2;
*ptr += 1;
putchar(*ptr);
ptr += 1;
*ptr += 2;
putchar(*ptr);return 0;
}

#include <stdio.h>

extern int bf();
extern char storage[0x4096];

int _print(int c) {
    printf("%c\n", c + '0');
}

int main() {
    int j = bf();

    for (int i = 0; i < 4096; i++) {
        if (storage[i] != 0) {
            printf("[%x] %d\t", i, storage[i]);
        }
    }
    printf("bf returned %d\n", j);
    return 0;
}
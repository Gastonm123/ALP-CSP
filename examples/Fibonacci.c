#include <stdio.h>

int fibo(unsigned N) {
    if (N == 1) return 0;
    return fibo(N-1) + fibo(N-2);
}

int main() {
    const int N = 10;
    printf("%d", fibo(N));
}
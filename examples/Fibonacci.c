#include <stdio.h>

int fibo(unsigned N) {
    printf("call.%d\n", N);
    if (N == 0) return 0;
    if (N == 1) return 0;
    return fibo(N-1) + fibo(N-2);
}

int main() {
    const int N = 5;
    printf("%d", fibo(N));
}
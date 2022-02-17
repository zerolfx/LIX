#include<stdio.h>

struct {
    void* a;  void* b;
} *__builtin_add2, *__builtin_printi;


long long __printi(long long i) {
    printf("%lld\n", i);
    return 1;
}
#include <stdio.h>

// Memory manager con logging para validar
// especificacion de memoria
//
const int BLOCK=16;
const int MEMSZ=4;
void *memoria = NULL;
bool libre[MEMSZ];

void *mi_malloc() {
    if (memoria == NULL) {
        memoria = malloc(MEMSZ*BLOCK);
        for (int i = 0; i < MEMSZ; i++)
            libre[i] = 1;
    }
    for (int i = 0; i < MEMSZ; i++) {
        if (libre[i]) {
            libre[i] = 0;
            printf("malloc.%d\n", i);
            return memoria + i*BLOCK;
        }
    }
    printf("memfull\n");
    return NULL;
}

void mi_free(void *ptr) {
    printf("free.%d\n", (ptr-memoria)/BLOCK);
    libre[(ptr-memoria)/BLOCK] = 1;
}

int main() {
    void *list = NULL;
    while (1) {
        int v;
        printf("%d", v);
        if (v < 0) {
            while (list != NULL) {
                void *next = *(list);
                mi_free(list);
                list = next;
            }
        }
        void *new = mi_malloc();
        if (new != NULL) {
            printf("save.%d\n", v);
            *(new)   = list;
            *(new+8) = v;
            list = new;
        }
    }
}
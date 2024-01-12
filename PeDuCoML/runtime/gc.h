#include <malloc.h>
#include <stdint.h>
typedef struct
{
    int is_free;
    size_t size;
} mm_block;

typedef struct
{
    void *current_bank;
    void *secondary_bank;
} mempool;

void peducoml_init();

void peducoml_destroy();

void *peducoml_alloc(size_t size);

int64_t check_pointer(int64_t ptr, int64_t cur_stack);
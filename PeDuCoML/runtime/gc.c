#include <malloc.h>
#include <stdint.h>
#include "gc.h"

static mempool *pool = NULL;
static int64_t stack_bottom = 0;
const size_t INFO_SIZE = sizeof(mm_block);
const size_t INITIAL_SIZE = 1048576;

// TODO: Recursive heap scanning. Without it, some required objects may be deleted

void peducoml_init(int64_t sb)
{
    stack_bottom = sb;
    pool = (mempool *)malloc(sizeof(mempool));
    pool->current_bank = (void *)malloc(INITIAL_SIZE);
    pool->secondary_bank = (void *)malloc(INITIAL_SIZE);

    mm_block *block = pool->current_bank;
    block->is_free = 1;
    block->size = INITIAL_SIZE;

    block = pool->secondary_bank;
    block->is_free = 1;
    block->size = INITIAL_SIZE;
}

void peducoml_destroy()
{
    free(pool->current_bank);
    pool->current_bank = NULL;
    free(pool->secondary_bank);
    pool->secondary_bank = NULL;
    free(pool);
    pool = NULL;
}

void *peducoml_alloc_in_bank(size_t size, void *bank)
{
    void *cur = bank;
    mm_block *cur_block = cur;

    while (cur < bank + INITIAL_SIZE && (!cur_block->is_free || cur_block->size < size))
    {
        cur += cur_block->size + INFO_SIZE;
        cur_block = cur;
    }

    if (cur >= bank + INITIAL_SIZE)
    {
        return NULL;
    }

    mm_block *next_block = cur + INFO_SIZE + size;
    if (size != cur_block->size)
    {
        if (cur_block->size > size + INFO_SIZE)
        {
            size_t prev_size = cur_block->size;
            cur_block->size = size;

            cur_block->is_free = 0;

            if (next_block < bank + INITIAL_SIZE)
            {
                next_block->is_free = 1;
                next_block->size = prev_size - size - INFO_SIZE;
            }

            return cur + INFO_SIZE;
        }

        cur_block->is_free = 0;
        return cur + INFO_SIZE;
    }

    cur_block->is_free = 0;
    cur_block->size = size;

    return cur + INFO_SIZE;
}

void print_blocks()
{
    fprintf(stderr, "Printing the blocks:\n");
    void *current = pool->current_bank;
    mm_block *block = current;
    int cnt = 0;
    while (current < pool->current_bank + INITIAL_SIZE)
    {
        fprintf(stderr, "Block %ld: pointer=%ld is_free=%ld size=%ld\n", cnt++, current + INFO_SIZE, block->is_free, block->size);
        if (block->size <= 0)
        {
            exit(1);
        }
        current += block->size + INFO_SIZE;
        block = current;
    }
}

void *peducoml_alloc(size_t size)
{
    if (size <= 0)
    {
        return NULL;
    }
    void *alloc = peducoml_alloc_in_bank(size, pool->current_bank);
    if (alloc != NULL)
    {
        return alloc;
    }

    gc();
    alloc = peducoml_alloc_in_bank(size, pool->current_bank);
    if (alloc != NULL)
    {
        return alloc;
    }

    exit(1);
}

void peducoml_free(void *ptr)
{
    if (ptr == NULL)
        return;

    mm_block *block_info = ptr - INFO_SIZE;
    block_info->is_free = 1;
    mm_block *next_block = ptr + block_info->size;

    if (next_block->is_free)
    {
        block_info->size = block_info->size + next_block->size + INFO_SIZE;
    }

    void *current = pool->current_bank;
    mm_block *prev_block = pool->current_bank;
    while (current < pool->current_bank + INITIAL_SIZE)
    {
        if (current + INFO_SIZE + prev_block->size == ptr - INFO_SIZE && prev_block->is_free)
        {
            prev_block->size += block_info->size + INFO_SIZE;

            return;
        }
        current += INFO_SIZE + prev_block->size;
        prev_block = current;
    }
}

void gc()
{
    mm_block *block = pool->secondary_bank;
    block->is_free = 1;
    block->size = INITIAL_SIZE;
    gc_stack_scan(stack_bottom);
    void *tmp = pool->current_bank;
    pool->current_bank = pool->secondary_bank;
    pool->secondary_bank = tmp;
}

int64_t check_pointer(int64_t ptr, int64_t cur_stack)
{
    if (pool->current_bank <= ptr && ptr < pool->current_bank + INITIAL_SIZE)
    {
        mm_block *block = (mm_block *)(ptr - INFO_SIZE);
        void *new_location = peducoml_alloc_in_bank(block->size, pool->secondary_bank);
        return (int64_t)new_location;
    }
    return ptr;
}
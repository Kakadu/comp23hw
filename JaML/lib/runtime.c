/*
    Copyright 2024, Ilya Pankratov, Maxim Drumov

    SPDX-License-Identifier: LGPL-2.1-or-later
*/

#include <stdio.h>
#include <stdlib.h>

#include "runtime.h"

/* pa = partial application */
typedef struct
{
    uint64_t *func_ptr;
    uint64_t curr_args_n;
    uint64_t max_args_n;
    uint64_t *args;
} pa;

uint64_t tuple_make(uint64_t n, ...)
{
    va_list vl;
    uint64_t tuple_arg;
    uint64_t *tuple = (uint64_t *)malloc(n * sizeof(uint64_t));

    va_start(vl, n);
    for (uint64_t i = 0; i < n; i++)
    {
        tuple_arg = va_arg(vl, uint64_t);
        tuple[i] = tuple_arg;
    }
    va_end(vl);

    return (uint64_t)tuple;
}

uint64_t tuple_take(uint64_t tuple, uint64_t n)
{
    return ((uint64_t *)tuple)[n];
}

uint64_t apply_n(uint64_t (*func_ptr)(uint64_t, ...), uint64_t n, uint64_t *args)
{

    uint64_t result = 0;

    switch (n)
    {
    case 1:
        result = func_ptr(args[0]);
        break;
    case 2:
        result = func_ptr(args[0], args[1]);
        break;
    case 3:
        result = func_ptr(args[0], args[1], args[2]);
        break;
    case 4:
        result = func_ptr(args[0], args[1], args[2], args[3]);
        break;
    case 5:
        result = func_ptr(args[0], args[1], args[2], args[3], args[4]);
        break;
    case 6:
        result = func_ptr(args[0], args[1], args[2], args[3], args[4], args[5]);
        break;
    case 7:
        result = func_ptr(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
        break;
    case 8:
        result = func_ptr(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
        break;
    case 9:
        result = func_ptr(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8]);
        break;
    case 10:
        result = func_ptr(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9]);
        break;
    case 11:
        result = func_ptr(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10]);
        break;
    case 12:
        result = func_ptr(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11]);
        break;
    case 13:
        result = func_ptr(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12]);
        break;
    case 14:
        result = func_ptr(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13]);
        break;
    case 15:
        result = func_ptr(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[4]);
        break;
    case 16:
        result = func_ptr(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15]);
        break;
    default:
        perror("apply_n does not support applying more than 16 arguments");
        exit(EXIT_FAILURE);
    }

    return result;
}

uint64_t make_pa(uint64_t f_ptr, uint64_t max_args, uint64_t n, ...)
{
    uint64_t *args = (uint64_t *)malloc(max_args * sizeof(uint64_t));
    va_list vl;

    va_start(vl, n);
    for (uint64_t i = 0; i < n; i++)
    {
        args[i] = va_arg(vl, uint64_t);
    }
    va_end(vl);

    pa *partial = (pa *)malloc(sizeof(pa));
    partial->args = args;
    partial->func_ptr = (uint64_t *)f_ptr;
    partial->curr_args_n = n;
    partial->max_args_n = max_args;

    return (uint64_t)partial;
}

/*
    When function returns function in JaML, that function should be
    returned as pointer to a closure to which zero arguments is applied.
*/
uint64_t add_args_to_pa(uint64_t partial_app, uint64_t n, ...)
{

    pa *partial = (pa *)partial_app;
    va_list vl;
    va_start(vl, n);

    for (uint64_t i = 0; i < n; i++)
    {
        partial->args[partial->curr_args_n++] = va_arg(vl, uint64_t);

        if (partial->curr_args_n == partial->max_args_n)
        {
            uint64_t (*valid_ptr)(uint64_t, ...) = (uint64_t(*)(uint64_t, ...))partial->func_ptr;
            uint64_t tmp = apply_n(valid_ptr, partial->max_args_n, partial->args);

            // Free applied partial application
            free(partial->args);
            free(partial);

            partial = (pa *)tmp;
        }
    }

    va_end(vl);

    return (uint64_t)partial;
}

// Copyright 2023-2024, Arthur Alekseev and Starcev Matvey

// SPDX-License-Identifier: LGPL-3.0-or-later

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define min(a, b) (((a) < (b)) ? (a) : (b))

#define cmptr int64_t // CreeperML pointer

typedef struct str_str
{
    int len;
    int *data;
} strr;

typedef struct function_str
{
    cmptr (*fn)(); // The pointer to function itself
    cmptr *argv;   // Arguments to be applied
    cmptr argc;    // How many arguments there are already
    cmptr arity;   // How many argument does function take at all
    cmptr fn_id;   // Id of function in asm code
} function;

void bin_print_int(const int i)
{
    printf("%d\n", i);
}

void print_string_raw(const char *str)
{
    printf("%s\n", str);
}

void bin_print_string(const strr *str)
{
    int len = str->len;

    for (int i = 0; i < len; i++)
    {
        printf("%c", str->data[i]);
    }
}

cmptr create_function(cmptr fn, cmptr argc, cmptr argv, cmptr arity, cmptr fn_id);

cmptr cm_malloc(size_t size)
{
    return (cmptr)malloc(size);
}

cmptr call_n(function *closure)
{
    cmptr *a = closure->argv;

    switch (closure->arity)
    {
    case 0:
        return closure->fn();
    case 1:
        return closure->fn(a[0]);
    case 2:
        return closure->fn(a[0], a[1]);
    case 3:
        return closure->fn(a[0], a[1], a[2]);
    case 4:
        return closure->fn(a[0], a[1], a[2], a[3]);
    case 5:
        return closure->fn(a[0], a[1], a[2], a[3], a[4]);
    case 6:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    case 8:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
    case 9:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
    case 10:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]);
    case 11:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10]);
    case 12:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11]);
    case 13:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12]);
    case 14:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13]);
    case 15:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14]);
    case 16:
        return closure->fn(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15]);
    default:
        printf("Too many arguments applied (max 16)\n");
        exit(1);
    }
}

void print_function(function *clsr)
{
    printf("+-------------------\n");
    printf("| [%ld]\n", clsr);
    printf("| fn address: %ld\n", clsr->fn);
    printf("| fn id : %ld\n", clsr->fn_id);
    printf("| arity: %ld\n", clsr->arity);
    printf("| argc: %ld\n| argv: ", clsr->argc);
    for (int i = 0; i < clsr->argc; i++)
        printf("%ld ", clsr->argv[i]);
    printf("\n\n");
}

cmptr apply_args(function *clsr, cmptr argc, cmptr *argv)
{
    // Clone closure object to avoid confusion

    function *clsr_old = clsr;
    clsr = (function *)create_function((cmptr)clsr_old->fn, clsr_old->argc, (cmptr)clsr_old->argv, clsr_old->arity, clsr_old->fn_id);
    cmptr all_argc = clsr->argc + argc;

    // Merge arguments
    cmptr *clsr_argv = clsr->argv;

    clsr->argv = (cmptr *)cm_malloc(all_argc * sizeof(cmptr));

    for (int i = 0; i < clsr->argc; i++)
        clsr->argv[i] = clsr_argv[i];

    for (int i = clsr->argc; i < all_argc; i++)
        clsr->argv[i] = argv[i - clsr->argc];

    cmptr awaited = clsr->arity - clsr->argc;
    cmptr applied = min((int)argc, (int)awaited);
    cmptr remaining = argc - awaited;

    clsr->argc += applied;

    // Apply arguments
    if (remaining > 0)
    {
        function *res = (function *)call_n(clsr);

        for (int i = 0; i < remaining; i++)
            argv[i] = argv[i + applied];

        return apply_args(res, remaining, argv);
    }

    if (remaining == 0)
    {
        cmptr res = call_n(clsr);

        return res;
    }

    return (cmptr)clsr;
}

cmptr create_function(cmptr fn, cmptr argc, cmptr argv, cmptr arity, cmptr fn_id)
{
    function *clsr = (function *)cm_malloc(sizeof(function));
    clsr->arity = arity; // Summary size of all arguments
    clsr->fn = (cmptr(*)())fn;
    clsr->argv = (cmptr *)argv;
    clsr->argc = argc;
    clsr->fn_id = fn_id;

    return (cmptr)clsr;
}
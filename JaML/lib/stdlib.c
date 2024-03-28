/*
    Copyright 2024, Ilya Pankratov, Maxim Drumov

    SPDX-License-Identifier: LGPL-2.1-or-later
*/

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

uint64_t print_int(int64_t n)
{
    printf("%" PRId64 "\n", n);
    return 0;
}

uint64_t print_bool(uint64_t n)
{
    char *data = n != 0 ? "true" : "false";
    printf("%s\n", data);
    return 0;
}

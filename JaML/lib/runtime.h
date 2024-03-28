/*
    Copyright 2024, Ilya Pankratov, Maxim Drumov

    SPDX-License-Identifier: LGPL-2.1-or-later
*/

#include <stdint.h>
#include <stdarg.h>

uint64_t tuple_make(uint64_t n, ...);
uint64_t tuple_take(uint64_t tuple, uint64_t n);
uint64_t apply_n(uint64_t (*func_ptr)(uint64_t, ...), uint64_t n, uint64_t *args);
uint64_t make_pa(uint64_t f_ptr, uint64_t max_args, uint64_t n, ...);
uint64_t add_args_to_pa(uint64_t partial_app, uint64_t n, ...);

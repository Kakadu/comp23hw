/*
    Copyright 2024, Ilya Pankratov, Maxim Drumov

    SPDX-License-Identifier: LGPL-2.1-or-later
*/

#include <stdio.h>
#include <stdlib.h>

#include "runtime.h"

/* Constants for formatting tests output */
#define RED "\033[31m"
#define GREEN "\033[32m"
#define RESET "\033[0m"
#define TAB "\t"

/* Macros for running tests */
#define TESTS                                                             \
    test_tuple_make_of_integers, test_tuple_make_of_integer_tuples,       \
        test_tuple_take1, test_tuple_take2, test_apply_n1, test_apply_n2, \
        test_apply_n3, test_add_args_to_pa_func_return_sum2,              \
        test_add_args_to_pa_func_return_sum3, test_add_args_to_pa_func_return_sum3_with_applied_zero
#define TESTS_NAMES                                                               \
    "test_tuple_make_of_integers", "test_tuple_make_of_integer_tuples",           \
        "test_tuple_take1", "test_tuple_take2", "test_apply_n1", "test_apply_n2", \
        "test_apply_n3", "test_add_args_to_pa_func_return_sum2",                  \
        "test_add_args_to_pa_func_return_sum3", "test_add_args_to_pa_func_return_sum3_with_applied_zero"
#define MAKE_ARRAY(...)   \
    {                     \
        __VA_ARGS__, NULL \
    }

/* Asserts for tests */
#define ASSERT_EQ(arg1, arg2) \
    if (arg1 != arg2)         \
    return 1

/* START: some function for partial application tests */

uint64_t id(uint64_t x)
{
    return x;
}

uint64_t sum2(uint64_t a, uint64_t b)
{
    return a + b;
}

uint64_t sum3(uint64_t a, uint64_t b, uint64_t c)
{
    return a + b + c;
}

uint64_t func_return_sum2(uint64_t, uint64_t, uint64_t)
{
    return make_pa((uint64_t)&sum2, 2, 0);
}

uint64_t func_return_sum3(uint64_t, uint64_t, uint64_t)
{
    return make_pa((uint64_t)&sum3, 3, 0);
}

uint64_t func_return_sum3_with_applied_zero(uint64_t, uint64_t, uint64_t)
{
    return make_pa((uint64_t)&sum3, 3, 1, 0);
}

/* END: some function for partial application tests */

int test_tuple_make_of_integers()
{
    int n = 10;
    uint64_t test_data[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    uint64_t *tuple = (uint64_t *)tuple_make(n, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

    for (int i = 0; i < n; i++)
    {
        if (tuple[i] != test_data[i])
        {
            return 1;
        }
    }

    return EXIT_SUCCESS;
}

int test_tuple_make_of_integer_tuples()
{

    int n = 2;
    uint64_t tuple1 = tuple_make(n, 0, 0);
    uint64_t tuple2 = tuple_make(n, 1, 1);
    uint64_t *tuple = (uint64_t *)tuple_make(n, tuple1, tuple2);

    for (int i = 0; i < n; i++)
    {

        uint64_t *inner_tuple = (uint64_t *)tuple[i];
        for (int j = 0; j < n; j++)
        {
            ASSERT_EQ(inner_tuple[j], (uint64_t)i);
        }
    }

    return EXIT_SUCCESS;
}

int test_tuple_take1()
{
    int n = 10;
    uint64_t tuple = tuple_make(n, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9);

    ASSERT_EQ(tuple_take(tuple, 0), 0);
    ASSERT_EQ(tuple_take(tuple, 1), 1);
    ASSERT_EQ(tuple_take(tuple, 2), 2);
    ASSERT_EQ(tuple_take(tuple, 3), 3);
    ASSERT_EQ(tuple_take(tuple, 4), 4);
    ASSERT_EQ(tuple_take(tuple, 5), 5);
    ASSERT_EQ(tuple_take(tuple, 6), 6);
    ASSERT_EQ(tuple_take(tuple, 7), 7);
    ASSERT_EQ(tuple_take(tuple, 8), 8);
    ASSERT_EQ(tuple_take(tuple, 9), 9);

    return EXIT_SUCCESS;
}

int test_tuple_take2()
{
    int n = 2;
    uint64_t tuple1 = tuple_make(n, 0, 1);
    uint64_t tuple2 = tuple_make(n, 2, 3);
    uint64_t tuple3 = tuple_make(n, tuple1, tuple2);

    ASSERT_EQ(tuple_take(tuple3, 0), tuple1);
    ASSERT_EQ(tuple_take(tuple3, 1), tuple2);
    ASSERT_EQ(tuple_take(tuple_take(tuple3, 0), 0), 0);
    ASSERT_EQ(tuple_take(tuple_take(tuple3, 0), 1), 1);
    ASSERT_EQ(tuple_take(tuple_take(tuple3, 1), 0), 2);
    ASSERT_EQ(tuple_take(tuple_take(tuple3, 1), 1), 3);

    return EXIT_SUCCESS;
}

int test_apply_n1()
{
    uint64_t *func_ptr = (uint64_t *)(&id);
    uint64_t (*valid_ptr)(uint64_t, ...) = (uint64_t(*)(uint64_t, ...))func_ptr;
    uint64_t n = 1;
    uint64_t args[1] = {42};

    ASSERT_EQ(apply_n(valid_ptr, n, (uint64_t *)args), id(args[0]));

    return EXIT_SUCCESS;
}

int test_apply_n2()
{
    uint64_t *func_ptr = (uint64_t *)(&sum2);
    uint64_t (*valid_ptr)(uint64_t, ...) = (uint64_t(*)(uint64_t, ...))func_ptr;
    uint64_t n = 2;
    uint64_t args[2] = {42, 1};

    ASSERT_EQ(apply_n(valid_ptr, n, (uint64_t *)args), sum2(args[0], args[1]));

    return EXIT_SUCCESS;
}

int test_apply_n3()
{
    uint64_t *func_ptr = (uint64_t *)(&sum3);
    uint64_t (*valid_ptr)(uint64_t, ...) = (uint64_t(*)(uint64_t, ...))func_ptr;
    uint64_t n = 3;
    uint64_t args[3] = {42, 1, 2};

    ASSERT_EQ(apply_n(valid_ptr, n, (uint64_t *)args), sum3(args[0], args[1], args[2]));

    return EXIT_SUCCESS;
}

int test_add_args_to_pa_func_return_sum2()
{
    uint64_t returned_f = func_return_sum2(0, 0, 0);
    uint64_t p1 = add_args_to_pa(returned_f, 1, 1);
    uint64_t sum2_res = add_args_to_pa(p1, 1, 1);

    ASSERT_EQ(sum2_res, 2);

    return EXIT_SUCCESS;
}

int test_add_args_to_pa_func_return_sum3()
{
    uint64_t returned_f = func_return_sum3(0, 0, 0);
    uint64_t sum3_res = add_args_to_pa(returned_f, 3, 1, 1, 1);

    ASSERT_EQ(sum3_res, 3);

    return EXIT_SUCCESS;
}

int test_add_args_to_pa_func_return_sum3_with_applied_zero()
{
    uint64_t returned_f = func_return_sum3_with_applied_zero(0, 0, 0);
    uint64_t res = add_args_to_pa(returned_f, 2, 1, 1);

    ASSERT_EQ(res, 2);

    return EXIT_SUCCESS;
}

int RUN_ALL_TESTS()
{
    printf("Running tests for runtime...\n");
    char *test_names[] = MAKE_ARRAY(TESTS_NAMES);
    int (*tests[])() = MAKE_ARRAY(TESTS);

    int tests_result = 0;

    for (int i = 0; tests[i] != NULL; i++)
    {
        int curr_test_result = tests[i]();

        if (curr_test_result != EXIT_SUCCESS)
        {
            printf(TAB "TEST %s " RED "FAILED" RESET "\n", test_names[i]);
        }
        else
        {
            printf(TAB "TEST %s " GREEN "PASSED" RESET "\n", test_names[i]);
        }

        tests_result |= curr_test_result;
    }

    return tests_result;
}

int main()
{
    return RUN_ALL_TESTS();
}

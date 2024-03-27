#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define pointer int64_t

typedef struct closure_struct
{
    pointer (*fun)();
    pointer *applied;
    pointer applied_count;
    pointer arity;
} closure;

pointer m_allocate_closure(pointer fun, pointer applied_count, pointer *applied, pointer arity)
{
    closure *new_closure = (closure *)malloc(sizeof(closure));
    new_closure->arity = arity;
    new_closure->fun = (pointer(*)())fun;
    new_closure->applied = (pointer *)applied;
    new_closure->applied_count = applied_count;

    return (pointer)new_closure;
}

closure *clone(closure *initial)
{
    closure *closure_old = initial;

    // Copy closure
    closure *closure_new = (closure *)m_allocate_closure(
        (pointer)closure_old->fun,
        closure_old->applied_count,
        closure_old->applied,
        closure_old->arity);

    // Copy arguments
    pointer *arguments = malloc(sizeof(pointer) * closure_new->arity);

    for (int i = 0; i < closure_new->applied_count; i++)
        arguments[i] = closure_new->applied[i];

    closure_new->applied = arguments;

    return closure_new;
}

pointer evaluate_closure(closure *target)
{
    pointer *a = target->applied;

    switch (target->arity)
    {
    case 0:
        return target->fun();
    case 1:
        return target->fun(a[0]);
    case 2:
        return target->fun(a[0], a[1]);
    case 3:
        return target->fun(a[0], a[1], a[2]);
    case 4:
        return target->fun(a[0], a[1], a[2], a[3]);
    case 5:
        return target->fun(a[0], a[1], a[2], a[3], a[4]);
    case 6:
        return target->fun(a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7:
        return target->fun(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    case 8:
        return target->fun(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
    case 9:
        return target->fun(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
    case 10:
        return target->fun(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]);
    default:
        exit(1);
    }
}

pointer m_malloc(pointer size)
{
    return (pointer)malloc(size);
}

pointer m_apply(closure *old_closure, pointer argument)
{
    closure *new_closure = clone(old_closure);

    new_closure->applied[new_closure->applied_count] = argument;
    new_closure->applied_count++;

    if (new_closure->applied_count == new_closure->arity)
    {
        pointer result = evaluate_closure(new_closure);
        return result;
    }

    return (pointer)new_closure;
}

// Arithmetics and logics written here to allow partial application with function address

pointer m_op_add(pointer x, pointer y) { return x + y; }
pointer m_op_sub(pointer x, pointer y) { return x - y; }
pointer m_op_mul(pointer x, pointer y) { return x * y; }
pointer m_op_div(pointer x, pointer y) { return x / y; }

pointer m_op_equals(pointer x, pointer y) { return x == y; }
pointer m_op_less(pointer x, pointer y) { return x < y; }
pointer m_op_less_equals(pointer x, pointer y) { return x <= y; }
pointer m_op_greater(pointer x, pointer y) { return x > y; }
pointer m_op_greater_equals(pointer x, pointer y) { return x >= y; }
pointer m_op_less_greater(pointer x, pointer y) { return x != y; }

void print_int(pointer i)
{
    printf("%li\n", i);

    fflush(stdout);
}

void print_bool(pointer i)
{
    if (i == 1)
        printf("true");
    else
        printf("false");

    fflush(stdout);
}

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>


typedef int64_t (*fun_ptr)();

typedef struct Closure {
	fun_ptr fun;
	int64_t args_cnt;
	int64_t applied_args_cnt;
	int64_t* applied_args;
}* ClosurePtr;
typedef struct Closure Closure;

static int64_t apply0(ClosurePtr ptr) {
	return ptr->fun();
}

static int64_t apply1(ClosurePtr ptr) {
	return ptr->fun(
		ptr->applied_args[0]
	);
}

static int64_t apply2(ClosurePtr ptr) {
	return ptr->fun(
		ptr->applied_args[0],
		ptr->applied_args[1]
	);
}

static int64_t apply3(ClosurePtr ptr) {
	return ptr->fun(
		ptr->applied_args[0],
		ptr->applied_args[1],
		ptr->applied_args[2]
	);
}

static int64_t apply4(ClosurePtr ptr) {
	return ptr->fun(
		ptr->applied_args[0],
		ptr->applied_args[1],
		ptr->applied_args[2],
		ptr->applied_args[3]
	);
}

static int64_t apply5(ClosurePtr ptr) {
	return ptr->fun(
		ptr->applied_args[0],
		ptr->applied_args[1],
		ptr->applied_args[2],
		ptr->applied_args[3],
		ptr->applied_args[4]
	);
}

static int64_t apply6(ClosurePtr ptr) {
	return ptr->fun(
		ptr->applied_args[0],
		ptr->applied_args[1],
		ptr->applied_args[2],
		ptr->applied_args[3],
		ptr->applied_args[4],
		ptr->applied_args[5]
	);
}

static int64_t apply7(ClosurePtr ptr) {
	return ptr->fun(
		ptr->applied_args[0],
		ptr->applied_args[1],
		ptr->applied_args[2],
		ptr->applied_args[3],
		ptr->applied_args[4],
		ptr->applied_args[5],
		ptr->applied_args[6]
	);
}

static int64_t apply8(ClosurePtr ptr) {
	return ptr->fun(
		ptr->applied_args[0],
		ptr->applied_args[1],
		ptr->applied_args[2],
		ptr->applied_args[3],
		ptr->applied_args[4],
		ptr->applied_args[5],
		ptr->applied_args[6],
		ptr->applied_args[7]
	);
}

static int64_t (*callees[9])(ClosurePtr) = {
	apply0,
	apply1,
	apply2,
	apply3,
	apply4,
	apply5,
	apply6,
	apply7,
	apply8,
};

static ClosurePtr copy_closure(ClosurePtr old)
{
    ClosurePtr new = (ClosurePtr)malloc(sizeof(Closure));
    new->fun = old->fun;
    new->args_cnt = old->args_cnt;
    new->applied_args_cnt = old->applied_args_cnt;
    new->applied_args = (int64_t*)malloc(old->args_cnt * sizeof(int64_t));
    for (int i = 0; i < old->applied_args_cnt; i++)
    {
        new->applied_args[i] = old->applied_args[i];
    }
    return new;
}

extern int64_t alloc_closure(int64_t fun, int64_t args_cnt) {
	ClosurePtr new_closure = (ClosurePtr)malloc(sizeof(Closure));
	new_closure->fun = (fun_ptr)fun;
	new_closure->args_cnt = args_cnt;
	new_closure->applied_args_cnt = 0;
	new_closure->applied_args = (int64_t*)malloc(args_cnt * sizeof(int64_t));

	if (args_cnt == 0) {
		int64_t res = callees[0](new_closure);
		free(new_closure);
		return res;
	}

	return (int64_t)new_closure;
}

extern int64_t apply_closure(int64_t ptr, int64_t arg) {
	ClosurePtr closure = copy_closure((ClosurePtr)ptr);
	closure->applied_args[closure->applied_args_cnt++] = arg;
	if (closure->applied_args_cnt != closure->args_cnt) {
		return (int64_t)closure;
	}

	int64_t result = callees[closure->args_cnt](closure);
	free(closure->applied_args);
	free(closure);
	return result;
}

extern int64_t print_int(int64_t value) {
    printf("%ld", value);
    return 0;
}

extern int64_t print_bool(int64_t value) {
	value ? printf("true") : printf("false");
	return 0;
}

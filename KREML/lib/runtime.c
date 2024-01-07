#include <stdlib.h>
#include <stdint.h>


typedef int64_t (*fun_ptr)();

typedef struct Closure {
	fun_ptr fun;
	int64_t args_cnt;
	int64_t applied_args_cnt;
	int64_t* applied_args;
}* ClosurePtr;
typedef struct Closure Closure;

typedef struct Node {
	ClosurePtr value;
	Node* next;
} Node;

static Node* create_node(ClosurePtr value) {
	Node* new_node = (Node*)malloc(sizeof(Node));
	new_node->value = value;
	new_node->next = NULL;
	return new_node;
}

static void add_closure(Node** head, ClosurePtr value) {
	Node* new_node = create_node(value);
	if (*head == NULL) {
		*head = new_node;
		return;
	}
	Node* current = *head;
	while (current->next != NULL) {
		current = current->next;
	}
	current->next = new_node;
}

static void remove_closure(Node** head, ClosurePtr target)
{
	while ((*head)->value != target) {
		head = &(*head)->next;
	}
	*head = (*head)->next;
}

static int64_t apply0(Closure* ptr) {
	return ptr->fun();
}

static int64_t apply1(Closure* ptr) {
	return ptr->fun(
		ptr->applied_args[0]
	);
}

static int64_t apply2(Closure* ptr) {
	return ptr->fun(
		ptr->applied_args[0],
		ptr->applied_args[1]
	);
}

static int64_t apply3(Closure* ptr) {
	return ptr->fun(
		ptr->applied_args[0],
		ptr->applied_args[1],
		ptr->applied_args[2]
	);
}

static int64_t apply4(Closure* ptr) {
	return ptr->fun(
		ptr->applied_args[0],
		ptr->applied_args[1],
		ptr->applied_args[2],
		ptr->applied_args[3]
	);
}

static int64_t apply5(Closure* ptr) {
	return ptr->fun(
		ptr->applied_args[0],
		ptr->applied_args[1],
		ptr->applied_args[2],
		ptr->applied_args[3],
		ptr->applied_args[4]
	);
}

static int64_t apply6(Closure* ptr) {
	return ptr->fun(
		ptr->applied_args[0],
		ptr->applied_args[1],
		ptr->applied_args[2],
		ptr->applied_args[3],
		ptr->applied_args[4],
		ptr->applied_args[5]
	);
}

static int64_t apply7(Closure* ptr) {
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

static int64_t apply8(Closure* ptr) {
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

static Node* closures_pool = NULL;
static int64_t(*callees[9])(Closure*) = {
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

extern int64_t alloc_closure(int64_t fun, int64_t args_cnt) {
	ClosurePtr new_closure = (ClosurePtr)malloc(sizeof(Closure));
	new_closure->fun = (fun_ptr)fun;
	new_closure->args_cnt = args_cnt;
	new_closure->applied_args_cnt = 0;
	new_closure->applied_args = (int64_t*)malloc(args_cnt * sizeof(int64_t));

	if (args_cnt == 0) {
		return callees[0](new_closure);
	}

	add_closure(&closures_pool, new_closure);
	return (int64_t)new_closure;
}

extern int64_t apply_closure(int64_t ptr, int64_t arg) {
	ClosurePtr closure = (ClosurePtr)ptr;
	closure->applied_args[closure->applied_args_cnt++] = arg;

	if (closure->applied_args_cnt != closure->args_cnt) {
		return ptr;
	}

	int64_t result = callees[closure->args_cnt](closure);
	remove_closure(&closures_pool, closure);
	free(closure->applied_args);
	free(closure);
	return result;
}

#include <stdio.h>
#include <stdint.h>
#include <malloc.h>
#include <stdlib.h>

// Application processing

typedef struct closure_struct
{
    int64_t (*func)();
    int64_t total_args;
    int64_t len_applied_args;
    int64_t *applied_args;
} closure;

static int64_t *closure_ptrs = NULL;
static int64_t stored_ptrs_len = 0;

static int64_t is_closure(int64_t ptr)
{
    for (int64_t i = 0; i < stored_ptrs_len; i++)
    {
        if (closure_ptrs[i] == ptr)
        {
            return 1;
        }
    }

    return 0;
}

static void realloc_ptrs_storage()
{
    int64_t *new_storage = (int64_t *)malloc((stored_ptrs_len + 64) * sizeof(int64_t));
    int64_t new_storage_index = 0;
    int64_t last_stored_ptrs_len = stored_ptrs_len;
    for (int64_t i = 0; i < last_stored_ptrs_len; i++)
    {
        if (closure_ptrs[i] == 0)
        {
            stored_ptrs_len--;
            continue;
        }

        new_storage[new_storage_index] = closure_ptrs[i];
        new_storage_index++;
    }

    free(closure_ptrs);

    closure_ptrs = new_storage;
}

static void add_to_closure_ptrs(int64_t ptr)
{
    if (stored_ptrs_len == 0)
    {
        closure_ptrs = (int64_t *)malloc(64 * sizeof(int64_t));
    }

    closure_ptrs[stored_ptrs_len] = ptr;
    stored_ptrs_len++;
    if (stored_ptrs_len % 64 == 0)
    {
        realloc_ptrs_storage();
    }
}

static void remove_from_closure_ptrs(int64_t ptr)
{
    for (int64_t i = 0; i < stored_ptrs_len; i++)
    {
        if (closure_ptrs[i] == ptr)
        {
            closure_ptrs[i] = 0;
        }
    }
}

extern int64_t peducoml_alloc_closure(int64_t ptr, int64_t total_args)
{
    if (is_closure(ptr) != 0)
    {
        closure *old_closure = (closure *)ptr;
        closure *new_closure = (closure *)malloc(sizeof(closure));
        new_closure->func = old_closure->func;
        new_closure->total_args = old_closure->total_args;
        new_closure->len_applied_args = old_closure->len_applied_args;
        new_closure->applied_args = (int64_t *)malloc((old_closure->total_args) * sizeof(int64_t));
        for (int64_t i = 0; i < old_closure->len_applied_args; i++)
        {
            new_closure->applied_args[i] = old_closure->applied_args[i];
        }
        add_to_closure_ptrs((int64_t)new_closure);

        return (int64_t)new_closure;
    }

    closure *closure_ptr = (closure *)malloc(sizeof(closure));
    closure_ptr->func = (int64_t(*)())ptr;
    closure_ptr->total_args = total_args;
    closure_ptr->len_applied_args = 0;
    closure_ptr->applied_args = (int64_t *)malloc(total_args * sizeof(int64_t));
    add_to_closure_ptrs((int64_t)closure_ptr);

    return (int64_t)closure_ptr;
}

extern int64_t peducoml_apply0(int64_t ptr)
{
    return ((int64_t(*)())ptr)();
}

static int64_t peducoml_apply1(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0]);
}

static int64_t peducoml_apply2(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1]);
}

static int64_t peducoml_apply3(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2]);
}

static int64_t peducoml_apply4(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3]);
}

static int64_t peducoml_apply5(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3],
        closure_ptr->applied_args[4]);
}

static int64_t peducoml_apply6(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3],
        closure_ptr->applied_args[4],
        closure_ptr->applied_args[5]);
}

static int64_t peducoml_apply7(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3],
        closure_ptr->applied_args[4],
        closure_ptr->applied_args[5],
        closure_ptr->applied_args[6]);
}

static int64_t peducoml_apply8(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3],
        closure_ptr->applied_args[4],
        closure_ptr->applied_args[5],
        closure_ptr->applied_args[6],
        closure_ptr->applied_args[7]);
}

extern int64_t peducoml_apply(int64_t ptr, int64_t arg)
{
    closure *closure_ptr = (closure *)ptr;
    closure_ptr->applied_args[closure_ptr->len_applied_args] = arg;
    closure_ptr->len_applied_args++;
    if (closure_ptr->len_applied_args == closure_ptr->total_args)
    {
        int64_t result = 0;
        switch (closure_ptr->total_args)
        {
        case 1:
            result = peducoml_apply1(closure_ptr);
            break;
        case 2:
            result = peducoml_apply2(closure_ptr);
            break;
        case 3:
            result = peducoml_apply3(closure_ptr);
            break;
        case 4:
            result = peducoml_apply4(closure_ptr);
            break;
        case 5:
            result = peducoml_apply5(closure_ptr);
            break;
        case 6:
            result = peducoml_apply6(closure_ptr);
            break;
        case 7:
            result = peducoml_apply7(closure_ptr);
            break;
        case 8:
            result = peducoml_apply8(closure_ptr);
            break;
        default:
            break;
        }

        free(closure_ptr->applied_args);
        free(closure_ptr);

        return result;
    }

    return ptr;
}

extern int64_t peducoml_divide(int64_t dividend, int64_t divider)
{
    if (divider == 0)
    {
        printf("Exception: devision by zero. Exited with 1\n");
        exit(1);
    }

    return dividend / divider;
}

// List processing

typedef struct node_struct
{
    int64_t data;
    struct node_struct *next;
} node;

extern int64_t peducoml_alloc_list()
{
    node *address = (node *)malloc(sizeof(node));
    address->data = 0; // The first element of the list is its length. It is 0 when creating the list
    address->next = NULL;
    return (int64_t)address;
}

extern int64_t peducoml_add_to_list(int64_t list_ptr, int64_t data)
{
    node *head = (node *)list_ptr;
    node *new_elem = (node *)malloc(sizeof(node));
    node *new_head = (node *)malloc(sizeof(node));
    new_elem->data = data;
    new_elem->next = head->next;
    new_head->data = head->data + 1; // Since we add the element, the list's length is increased by 1
    new_head->next = new_elem;
    return (int64_t)(new_head);
}

extern int64_t peducoml_list_field(int64_t list_ptr, int64_t index)
{
    node *head = (node *)list_ptr;
    node *current = head->next;
    for (int64_t i = 0; i < index; i++)
    {
        current = current->next;
    }
    return current->data;
}

extern int64_t peducoml_tail(int64_t list_ptr)
{
    node *head = (node *)list_ptr;
    node *new_elem = (node *)malloc(sizeof(node));
    new_elem->data = head->data - 1;
    new_elem->next = head->next->next;
    return (int64_t)new_elem;
}

extern int64_t peducoml_list_length(int64_t list_ptr)
{
    node *head = (node *)list_ptr;
    return head->data;
}

static int64_t peducoml_compare_lists(int64_t list1_ptr, int64_t list2_ptr)
{
    // -1 <=> list1 > list2
    //  0 <=> list1 = list2
    //  1 <=> list1 < list2
    node *head1 = (node *)list1_ptr;
    node *head2 = (node *)list2_ptr;
    int64_t length1 = head1->data;
    int64_t length2 = head2->data;
    int64_t min_length = 0;
    if (length1 <= length2)
        min_length = length1;
    else
        min_length = length2;
    node *current1 = head1->next;
    node *current2 = head2->next;
    for (int64_t i = 0; i < min_length; i++)
    {
        if (current1->data < current2->data)
            return 1;
        if (current1->data > current2->data)
            return -1;
        current1 = current1->next;
        current2 = current2->next;
    }

    if (length1 < length2)
        return 1;

    return 0;
}

extern int64_t compare_lists_eq(int64_t list1_ptr, int64_t list2_ptr)
{
    if (peducoml_compare_lists(list1_ptr, list2_ptr) == 0)
    {
        return 1;
    }
    return 0;
}

extern int64_t compare_lists_neq(int64_t list1_ptr, int64_t list2_ptr)
{
    return 1 - compare_lists_eq(list1_ptr, list2_ptr);
}

extern int64_t compare_lists_gt(int64_t list1_ptr, int64_t list2_ptr)
{
    if (peducoml_compare_lists(list1_ptr, list2_ptr) == -1)
    {
        return 1;
    }
    return 0;
}

extern int64_t compare_lists_gte(int64_t list1_ptr, int64_t list2_ptr)
{
    if (peducoml_compare_lists(list1_ptr, list2_ptr) <= 0)
    {
        return 1;
    }
    return 0;
}

extern int64_t compare_lists_lt(int64_t list1_ptr, int64_t list2_ptr)
{
    return 1 - compare_lists_gte(list1_ptr, list2_ptr);
}

extern int64_t compare_lists_lte(int64_t list1_ptr, int64_t list2_ptr)
{
    return 1 - compare_lists_gt(list1_ptr, list2_ptr);
}

extern int64_t print_list(int64_t list_ptr)
{
    node *head = (node *)list_ptr;
    int64_t length = head->data;
    printf("[");
    if (length == 0)
    {
        printf("]");
        return 0;
    }
    node *current = head->next;
    for (int64_t i = 0; i < head->data - 1; i++)
    {
        printf("%ld; ", current->data);
        current = current->next;
    }
    printf("%ld]", current->data);
    return 0;
}

extern int64_t concat_lists(int64_t list1_ptr, int64_t list2_ptr)
{
    node *head1 = (node *)list1_ptr;
    node *head2 = (node *)list2_ptr;
    int64_t length1 = head1->data;
    int64_t length2 = head2->data;
    node *result = (node *)malloc(sizeof(node));
    result->data = 0;
    node *current_result = result;
    node *current = head1->next;
    for (int64_t i = 0; i < length1; i++)
    {
        node *new_node = (node *)malloc(sizeof(node));
        new_node->data = current->data;
        current_result->next = new_node;
        current_result = new_node;
        current = current->next;
        result->data++;
    }
    current = head2->next;
    for (int64_t i = 0; i < length2; i++)
    {
        node *new_node = (node *)malloc(sizeof(node));
        new_node->data = current->data;
        current_result->next = new_node;
        current_result = new_node;
        current = current->next;
        result->data++;
    }
    return (int64_t)result;
}

// Tuple processing

extern int64_t peducoml_alloc_tuple(int64_t cardinality)
{
    int64_t *tuple_ptr = (int64_t *)calloc((cardinality + 1), sizeof(int64_t));
    return (int64_t)tuple_ptr;
}

extern int64_t peducoml_fill_tuple(int64_t ptr, int64_t elem)
{
    int64_t *tuple_ptr = (int64_t *)ptr;
    tuple_ptr[0]++;
    tuple_ptr[tuple_ptr[0]] = elem;
    return ptr;
}

extern int64_t peducoml_tuple_field(int64_t ptr, int64_t index)
{
    int64_t *tuple_ptr = (int64_t *)ptr;
    return tuple_ptr[index + 1];
}

static int64_t peducoml_compare_tuples(int64_t ptr1, int64_t ptr2)
{
    // -1 <=> tuple1 > tuple2
    //  0 <=> tuple1 = tuple2
    //  1 <=> tuple1 < tuple2
    int64_t *tuple1_ptr = (int64_t *)ptr1;
    int64_t *tuple2_ptr = (int64_t *)ptr2;
    int64_t length = tuple1_ptr[0];
    for (int64_t i = 1; i <= length; i++)
    {
        if (tuple1_ptr[i] < tuple2_ptr[i])
            return 1;
        if (tuple1_ptr[i] > tuple2_ptr[i])
            return -1;
    }

    return 0;
}

extern int64_t compare_tuples_eq(int64_t ptr1, int64_t ptr2)
{
    if (peducoml_compare_tuples(ptr1, ptr2) == 0)
    {
        return 1;
    }
    return 0;
}

extern int64_t compare_tuples_neq(int64_t ptr1, int64_t ptr2)
{
    return 1 - compare_tuples_eq(ptr1, ptr2);
}

extern int64_t compare_tuples_gt(int64_t ptr1, int64_t ptr2)
{
    if (peducoml_compare_tuples(ptr1, ptr2) == -1)
    {
        return 1;
    }
    return 0;
}

extern int64_t compare_tuples_gte(int64_t ptr1, int64_t ptr2)
{
    if (peducoml_compare_tuples(ptr1, ptr2) <= 0)
    {
        return 1;
    }
    return 0;
}

extern int64_t compare_tuples_lt(int64_t ptr1, int64_t ptr2)
{
    return 1 - compare_tuples_gte(ptr1, ptr2);
}

extern int64_t compare_tuples_lte(int64_t ptr1, int64_t ptr2)
{
    return 1 - compare_tuples_gt(ptr1, ptr2);
}

extern int64_t print_tuple(int64_t ptr)
{
    int64_t *tuple_ptr = (int64_t *)ptr;
    int64_t length = tuple_ptr[0];
    printf("(");
    for (int64_t i = 1; i < length; i++)
    {
        printf("%ld, ", tuple_ptr[i]);
    }
    printf("%ld)", tuple_ptr[length]);
    return 0;
}

// String processing

extern int64_t print_char(int64_t c)
{
    putchar(c);
    return 0;
}

extern int64_t print_string(int64_t string_ptr)
{
    node *string = (node *)string_ptr;
    int64_t length = string->data;
    node *current = string->next;
    for (int64_t i = 0; i < length; i++)
    {
        print_char(current->data);
        current = current->next;
    }
    return 0;
}

extern int64_t concat_strings(int64_t string1_ptr, int64_t string2_ptr)
{
    return concat_lists(string1_ptr, string2_ptr);
}

extern int64_t compare_strings_eq(int64_t string1_ptr, int64_t string2_ptr)
{
    return compare_lists_eq(string1_ptr, string2_ptr);
}

extern int64_t compare_strings_neq(int64_t string1_ptr, int64_t string2_ptr)
{
    return compare_lists_neq(string1_ptr, string2_ptr);
}

extern int64_t compare_strings_gt(int64_t string1_ptr, int64_t string2_ptr)
{
    return compare_lists_gt(string1_ptr, string2_ptr);
}

extern int64_t compare_strings_gte(int64_t string1_ptr, int64_t string2_ptr)
{
    return compare_lists_gte(string1_ptr, string2_ptr);
}

extern int64_t compare_strings_lt(int64_t string1_ptr, int64_t string2_ptr)
{
    return compare_lists_lt(string1_ptr, string2_ptr);
}

extern int64_t compare_strings_lte(int64_t string1_ptr, int64_t string2_ptr)
{
    return compare_lists_lte(string1_ptr, string2_ptr);
}

// Other Stdlib functions

extern int64_t print_int(int64_t x)
{
    printf("%ld", x);
    return 0;
}

extern int64_t print_bool(int64_t b)
{
    if (b > 0)
    {
        printf("true");
    }
    else
    {
        printf("false");
    }
    return 0;
}

extern int64_t print_new_line()
{
    printf("\n");
    return 0;
}
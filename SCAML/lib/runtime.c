#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct PartAppl
{
    int64_t *fn;
    int tcount;    // Total number of arguments
    int acount;    // Number of arguments applied
    int64_t *args; // Stored arguments
} *PAply;          // Читать как Папли :D

// Functoins invocation

static int64_t invoke0(PAply p)
{
    return ((int64_t(*)())p->fn)();
}

static int64_t invoke1(PAply p)
{
    return ((int64_t(*)(int64_t))p->fn)(p->args[0]);
}

static int64_t invoke2(PAply p)
{
    return ((int64_t(*)(int64_t, int64_t))p->fn)(p->args[0], p->args[1]);
}

static int64_t invoke3(PAply p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2]);
}

static int64_t invoke4(PAply p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3]);
}

static int64_t invoke5(PAply p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4]);
}

static int64_t invoke6(PAply p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5]);
}

static int64_t invoke7(PAply p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6]);
}

static int64_t invoke8(PAply p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7]);
}

static int64_t invoke9(PAply p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8]);
}

static int64_t invoke10(PAply p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9]);
}

static int64_t (*invokers[11])(PAply) = {
    invoke0,
    invoke1,
    invoke2,
    invoke3,
    invoke4,
    invoke5,
    invoke6,
    invoke7,
    invoke8,
    invoke9,
    invoke10};

PAply newPaply(int64_t *fn, int tcount)
{
    PAply p = (PAply)malloc(sizeof(struct PartAppl));
    p->fn = fn;
    p->tcount = tcount;
    p->acount = 0;
    p->args = (int64_t *)malloc(sizeof(int64_t) * tcount);
    return p;
}

PAply fromPaply(PAply p)
{
    PAply p2 = (PAply)malloc(sizeof(struct PartAppl));
    p2->fn = p->fn;
    p2->tcount = p->tcount;
    p2->acount = p->acount;
    p2->args = (int64_t *)malloc(sizeof(int64_t) * p->tcount);
    for (int i = 0; i < p->acount; i++)
    {
        p2->args[i] = p->args[i];
    }
    return p2;
}

PAply applyPaply(PAply p, int64_t arg)
{
    if (p->acount < p->tcount)
    {
        p->args[p->acount] = arg;
        p->acount++;
    }
    else
    {
        if (p->acount == p->tcount)
        {
            int64_t (*fn)(PAply) = invokers[p->tcount];
            return fn(p);
        }
        else
        {
            printf("Error: too many arguments applied\n");
            exit(1);
        }
    }
}
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct PartAppl
{
    int64_t *fn;
    int tcount;    // Total number of arguments
    int acount;    // Number of arguments applied
    int64_t *args; // Stored arguments
} *PAppli;          // Читать как Папли :D

// Functoins invocation

static int64_t invoke0(PAppli p)
{
    return ((int64_t(*)())p->fn)();
}

static int64_t invoke1(PAppli p)
{
    return ((int64_t(*)(int64_t))p->fn)(p->args[0]);
}

static int64_t invoke2(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t))p->fn)(p->args[0], p->args[1]);
}

static int64_t invoke3(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2]);
}

static int64_t invoke4(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3]);
}

static int64_t invoke5(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4]);
}

static int64_t invoke6(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5]);
}

static int64_t invoke7(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6]);
}

static int64_t invoke8(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7]);
}

static int64_t invoke9(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8]);
}

static int64_t invoke10(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9]);
}

static int64_t invoke11(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10]);
}

static int64_t invoke12(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11]);
}

static int64_t invoke13(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12]);
}

static int64_t invoke14(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13]);
}

static int64_t invoke15(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14]);
}

static int64_t invoke16(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15]);
}

static int64_t invoke17(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16]);
}

static int64_t invoke18(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17]);
}

static int64_t invoke19(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18]);
}

static int64_t invoke20(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19]);
}

static int64_t invoke21(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20]);
}

static int64_t invoke22(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21]);
}

static int64_t invoke23(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22]);
}

static int64_t invoke24(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23]);
}

static int64_t invoke25(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24]);
}

static int64_t invoke26(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25]);
}

static int64_t invoke27(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26]);
}

static int64_t invoke28(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27]);
}

static int64_t invoke29(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28]);
}

static int64_t invoke30(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29]);
}

static int64_t invoke31(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30]);
}

static int64_t invoke32(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31]);
}

static int64_t invoke33(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32]);
}

static int64_t invoke34(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33]);
}

static int64_t invoke35(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34]);
}

static int64_t invoke36(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35]);
}

static int64_t invoke37(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36]);
}

static int64_t invoke38(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37]);
}

static int64_t invoke39(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38]);
}

static int64_t invoke40(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39]);
}

static int64_t invoke41(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40]);
}

static int64_t invoke42(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41]);
}

static int64_t invoke43(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42]);
}

static int64_t invoke44(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43]);
}

static int64_t invoke45(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44]);
}

static int64_t invoke46(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45]);
}

static int64_t invoke47(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46]);
}

static int64_t invoke48(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47]);
}

static int64_t invoke49(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48]);
}

static int64_t invoke50(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49]);
}

static int64_t invoke51(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50]);
}

static int64_t invoke52(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51]);
}

static int64_t invoke53(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52]);
}

static int64_t invoke54(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53]);
}

static int64_t invoke55(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54]);
}

static int64_t invoke56(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55]);
}

static int64_t invoke57(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56]);
}

static int64_t invoke58(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57]);
}

static int64_t invoke59(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58]);
}

static int64_t invoke60(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59]);
}

static int64_t invoke61(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60]);
}

static int64_t invoke62(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61]);
}

static int64_t invoke63(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62]);
}

static int64_t invoke64(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63]);
}

static int64_t invoke65(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64]);
}

static int64_t invoke66(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65]);
}

static int64_t invoke67(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66]);
}

static int64_t invoke68(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67]);
}

static int64_t invoke69(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68]);
}

static int64_t invoke70(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69]);
}

static int64_t invoke71(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70]);
}

static int64_t invoke72(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71]);
}

static int64_t invoke73(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72]);
}

static int64_t invoke74(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73]);
}

static int64_t invoke75(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74]);
}

static int64_t invoke76(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75]);
}

static int64_t invoke77(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76]);
}

static int64_t invoke78(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77]);
}

static int64_t invoke79(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78]);
}

static int64_t invoke80(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79]);
}

static int64_t invoke81(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80]);
}

static int64_t invoke82(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81]);
}

static int64_t invoke83(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82]);
}

static int64_t invoke84(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83]);
}

static int64_t invoke85(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84]);
}

static int64_t invoke86(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85]);
}

static int64_t invoke87(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86]);
}

static int64_t invoke88(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86], p->args[87]);
}

static int64_t invoke89(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86], p->args[87], p->args[88]);
}

static int64_t invoke90(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86], p->args[87], p->args[88], p->args[89]);
}

static int64_t invoke91(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86], p->args[87], p->args[88], p->args[89], p->args[90]);
}

static int64_t invoke92(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86], p->args[87], p->args[88], p->args[89], p->args[90], p->args[91]);
}

static int64_t invoke93(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86], p->args[87], p->args[88], p->args[89], p->args[90], p->args[91], p->args[92]);
}

static int64_t invoke94(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86], p->args[87], p->args[88], p->args[89], p->args[90], p->args[91], p->args[92], p->args[93]);
}

static int64_t invoke95(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86], p->args[87], p->args[88], p->args[89], p->args[90], p->args[91], p->args[92], p->args[93], p->args[94]);
}

static int64_t invoke96(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86], p->args[87], p->args[88], p->args[89], p->args[90], p->args[91], p->args[92], p->args[93], p->args[94], p->args[95]);
}

static int64_t invoke97(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86], p->args[87], p->args[88], p->args[89], p->args[90], p->args[91], p->args[92], p->args[93], p->args[94], p->args[95], p->args[96]);
}

static int64_t invoke98(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86], p->args[87], p->args[88], p->args[89], p->args[90], p->args[91], p->args[92], p->args[93], p->args[94], p->args[95], p->args[96], p->args[97]);
}

static int64_t invoke99(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86], p->args[87], p->args[88], p->args[89], p->args[90], p->args[91], p->args[92], p->args[93], p->args[94], p->args[95], p->args[96], p->args[97], p->args[98]);
}

static int64_t invoke100(PAppli p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fn)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15], p->args[16], p->args[17], p->args[18], p->args[19], p->args[20], p->args[21], p->args[22], p->args[23], p->args[24], p->args[25], p->args[26], p->args[27], p->args[28], p->args[29], p->args[30], p->args[31], p->args[32], p->args[33], p->args[34], p->args[35], p->args[36], p->args[37], p->args[38], p->args[39], p->args[40], p->args[41], p->args[42], p->args[43], p->args[44], p->args[45], p->args[46], p->args[47], p->args[48], p->args[49], p->args[50], p->args[51], p->args[52], p->args[53], p->args[54], p->args[55], p->args[56], p->args[57], p->args[58], p->args[59], p->args[60], p->args[61], p->args[62], p->args[63], p->args[64], p->args[65], p->args[66], p->args[67], p->args[68], p->args[69], p->args[70], p->args[71], p->args[72], p->args[73], p->args[74], p->args[75], p->args[76], p->args[77], p->args[78], p->args[79], p->args[80], p->args[81], p->args[82], p->args[83], p->args[84], p->args[85], p->args[86], p->args[87], p->args[88], p->args[89], p->args[90], p->args[91], p->args[92], p->args[93], p->args[94], p->args[95], p->args[96], p->args[97], p->args[98], p->args[99]);
}

static int64_t (*invokers[101])(PAppli) = {
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
    invoke10,
    invoke11,
    invoke12,
    invoke13,
    invoke14,
    invoke15,
    invoke16,
    invoke17,
    invoke18,
    invoke19,
    invoke20,
    invoke21,
    invoke22,
    invoke23,
    invoke24,
    invoke25,
    invoke26,
    invoke27,
    invoke28,
    invoke29,
    invoke30,
    invoke31,
    invoke32,
    invoke33,
    invoke34,
    invoke35,
    invoke36,
    invoke37,
    invoke38,
    invoke39,
    invoke40,
    invoke41,
    invoke42,
    invoke43,
    invoke44,
    invoke45,
    invoke46,
    invoke47,
    invoke48,
    invoke49,
    invoke50,
    invoke51,
    invoke52,
    invoke53,
    invoke54,
    invoke55,
    invoke56,
    invoke57,
    invoke58,
    invoke59,
    invoke60,
    invoke61,
    invoke62,
    invoke63,
    invoke64,
    invoke65,
    invoke66,
    invoke67,
    invoke68,
    invoke69,
    invoke70,
    invoke71,
    invoke72,
    invoke73,
    invoke74,
    invoke75,
    invoke76,
    invoke77,
    invoke78,
    invoke79,
    invoke80,
    invoke81,
    invoke82,
    invoke83,
    invoke84,
    invoke85,
    invoke86,
    invoke87,
    invoke88,
    invoke89,
    invoke90,
    invoke91,
    invoke92,
    invoke93,
    invoke94,
    invoke95,
    invoke96,
    invoke97,
    invoke98,
    invoke99,
    invoke100,
};


PAppli newPAppli(int64_t *fn, int tcount)
{
    PAppli p = (PAppli)malloc(sizeof(struct PartAppl));
    p->fn = fn;
    p->tcount = tcount;
    p->acount = 0;
    p->args = (int64_t *)malloc(sizeof(int64_t) * tcount);
    return p;
}

PAppli fromPAppli(PAppli p)
{
    PAppli p2 = (PAppli)malloc(sizeof(struct PartAppl));
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

int64_t applyPAppli(int64_t pointer, int64_t arg)
{
    PAppli p = (PAppli)pointer;
    // Create new partitial application structure for every application
    PAppli new = fromPAppli(p);

    new->args[p->acount] = arg;
    new->acount++;
    if (new->acount == new->tcount)
    {
        int64_t (*fn)(PAppli) = invokers[new->tcount];
        int64_t res = fn(new);
        free(new->args);
        free(new);
        return res;
    }
    else
    {
        if (new->acount > new->tcount)
        {
            printf("Error: too many arguments applied\n");
            exit(1);
        }
        
    }
    return (int64_t)new;
}

int64_t addNewPAppliClosure(int64_t func, int64_t tcount)
{
    int64_t* fn = (int64_t*)func;
    PAppli new = NULL;
    if (tcount > 100)
    {
        printf("Error: сannot create a function with more than 100 arguments");
        exit(1);
    }
    new = newPAppli(fn, tcount);
    // Yes, it ends up being a memory leak but I don't know of a way to detect at runtime which closures are no longer needed
    if (tcount == 0){
        int64_t (*fn)(PAppli) = invokers[new->tcount];
        int64_t res = fn(new);
        free(new->args);
        free(new);
        return res;
    }
    return (int64_t) new;
}

int64_t print_int(int64_t x)
{
    printf("%ld\n", x);
    return 0;
}

int64_t print_bool(int64_t x)
{
    if (x == 0)
    {
        printf("false\n");
    }
    else
    {
        printf("true\n");
    }
    return 0;
}
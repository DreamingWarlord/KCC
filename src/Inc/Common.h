#pragma once

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>


#define TRUE ((bool) 1)
#define FALSE ((bool) 0)

#define Assert(expr) if(!(expr)) BugOn(#expr, __FILE__, __LINE__)


typedef int8_t int8;
typedef int16_t int16;
typedef int32_t int32;
typedef int64_t int64;
typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef uint64_t uint64;
typedef uint32_t bool;


void BugOn(char *expr_str, char *file, int line);

void *Alloc(uint64 size);

void *AllocReduced(uint64 size);

void Reduce(uint64 new_size);

uint64 Hash(void *str, uint64 length);

char *Reverse(char *str);

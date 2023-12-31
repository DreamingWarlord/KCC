#pragma once

#include "Common.h"


#define FUNC_MAX_ARGC 8

#define KIND_VOID    0
#define KIND_INT8    1
#define KIND_INT16   2
#define KIND_INT32   3
#define KIND_INT64   4
#define KIND_STRUCT  5
#define KIND_FUNC    6


struct Type
{
	uint64 kind :  3;
	uint64 ptrc :  5;
	uint64 idx  : 56;
};

struct Member
{
	struct Type type;
	char       *name;
	uint64    offset;
	uint64     count; // if this is 0, it's a FAM
};

struct Struct
{
	char            *name;
	struct Struct   *next;
	uint64           size;
	uint64          count; // if this is 0, it's an incomplete struct
	struct Member members[];
};

struct Function
{
	struct Function *next;
	struct Type      type;
	uint64          count;
	bool         variadic;
	struct Type      args[FUNC_MAX_ARGC];
};


uint64 StructInsert(struct Struct *struc);

uint64 StructFind(char *name);

struct Struct *StructGet(uint64 idx);

uint64 FunctionFind(struct Function *func_template);

struct Function *FunctionGet(uint64 idx);

uint64 TypeSize(struct Type type);

char *TypeStr(struct Type type);

bool TypeCmp(struct Type a, struct Type b);

bool TypeCastable(struct Type a, struct Type b);

bool TypeInt(struct Type type);

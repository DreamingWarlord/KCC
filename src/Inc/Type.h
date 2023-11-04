#pragma once

#include "Common.h"


#define KIND_VOID    0
#define KIND_UINT8   1
#define KIND_UINT16  2
#define KIND_UINT32  3
#define KIND_UINT64  4
#define KIND_INT8    5
#define KIND_INT16   6
#define KIND_INT32   7
#define KIND_INT64   8
#define KIND_STRUCT  9
#define KIND_FUNC   10


struct Type
{
	uint64 kind :  4;
	uint64 ptrc :  4;
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
	struct Type      args[];
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

bool TypeSigned(struct Type type);

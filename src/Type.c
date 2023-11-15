#include "Type.h"


#define TABLE_SIZE 2048


static const char *type_name_table[] = {
	"void", "int8", "int16", "int32", "int64", "struct"
};
static struct Struct *struct_table[TABLE_SIZE] = { NULL };
static struct Function *func_table[TABLE_SIZE] = { NULL };


static bool FuncCmp(struct Function *a, struct Function *b)
{
	if(a->count != b->count)
		return FALSE;

	return !memcmp(&a->type, &b->type, sizeof(struct Function) - 8);
}


uint64 StructInsert(struct Struct *struc)
{
	uint64 hash = Hash(struc->name, strlen(struc->name)) % TABLE_SIZE;

	if(struct_table[hash] == NULL) {
		struct_table[hash] = struc;
		return hash << 32;
	}

	struct Struct *cur = struct_table[hash];
	uint64 list_idx = 1;

	do {
		if(!strcmp(cur->name, struc->name))
			return -1ULL;

		if(cur->next != NULL) {
			cur = cur->next;
			list_idx++;
			continue;
		}

		cur->next = struc;
		return (hash << 32) + list_idx;
	} while(1);
}

uint64 StructFind(char *name)
{
	uint64 hash = Hash(name, strlen(name)) % TABLE_SIZE;
	struct Struct *cur = struct_table[hash];
	uint64 idx = 0;

	while(cur != NULL) {
		if(!strcmp(cur->name, name))
			return (hash << 32) + idx;

		cur = cur->next;
		idx++;
	}

	return -1ULL;
}

struct Struct *StructGet(uint64 idx)
{
	if(idx == -1ULL)
		return NULL;

	uint64 tab_idx = idx >> 32;
	uint64 list_idx = idx & 0xFFFFFFFF;
	struct Struct *cur = struct_table[tab_idx];

	for(uint64 i = 0; i < list_idx; i++) {
		if(cur == NULL)
			return NULL;

		cur = cur->next;
	}

	return cur;
}

uint64 FunctionFind(struct Function *funct)
{
	uint64 hash = Hash(&funct->type, sizeof(struct Function) - 8) % TABLE_SIZE;
	struct Function *cur = func_table[hash];
	uint64 idx = 0;

	while(cur != NULL) {
		if(FuncCmp(cur, funct))
			return (hash << 32) + idx;

		if(cur->next == NULL)
			break;

		cur = cur->next;
		idx++;
	}

	struct Function *func = Alloc(sizeof(struct Function));
	*func = *funct;

	if(cur == NULL) {
		func_table[hash] = func;
		return hash << 32;
	}

	cur->next = func;
	return (hash << 32) + idx + 1;
}

struct Function *FunctionGet(uint64 idx)
{
	if(idx == -1ULL)
		return NULL;

	uint64 tab_idx = idx >> 32;
	uint64 list_idx = idx & 0xFFFFFFFF;
	struct Function *cur = func_table[tab_idx];

	for(uint64 i = 0; i < list_idx; i++) {
		if(cur == NULL)
			return NULL;

		cur = cur->next;
	}

	return cur;
}

uint64 TypeSize(struct Type type)
{
	if(type.ptrc > 0)
		return 8;

	switch(type.kind)
	{
	case KIND_VOID:   return 0;
	case KIND_INT8:   return 1;
	case KIND_INT16:  return 2;
	case KIND_INT32:  return 4;
	case KIND_INT64:  return 8;
	}

	Assert(type.kind != KIND_FUNC);
	return StructGet(type.idx)->size;
}

char *TypeStr(struct Type type)
{
	char *str = Alloc(512);

	if(type.kind == KIND_FUNC) {
		struct Function *func = FunctionGet(type.idx);
		char *rstr = TypeStr(func->type);
		sprintf(str, "%s @(", rstr);

		for(uint64 i = 0; i < func->count; i++) {
			rstr = TypeStr(func->args[i]);
			strcat(str, rstr);

			if(i != func->count - 1 || (i == func->count - 1 && func->variadic))
				strcat(str, ", ");
		}

		if(func->variadic)
			strcat(str, "...)");
		else
			strcat(str, ")");
	} else if(type.kind == KIND_STRUCT) {
		struct Struct *struc = StructGet(type.idx);
		sprintf(str, "struct %s", struc->name);
	} else {
		sprintf(str, "%s", type_name_table[type.kind]);
	}

	if(type.ptrc > 0) {
		strcat(str, " ");

		for(uint64 i = 0; i < type.ptrc; i++)
			strcat(str, "*");
	}

	return str;
}

bool TypeCmp(struct Type a, struct Type b)
{
	return !memcmp(&a, &b, sizeof(struct Type));
}

bool TypeCastable(struct Type a, struct Type b)
{
	if(TypeCmp(a, b))
		return TRUE;

	if(a.ptrc > 0 && b.ptrc > 0)
		return TRUE;

	if(TypeInt(a) && TypeInt(b))
		return TRUE;

	if(a.ptrc > 0 && b.ptrc == 0 && b.kind == KIND_INT64)
		return TRUE;

	if(b.ptrc > 0 && a.ptrc == 0 && a.kind == KIND_INT64)
		return TRUE;

	return FALSE;
}

bool TypeInt(struct Type type)
{
	return type.ptrc == 0 && (type.kind >= KIND_INT8 && type.kind <= KIND_INT64);
}

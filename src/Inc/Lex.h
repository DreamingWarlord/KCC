#pragma once

#include "Common.h"
#include "Hashes.h"


#define TK_EOF   0
#define TK_INT   1
#define TK_IDENT 2
#define TK_KWORD 3
#define TK_STR   4


struct Token
{
	uint64  tok;
	uint64  num;
	uint64 hash;
	char   *str;
	char  *file;
	uint64 line;
	uint64  col;
};


uint64 Lex(struct Token *tok);

void LexString(char *name, char *str);

bool LexFile(char *file_name);

void TokenError(struct Token *tok, char *fmt, ...);

char *TokenStr(struct Token *tok);

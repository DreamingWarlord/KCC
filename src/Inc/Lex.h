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
	uint64  tok; // Token kind
	uint64  num; // Integer constant / string length
	uint64 hash; // String hash
	char   *str; // String
	char  *file; // Token file name
	uint64 line; // Token line
	uint64  col; // Token column
};


uint64 Lex(struct Token *tok);

void LexString(char *name, char *str);

bool LexFile(char *file_name);

void TokenError(struct Token *tok, char *fmt, ...);

char *TokenStr(struct Token *tok);

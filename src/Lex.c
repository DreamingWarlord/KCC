#include "Lex.h"


#define MAX_LEX_FILES 2048


struct Source
{
	char  *name;
	char  *text;
	uint64  pos;
	uint64 line;
	uint64  col;

	struct Source *next;
};


static struct Source *src = NULL;
static uint64 keyword_hashes[] = { KW_IF, KW_ELSE, KW_FOR, KW_WHILE, KW_DO, KW_GOTO, KW_BREAK, KW_CONTINUE, KW_RETURN, KW_SIZEOF, KW_CONTAINEROF, KW_STRUCT, KW_UNION, KW_SWITCH, KW_INLINE, KW_STATIC, KW_BOOL, KW_INT8, KW_INT16, KW_INT32, KW_INT64, KW_VOID, KW_CONST, KW_INCLUDE, KW_ENUM };
static uint64 lexed_files[MAX_LEX_FILES] = { 0 };
static uint64 lexed_file_count = 0;


static bool LexEOF()
{
	Assert(src != NULL);

	if(src->pos == -1ULL)
		return FALSE;

	return src->text[src->pos] == '\0' && src->next == NULL;
}

static char LexCh()
{
	Assert(src != NULL);

	if(LexEOF())
		return '\0';

	char ch = src->text[++src->pos];

	if(ch == '\0' && src->next != NULL) {
		src = src->next;
		return LexCh();
	}

	src->col++;
	return ch;
}

static void LexUnCh()
{
	src->col--;
	src->pos--;
}

static char LexCurCh()
{
	Assert(src != NULL);
	Assert(src->pos != -1ULL);
	return src->text[src->pos];
}

static void LexNL()
{
	Assert(src != NULL);
	src->line++;
	src->col = 0;
};

static void LexError(char *fmt, ...)
{
	Assert(src != NULL);
	printf("\x1B[1m%s:%lu:%lu:\x1B[31m ", src->name, src->line, src->col);
	va_list ap;
	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);
	printf("\x1B[0m\n");
	exit(1);
}

static uint8 HexCh(char ch)
{
	ch = tolower(ch);
	return ch >= 'a' ? ch - 'a' + 10 : ch - '0';
}

static char LexStrCh()
{
	char ch = LexCurCh();
	char ch2 = '\0';

	if(ch != '\\')
		return ch;

	ch = LexCh();

	switch(ch)
	{
	case '0': return '\0';
	case 'a': return '\a';
	case 'b': return '\b';
	case 't': return '\t';
	case 'n': return '\n';
	case 'v': return '\v';
	case 'f': return '\f';
	case 'r': return '\r';
	case 'x':
		ch = LexCh();

		if(!isxdigit(ch))
			LexError("Escaped hexadecimal constant in string contains invalid digit");

		ch2 = LexCh();

		if(!isxdigit(ch2))
			LexError("Escaped hexadecimal constant in string contains invalid digit");

		return (HexCh(ch) << 4) | HexCh(ch2);
	default:
		return ch;
	}

	return '\0';
}


void TokenError(struct Token *tok, char *fmt, ...)
{
	printf("\x1B[1m%s:%lu:%lu:\x1B[31m ", tok->file, tok->line, tok->col);
	va_list ap;
	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);
	printf("\x1B[0m\n");
	exit(1);
}

uint64 Lex(struct Token *tok)
{
	Assert(src != NULL);
	*tok = (struct Token) { 0 };

	while(isspace(LexCh())) {
		if(LexCurCh() == '\n')
			LexNL();
	}

	tok->file = src->name;
	tok->line = src->line;
	tok->col = src->col;

	switch(LexCurCh())
	{
	case '\0':
		if(src->next != NULL) {
			src = src->next;
			return Lex(tok);
		}

		tok->tok = TK_EOF;
		return TK_EOF;
	case '0':
		tok->tok = TK_INT;
		LexCh();

		if(LexCurCh() == 'b') {
			tok->hash = 2;
			uint64 num_bits = 0;
			LexCh();

			while(LexCurCh() == '0' || LexCurCh() == '1' || LexCurCh() == '_') {
				if(LexCurCh() == '_') {
					LexCh();
					continue;
				}

				tok->num <<= 1;
				tok->num |= LexCurCh() - '0';
				LexCh();
				num_bits++;
			}

			if(num_bits > 64)
				LexError("Binary number contains more than 64 bits");

			if(isalpha(LexCurCh()) || LexCurCh() == '_')
				LexError("Invalid character in the middle of binary number");

			LexUnCh();

			if(LexCurCh() == '_')
				LexError("Invalid character in the middle of binary number");

			return TK_INT;
		} else if(LexCurCh() == 'x') {
			tok->hash = 16;
			uint64 num_bits = 0;
			LexCh();

			while(isxdigit(LexCurCh()) || LexCurCh() == '_') {
				if(LexCurCh() == '_') {
					LexCh();
					continue;
				}

				tok->num <<= 4;
				tok->num |= HexCh(LexCurCh());
				LexCh();
				num_bits += 4;
			}

			if(num_bits > 64)
				LexError("Hexadecimal number contains more than 64 bits");

			if(isalpha(LexCurCh()))
				LexError("Invalid character in the middle of hexadecimal number");

			LexUnCh();

			if(LexCurCh() == '_')
				LexError("Invalid character in the middle of hexadecimal number");

			return TK_INT;
		}

		LexUnCh();
	case '1'...'9':
		tok->tok = TK_INT;
		tok->hash = 10;

		while(isdigit(LexCurCh()) || LexCurCh() == '_') {
			if(LexCurCh() == '_') {
				LexCh();
				continue;
			}

			uint64 old = tok->num;
			tok->num *= 10;
			tok->num += LexCurCh() - '0';

			if(tok->num < old)
				LexError("Decimal number bigger than 64 bits");

			LexCh();
		}

		if(isalpha(LexCurCh()))
			LexError("Invalid character in the middle of decimal number");

		LexUnCh();

		if(LexCurCh() == '_')
			LexError("Invalid character in the middle of decimal number");

		return TK_INT;
	case '_':
	case 'a'...'z':
	case 'A'...'Z':
		tok->tok = TK_IDENT;
		tok->str = AllocReduced(256);

		while(isalnum(LexCurCh()) || LexCurCh() == '_') {
			if(tok->num == 255)
				LexError("Identifier too long (more than 255 characters)");

			tok->str[tok->num++] = LexCurCh();
			LexCh();
		}

		Reduce(tok->num + 1);
		tok->str[tok->num] = '\0';
		tok->hash = Hash(tok->str, tok->num);

		for(uint64 i = 0; i < sizeof(keyword_hashes) / 8; i++) {
			if(tok->hash == keyword_hashes[i]) {
				tok->tok = TK_KWORD;
				break;
			}
		}

		LexUnCh();
		return tok->tok;
	case '\"':
		tok->tok = TK_STR;
		tok->str = AllocReduced(1024);
		LexCh();

		while(LexCurCh() != '\"') {
			if(LexEOF())
				LexError("End-of-file reached before string end");

			if(tok->num == 1023)
				LexError("String too long (more than 1023 characters)");

			tok->str[tok->num++] = LexStrCh();
			LexCh();
		}

		Reduce(tok->num + 1);
		tok->str[tok->num] = '\0';
		tok->hash = Hash(tok->str, tok->num);
		return TK_STR;
	case '\'':
		tok->tok = TK_INT;
		LexCh();

		while(LexCurCh() != '\'') {
			tok->num <<= 8;
			tok->num |= LexStrCh();
			LexCh();
		}

		return TK_INT;
	case '.':
		tok->tok = LexCurCh();
		LexCh();

		if(LexCurCh() == '.') {
			LexCh();

			if(LexCurCh() == '.')
				tok->tok = '...';
			else
				LexUnCh();
		} else {
			LexUnCh();
		}

		return tok->tok;
	case ',':
	case '@':
	case '(':
	case ')':
	case '[':
	case ']':
	case '{':
	case '}':
	case ';':
	case ':':
	case '?':
		tok->tok = LexCurCh();
		return tok->tok;
	case '!':
	case '*':
	case '%':
	case '/':
	case '^':
	case '~':
	case '=':
		tok->tok = LexCurCh();
		LexCh();

		if(LexCurCh() == '=') {
			tok->tok <<= 8;
			tok->tok |= LexCurCh();
		} else {
			LexUnCh();
		}

		return tok->tok;
	case '&':
	case '|':
	case '+':
		tok->tok = LexCurCh();
		LexCh();

		if(LexCurCh() == tok->tok || LexCurCh() == '=') {
			tok->tok <<= 8;
			tok->tok |= LexCurCh();
		} else {
			LexUnCh();
		}

		return tok->tok;
	case '-':
		tok->tok = LexCurCh();
		LexCh();

		if(LexCurCh() == tok->tok || LexCurCh() == '=' || LexCurCh() == '>') {
			tok->tok <<= 8;
			tok->tok |= LexCurCh();
		} else {
			LexUnCh();
		}

		return tok->tok;
	case '<':
	case '>':
		tok->tok = LexCurCh();
		LexCh();

		if(LexCurCh() == tok->tok) {
			tok->tok <<= 8;
			tok->tok |= LexCurCh();
			LexCh();

			if(LexCurCh() == '=') {
				tok->tok <<= 8;
				tok->tok |= LexCurCh();
			} else {
				LexUnCh();
			}
		} else if(LexCurCh() == '=') {
			tok->tok <<= 8;
			tok->tok |= LexCurCh();
		} else if(LexCurCh() == '+') {
			tok->tok <<= 8;
			tok->tok |= LexCurCh();
			LexCh();

			if(LexCurCh() == '=') {
				tok->tok <<= 8;
				tok->tok |= LexCurCh();
			} else {
				LexUnCh();
			}
		} else {
			LexUnCh();
		}

		return tok->tok;
	default:
		LexError("Invalid character");
	}

	return -1ULL;
}

void LexString(char *name, char *str)
{
	struct Source *new_src = Alloc(sizeof(struct Source));
	new_src->name = name;
	new_src->text = str;
	new_src->pos = -1ULL;
	new_src->line = 1;
	new_src->col = 0;
	new_src->next = src;
	src = new_src;
}

bool LexFile(char *file_name)
{
	struct Source *new_src = Alloc(sizeof(struct Source));
	new_src->name = file_name;
	FILE *f = fopen(file_name, "r");

	if(f == NULL)
		return FALSE;

	char *real_name = realpath(file_name, NULL);
	uint64 hash = Hash(real_name, strlen(real_name));
	free(real_name);

	for(uint64 i = 0; i < lexed_file_count; i++) {
		if(lexed_files[i] == hash)
			return TRUE;
	}

	lexed_files[lexed_file_count++] = hash;
	fseek(f, 0, SEEK_END);
	uint64 size = ftell(f);
	new_src->text = Alloc(size + 1);
	fseek(f, 0, SEEK_SET);
	fread(new_src->text, 1, size, f);

	if(ferror(f))
		return FALSE;

	fclose(f);
	new_src->pos = -1ULL;
	new_src->line = 1;
	new_src->col = 0;
	new_src->next = src;
	src = new_src;
	return TRUE;
}

char *TokenStr(struct Token *tok)
{
	char *str = Alloc(32);

	switch(tok->tok)
	{
	case TK_EOF:
		return "<EOF>";
	case TK_INT:
		if(tok->hash == 10) {
			snprintf(str, 32, "%lu", tok->num);
		} else if(tok->hash == 0) {
			snprintf(str, 32, "%.3s", (char *) &tok->num);
			str = Reverse(str);
		} else {
			snprintf(str, 32, "%016lX", tok->num);
		}

		return str;
	case TK_IDENT:
		return tok->str;
	case TK_KWORD:
		snprintf(str, 32, "\x1B[1m%s\x1B[0m", tok->str);
		return str;
	case TK_STR:
		return "<STR>";
	default:
		snprintf(str, 32, "%.3s", (char *) &tok->tok);
		return Reverse(str);
	}
}

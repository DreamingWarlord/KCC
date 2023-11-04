#pragma once

#include "Lex.h"
#include "Type.h"


#define EXPR_CONST        0
#define EXPR_OBJECT       1
#define EXPR_FUNCALL      2
#define EXPR_CAST         3
#define EXPR_POST_INC     4
#define EXPR_POST_DEC     5
#define EXPR_MEMBER       6
#define EXPR_MEMBER_DREF  7
#define EXPR_NEG          8
#define EXPR_NOT          9
#define EXPR_LNOT        10
#define EXPR_ADDROF      11
#define EXPR_DREF        12
#define EXPR_PRE_INC     13
#define EXPR_PRE_DEC     14
#define EXPR_SHL         15
#define EXPR_SHR         16
#define EXPR_MUL         17
#define EXPR_DIV         18
#define EXPR_MOD         19
#define EXPR_ADD         20
#define EXPR_SUB         21
#define EXPR_AND         22
#define EXPR_XOR         23
#define EXPR_OR          24
#define EXPR_LT          25
#define EXPR_LTE         26
#define EXPR_GT          27
#define EXPR_GTE         28
#define EXPR_EQ          29
#define EXPR_NEQ         30
#define EXPR_LAND        31
#define EXPR_LOR         32
#define EXPR_ASSIGN      33
#define EXPR_TERNARY     34

#define STMT_COMPOUND    0
#define STMT_EXPRESSION  1
#define STMT_CONDITIONAL 2
#define STMT_SELECTION   3
#define STMT_WHILE       4
#define STMT_DO_WHILE    5
#define STMT_FOR         6
#define STMT_GOTO        7
#define STMT_CONTINUE    8
#define STMT_BREAK       9
#define STMT_RETURN     10
#define STMT_VAR_DECL   11
#define STMT_FUNC_DECL  12
#define STMT_STRUC_DECL 13


struct ExprList;
struct Statement;

struct Object
{
	char            *name;
	struct Object   *next;
	struct Type      type;
	uint64          count;
	bool         is_const;
	struct ExprList *expr; // non-NULL if it's a compile-time object
	void             *buf; // non-NULL if it's a constant buffer

	union {
		struct {
			bool         is_static;
			bool         is_inline;
			struct Statement *body;
		} func;
	};
};

struct ExprNode
{
	uint64          kind;
	struct Type     type;
	struct ExprNode *lhs;
	struct ExprNode *rhs;
	struct Token  *token;

	union {
		uint64            num;
		char          *member;
		struct Object    *obj;
		struct ExprNode *tern;
		struct ExprList *list;
	};
};

struct ExprList
{
	struct ExprNode *node;
	struct ExprList *next;
};

struct Statement
{
	uint64            kind;
	char            *label;
	uint64      case_label;
	struct Statement *next;
	struct Token    *token;

	union {
		struct {
			struct Object    *obj;
			struct ExprList *expr;
		} var_decl;

		struct {
			struct ExprNode *condition;
			struct Statement  *br_true;
			struct Statement *br_false;
		} conditional;

		struct {
			struct ExprNode       *expr;
			struct Statement *case_list;
		} selection;

		struct {
			struct ExprNode *condition;
			struct Statement  *br_true;
		} while_loop;

		struct {
			struct Statement  *preloop;
			struct ExprNode *condition;
			struct Statement *postloop;
			struct Statement  *br_true;
		} for_loop;

		char           *goto_label;
		struct ExprNode      *expr;
		struct Statement *compound;
		struct Struct       *struc;
		struct Object        *func;
	};
};


uint64 TokenFetch();

void TokenUnfetch();

void ScopePush();

bool ScopeInsert(struct Object *obj);

struct Object *ScopeFind(char *name);

void ScopePop();

bool GlblObjectTableInsert(struct Object *obj);

struct Object *GlblObjectTableFind(char *name);

struct Object **GlblObjectTable();

void ExprNodePrint(struct ExprNode *node);

uint64 ExprNodePrec(uint64 kind);

uint64 ExprNodeFromToken(uint64 token);

char *ExprNodeStr(uint64 kind);

bool ExprNodeEvaluate(struct ExprNode *node, uint64 *res);

bool ExprNodeConst(struct ExprNode *node);

bool ExprNodeRef(struct ExprNode *node);

void ExprNodeBuildType(struct ExprNode *node);

void StatementPrint(struct Statement *stmt);

bool ParseTypePrimitive(struct Type *type);

bool ParseType(struct Type *type);

struct ExprNode *ParsePrimary();

struct ExprNode *ParsePostfix();

struct ExprNode *ParsePrefix();

struct ExprNode *ParseBinary(struct ExprNode *left, uint64 min_prec);

struct ExprNode *ParseExpression();

struct ExprList *ParseExpressionList(uint64 max_count);

struct ExprNode *ParseAssignment(struct ExprNode *left);

struct Statement *ParseVarDecl(bool allow_def);

struct Statement *ParseCompound();

struct Statement *ParseExpressionStatement();

struct Statement *ParseConditional();

struct Statement *ParseWhile();

struct Statement *ParseDoWhile();

struct Statement *ParseFor();

struct Statement *ParseSwitch();

struct Statement *ParseJump();

struct Statement *ParseStatement();

struct Statement *ParseStructDecl();

struct Statement *ParseFuncDecl();

void ParseIncludes();

struct Statement *ParseDecl();

struct Statement *Parse();

void StatementValidate(struct Statement *stmt);

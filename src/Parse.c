#include "Parse.h"


struct TokenList
{
	struct Token      *tok;
	struct TokenList *next;
	struct TokenList *prev;
};


static struct Token *tok = NULL;
static struct TokenList *toklist_head = NULL;


uint64 TokenFetch()
{
	if(toklist_head != NULL && toklist_head->next != NULL) {
		toklist_head = toklist_head->next;
		tok = toklist_head->tok;
		return tok->tok;
	}

	struct TokenList *list = Alloc(sizeof(struct TokenList));
	list->prev = toklist_head;
	list->tok = Alloc(sizeof(struct Token));
	tok = list->tok;

	if(toklist_head != NULL)
		toklist_head->next = list;

	toklist_head = list;
	return Lex(list->tok);
}

void TokenUnfetch()
{
	Assert(toklist_head != NULL && toklist_head->prev != NULL);
	toklist_head = toklist_head->prev;
	tok = toklist_head->tok;
}

bool ParseTypePrimitive(struct Type *type)
{
	if(tok->tok != TK_KWORD)
		return FALSE;

	switch(tok->hash)
	{
	case KW_VOID:
		type->kind = KIND_VOID;
		break;
	case KW_INT8:
		type->kind = KIND_INT8;
		break;
	case KW_INT16:
		type->kind = KIND_INT16;
		break;
	case KW_INT32:
		type->kind = KIND_INT32;
		break;
	case KW_INT64:
		type->kind = KIND_INT64;
		break;
	case KW_STRUCT:
		type->kind = KIND_STRUCT;

		if(TokenFetch() != TK_IDENT)
			TokenError(tok, "Expected identifier, got %s", TokenStr(tok));

		type->idx = StructFind(tok->str);

		if(type->idx == -1ULL)
			TokenError(tok, "%s is not a valid struct", TokenStr(tok));

		break;
	default:
		return FALSE;
	}

	TokenFetch();
	return TRUE;
}

bool ParseType(struct Type *type)
{
	if(!ParseTypePrimitive(type))
		return FALSE;

	while(TRUE) {
		if(tok->tok == '*') {
			if(type->ptrc == 15)
				TokenError(tok, "Too many pointers");

			type->ptrc++;
			TokenFetch();
		} else if(tok->tok == '@') {
			if(TokenFetch() != '(')
				TokenError(tok, "Expected '(', got %s", TokenStr(tok));

			struct Function *func_templ = Alloc(sizeof(struct Function));
			func_templ->type = *type;
			TokenFetch();

			while(tok->tok != ')') {
				if(tok->tok == '...') {
					func_templ->variadic = TRUE;

					if(TokenFetch() != ')')
						TokenError(tok, "Expected ')' after '...', got %s", TokenStr(tok));

					break;
				}

				if(func_templ->count == FUNC_MAX_ARGC)
					TokenError(tok, "Too many function arguments");

				if(!ParseType(&func_templ->args[func_templ->count++]))
					TokenError(tok, "Expected valid function argument type, got %s", TokenStr(tok));

				if(TypeSize(func_templ->args[func_templ->count - 1]) == 0)
					TokenError(tok, "Incomplete function argument type");

				if(tok->tok == TK_IDENT)
					TokenFetch();

				if(tok->tok == ',') {
					if(TokenFetch() == ')')
						TokenError(tok, "Expected function argument, got %s", TokenStr(tok));
				} else if(tok->tok == ')') {
					break;
				} else {
					TokenError(tok, "Expected ')' or function argument, got %s", TokenStr(tok));
				}
			}

			TokenFetch();
			type->kind = KIND_FUNC;
			type->ptrc = 1;
			type->idx = FunctionFind(func_templ);
		} else {
			break;
		}
	}

	return TRUE;
}

struct ExprNode *ParsePrimary()
{
	struct ExprNode *node = Alloc(sizeof(struct ExprNode));
	node->token = tok;

	switch(tok->tok)
	{
	case TK_STR:
		node->kind = EXPR_OBJECT;
		node->type = (struct Type) { KIND_INT8, 1, 0 };
		node->obj = Alloc(sizeof(struct Object));
		node->obj->name = "<str>";
		node->obj->type = (struct Type) { KIND_INT8, 0, 0 };
		node->obj->count = tok->num + 1;
		node->obj->buf = tok->str;
		break;
	case TK_INT:
		node->kind = EXPR_CONST;
		node->type = (struct Type) { KIND_INT64, 0, 0 };
		node->num = tok->num;
		break;
	case TK_IDENT:
		node->kind = EXPR_CONST;
		node->num = GlblEnumTableFind(tok->str);

		if(node->num != -1ULL) {
			node->type = (struct Type) { KIND_INT64, 0, 0 };
			break;
		}

		node->kind = EXPR_OBJECT;
		node->obj = ScopeFind(tok->str);

		if(node->obj == NULL)
			node->obj = GlblObjectTableFind(tok->str);

		if(node->obj == NULL)
			TokenError(tok, "Object %s does not exist", TokenStr(tok));

		node->type = node->obj->type;

		if(node->obj->count != 1)
			node->type.ptrc++;

		break;
	case TK_KWORD:
		if(tok->hash == KW_SIZEOF) {
			if(TokenFetch() != '(')
				TokenError(tok, "Expected '(', got %s", TokenStr(tok));

			TokenFetch();

			if(!ParseType(&node->type))
				TokenError(tok, "Expected type, got %s", TokenStr(tok));

			if(tok->tok != ')')
				TokenError(tok, "Expected ')', got %s", TokenStr(tok));

			TokenFetch();
			node->kind = EXPR_CONST;
			node->num = TypeSize(node->type);
			node->type = (struct Type) { KIND_INT64, 0, 0 };
		} else if(tok->hash == KW_CONTAINEROF) {
			if(TokenFetch() != '(')
				TokenError(tok, "Expected '(', got %s", TokenStr(tok));

			node->kind = EXPR_CAST;
			node->type = (struct Type) { KIND_INT8, 1, 0 };
			node->lhs = ParseExpression();

			if(node->lhs == NULL)
				TokenError(tok, "Expected expression, got %s", TokenStr(tok));

			if(tok->tok == ',')
				TokenError(tok, "Expected ',', got %s", TokenStr(tok));

			if(TokenFetch() != TK_KWORD || tok->hash != KW_STRUCT)
				TokenError(tok, "Expected struct, got %s", TokenStr(tok));

			if(TokenFetch() != TK_IDENT)
				TokenError(tok, "Expected struct name, got %s", TokenStr(tok));

			struct Struct *struc = StructGet(StructFind(tok->str));

			if(struc == NULL)
				TokenError(tok, "Struct %s doesn't exist", TokenStr(tok));

			if(TokenFetch() != ',')
				TokenError(tok, "Expected ',', got %s", TokenStr(tok));

			if(TokenFetch() != TK_IDENT)
				TokenError(tok, "Expected member name, got %s", TokenStr(tok));

			struct Member *member = NULL;

			for(uint64 i = 0; i < struc->count; i++) {
				if(!strcmp(struc->members[i].name, tok->str)) {
					member = &struc->members[i];
					break;
				}
			}

			if(member == NULL)
				TokenError(tok, "Member %s does not exist", TokenStr(tok));

			struct ExprNode *const_node = Alloc(sizeof(struct ExprNode));
			const_node->kind = EXPR_CONST;
			const_node->token = tok;
			const_node->num = member->offset;
			const_node->type = (struct Type) { KIND_INT64, 0, 0 };
			struct ExprNode *offset_node = Alloc(sizeof(struct ExprNode));
			offset_node->kind = EXPR_SUB;
			offset_node->token = tok;
			offset_node->type = member->type;
			offset_node->type.ptrc++;
			offset_node->lhs = node;
			offset_node->rhs = const_node;
			node = offset_node;

			if(TokenFetch() != ')')
				TokenError(tok, "Expected ')', got %s", TokenStr(tok));
		} else {
			if(!ParseType(&node->type))
				return NULL;

			if(tok->tok == '+') {
				node->kind = EXPR_CAST_EXT;
				TokenFetch();
			} else {
				node->kind = EXPR_CAST;
			}

			if(tok->tok != '(')
				TokenError(tok, "Expected '(', got %s", TokenStr(tok));

			TokenFetch();
			node->lhs = ParseExpression();

			if(tok->tok != ')')
				TokenError(tok, "Expected ')', got %s", TokenStr(tok));
		}

		break;
	case '(':
		TokenFetch();
		node = ParseExpression();

		if(tok->tok != ')')
			TokenError(tok, "Expected ')', got %s", TokenStr(tok));

		break;
	default:
		return NULL;
	}

	TokenFetch();
	return node;
}

struct ExprNode *ParsePostfix()
{
	struct ExprNode *node = ParsePrimary();

	while(TRUE) {
		struct ExprNode *wrapper = Alloc(sizeof(struct ExprNode));
		wrapper->token = tok;
	
		switch(tok->tok)
		{
		case '[':
			wrapper->kind = EXPR_ADD;
			TokenFetch();
			wrapper->lhs = node;
			wrapper->rhs = ParseExpression();
	
			if(tok->tok != ']')
				TokenError(tok, "Expected ']', got %s", TokenStr(tok));
	
			node = Alloc(sizeof(struct ExprNode));
			node->kind = EXPR_DREF;
			node->token = wrapper->token;
			node->lhs = wrapper;
			wrapper = node;
			break;
		case '++':
			wrapper->kind = EXPR_POST_INC;
			wrapper->lhs = node;
			break;
		case '--':
			wrapper->kind = EXPR_POST_DEC;
			wrapper->lhs = node;
			break;
		case '.':
			wrapper->kind = EXPR_MEMBER;
			wrapper->lhs = node;
	
			if(TokenFetch() != TK_IDENT)
				TokenError(tok, "Expected member name identifier, got %s", TokenStr(tok));
	
			wrapper->member = tok->str;
			break;
		case '->':
			wrapper->kind = EXPR_MEMBER_DREF;
			wrapper->lhs = node;
	
			if(TokenFetch() != TK_IDENT)
				TokenError(tok, "Expected member name identifier, got %s", TokenStr(tok));
	
			wrapper->member = tok->str;
			break;
		case '(': {
			TokenFetch();
			struct ExprList *head = Alloc(sizeof(struct ExprList));
			struct ExprList *cur = head;
	
			while(tok->tok != ')') {
				cur->node = ParseExpression();
	
				if(cur->node == NULL)
					TokenError(tok, "Expected expression, got %s", TokenStr(tok));
	
				if(tok->tok == ',') {
					TokenFetch();
	
					if(tok->tok == ')')
						TokenError(tok, "Expected argument, got ')'");
	
					cur->next = Alloc(sizeof(struct ExprList));
					cur = cur->next;
				} else if(tok->tok == ')') {
					break;
				} else {
					TokenError(tok, "Expected ')' or argument, got %s", TokenStr(tok));
				}
			}

			wrapper->kind = EXPR_FUNCALL;
			wrapper->lhs = node;

			if(head->node != NULL)
				wrapper->list = head;

			break;
		}
		default:
			return node;
		}

		node = wrapper;
		TokenFetch();
	}

	return NULL;
}

struct ExprNode *ParsePrefix()
{
	struct ExprNode *wrapper = Alloc(sizeof(struct ExprNode));
	wrapper->token = tok;

	switch(tok->tok)
	{
	case '-':
		wrapper->kind = EXPR_NEG;
		break;
	case '~':
		wrapper->kind = EXPR_NOT;
		break;
	case '!':
		wrapper->kind = EXPR_LNOT;
		break;
	case '&':
		wrapper->kind = EXPR_ADDROF;
		break;
	case '*':
		wrapper->kind = EXPR_DREF;
		break;
	case '--':
		wrapper->kind = EXPR_PRE_DEC;
		break;
	case '++':
		wrapper->kind = EXPR_PRE_INC;
		break;
	default:
		return ParsePostfix();
	}

	TokenFetch();
	wrapper->lhs = ParsePrefix();
	return wrapper;
}

struct ExprNode *ParseBinary(struct ExprNode *left, uint64 min_prec)
{
	struct Token *token = tok;
	uint64 kind = ExprNodeFromToken(tok->tok);
	uint64 prec = ExprNodePrec(kind);

	while(prec != -1ULL && prec >= min_prec) {
		struct Token *op_token = token;
		uint64 op_kind = kind;
		uint64 op_prec = prec;
		TokenFetch();
		struct ExprNode *right = ParsePrefix();
		token = tok;
		kind = ExprNodeFromToken(tok->tok);
		prec = ExprNodePrec(kind);

		while(prec != -1ULL && prec > op_prec) {
			right = ParseBinary(right, op_prec + 1);
			kind = ExprNodeFromToken(tok->tok);
			prec = ExprNodePrec(kind);
		}

		struct ExprNode *op = Alloc(sizeof(struct ExprNode));
		op->kind = op_kind;
		op->token = op_token;
		op->lhs = left;
		op->rhs = right;
		left = op;
	}

	return left;
}

struct ExprNode *ParseTernary()
{
	struct ExprNode *node = ParseBinary(ParsePrefix(), 0);

	if(node == NULL)
		return NULL;

	if(tok->tok == '?') {
		struct Token *op_tok = tok;
		TokenFetch();
		struct ExprNode *node_true = ParseExpression();

		if(node_true == NULL)
			TokenError(tok, "Expected expression, got %s", TokenStr(tok));

		if(tok->tok != ':')
			TokenError(tok, "Expected ':', got %s", TokenStr(tok));

		TokenFetch();
		struct ExprNode *node_false = ParseExpression();

		if(node_false == NULL)
			TokenError(tok, "Expected expression, got %s", TokenStr(tok));

		struct ExprNode *ternary = Alloc(sizeof(struct ExprNode));
		ternary->kind = EXPR_TERNARY;
		ternary->token = op_tok;
		ternary->lhs = node_true;
		ternary->rhs = node_false;
		ternary->tern = node;
		return ternary;
	}

	return node;
}

struct ExprNode *ParseExpression()
{
	struct ExprNode *left = ParseTernary();

	if(left == NULL)
		return NULL;

	struct ExprNode *node = ParseAssignment(left);
	return node != NULL ? node : left;
}

struct ExprList *ParseExpressionList(uint64 max_count)
{
	if(tok->tok != '{')
		return NULL;

	TokenFetch();
	struct ExprList *head = Alloc(sizeof(struct ExprList));
	struct ExprList *cur = head;
	uint64 count = 0;

	while(TRUE) {
		cur->node = ParseExpression();

		if(cur->node == NULL)
			TokenError(tok, "Expected expression, got %s", TokenStr(tok));

		count++;

		if(tok->tok == '}') {
			break;
		} else if(tok->tok == ',') {
			if(count >= max_count)
				TokenError(tok, "Expected at most %lu expressions", max_count);

			cur->next = Alloc(sizeof(struct ExprList));
			cur = cur->next;
			TokenFetch();
		} else {
			TokenError(tok, "Expected ',' or '}', got %s", TokenStr(tok));
		}
	}

	TokenFetch();
	return head;
}

struct ExprNode *ParseAssignment(struct ExprNode *left)
{
	uint64 kind = 0;

	switch(tok->tok)
	{
	case '=':
		kind = EXPR_CONST;
		break;
	case '<<=':
		kind = EXPR_SHL;
		break;
	case '>>=':
		kind = EXPR_SHR;
		break;
	case '*=':
		kind = EXPR_MUL;
		break;
	case '/=':
		kind = EXPR_DIV;
		break;
	case '%=':
		kind = EXPR_MOD;
		break;
	case '+=':
		kind = EXPR_ADD;
		break;
	case '-=':
		kind = EXPR_SUB;
		break;
	case '&=':
		kind = EXPR_AND;
		break;
	case '^=':
		kind = EXPR_XOR;
		break;
	case '|=':
		kind = EXPR_OR;
		break;
	case '~=':
		kind = EXPR_NOT;
		break;
	default:
		return NULL;
	}

	struct ExprNode *node = Alloc(sizeof(struct ExprNode));
	struct ExprNode *right = Alloc(sizeof(struct ExprNode));
	node->kind = EXPR_ASSIGN;
	node->token = tok;
	node->lhs = left;
	node->rhs = right;
	TokenFetch();
	right->kind = kind;
	right->lhs = left;
	right->rhs = ParseExpression();

	if(right->rhs == NULL)
		TokenError(tok, "Expected right-hand side of assignment, got %s", TokenStr(tok));

	if(right->kind == EXPR_CONST)
		node->rhs = right->rhs;
	else
		node->rhs = right;

	return node;
}

struct Statement *ParseCompound()
{
	if(tok->tok != '{')
		return NULL;

	ScopePush();
	struct Statement *stmt = Alloc(sizeof(struct Statement));
	stmt->kind = STMT_COMPOUND;
	stmt->token = tok;
	struct Statement *cur = NULL;
	TokenFetch();

	while(TRUE) {
		if(tok->tok == '}')
			break;

		if(tok->tok == TK_EOF)
			TokenError(tok, "End of file before end of compound statement");

		struct Statement *next = ParseVarDecl(TRUE);

		if(next == NULL)
			next = ParseStatement();

		if(next == NULL)
			TokenError(tok, "Expected statement or variable declaration, got %s", TokenStr(tok));

		if(cur == NULL) {
			cur = next;
			stmt->compound = cur;
		} else {
			cur->next = next;
			cur = next;
		}
	}

	TokenFetch();
	ScopePop();
	return stmt;
}

struct Statement *ParseExpressionStatement()
{
	struct Token *stmt_tok = tok;
	struct ExprNode *node = ParseExpression();

	if(node == NULL)
		return NULL;

	if(tok->tok != ';')
		TokenError(tok, "Expected ';', got %s", TokenStr(tok));

	TokenFetch();
	struct Statement *stmt = Alloc(sizeof(struct Statement));
	stmt->kind = STMT_EXPRESSION;
	stmt->expr = node;
	stmt->token = stmt_tok;
	return stmt;
}

struct Statement *ParseConditional()
{
	if(tok->tok != TK_KWORD || tok->hash != KW_IF)
		return NULL;

	struct Statement *stmt = Alloc(sizeof(struct Statement));
	stmt->kind = STMT_CONDITIONAL;
	stmt->token = tok;

	if(TokenFetch() != '(')
		TokenError(tok, "Expected '(' after 'if', got %s", TokenStr(tok));

	TokenFetch();
	stmt->conditional.condition = ParseExpression();

	if(stmt->conditional.condition == NULL)
		TokenError(tok, "Expected if condition, got %s", TokenStr(tok));

	if(tok->tok != ')')
		TokenError(tok, "Expected ')', got %s", TokenStr(tok));

	TokenFetch();
	stmt->conditional.br_true = ParseStatement();

	if(stmt->conditional.br_true == NULL)
		TokenError(tok, "Expected if statement body, got %s", TokenStr(tok));

	if(tok->tok != TK_KWORD || tok->hash != KW_ELSE)
		return stmt;

	TokenFetch();
	stmt->conditional.br_false = ParseStatement();

	if(stmt->conditional.br_false == NULL)
		TokenError(tok, "Expected if-else statement body, got %s", TokenStr(tok));

	return stmt;
}

struct Statement *ParseWhile()
{
	if(tok->tok != TK_KWORD || tok->hash != KW_WHILE)
		return NULL;

	struct Statement *stmt = Alloc(sizeof(struct Statement));
	stmt->kind = STMT_WHILE;
	stmt->token = tok;

	if(TokenFetch() != '(')
		TokenError(tok, "Expected '(' after 'while', got %s", TokenStr(tok));

	TokenFetch();
	stmt->while_loop.condition = ParseExpression();

	if(stmt->while_loop.condition == NULL)
		TokenError(tok, "Expected loop condition, got %s", TokenStr(tok));

	if(tok->tok != ')')
		TokenError(tok, "Expected ')', got %s", TokenStr(tok));

	TokenFetch();
	stmt->while_loop.br_true = ParseStatement();

	if(stmt->while_loop.br_true == NULL)
		TokenError(tok, "Expected while statement body, got %s", TokenStr(tok));

	return stmt;
}

struct Statement *ParseDoWhile()
{
	if(tok->tok != TK_KWORD || tok->hash != KW_DO)
		return NULL;

	struct Statement *stmt = Alloc(sizeof(struct Statement));
	stmt->kind = STMT_DO_WHILE;
	stmt->token = tok;
	TokenFetch();
	stmt->while_loop.br_true = ParseStatement();

	if(stmt->while_loop.br_true == NULL)
		TokenError(tok, "Expected do-while statement body, got %s", TokenStr(tok));

	if(tok->tok != TK_KWORD || tok->hash != KW_WHILE)
		TokenError(tok, "Expected 'while' after do-while statement body, got %s", TokenStr(tok));

	if(TokenFetch() != '(')
		TokenError(tok, "Expected '(' after 'while' body, got %s", TokenStr(tok));

	TokenFetch();
	stmt->while_loop.condition = ParseExpression();

	if(stmt->while_loop.condition == NULL)
		TokenError(tok, "Expected loop condition, got %s", TokenStr(tok));

	if(tok->tok != ')')
		TokenError(tok, "Expected ')', got %s", TokenStr(tok));

	if(TokenFetch() != ';')
		TokenError(tok, "Expected ';', got %s", TokenStr(tok));

	TokenFetch();
	return stmt;
}

struct Statement *ParseFor()
{
	if(tok->tok != TK_KWORD || tok->hash != KW_FOR)
		return NULL;

	struct Statement *stmt = Alloc(sizeof(struct Statement));
	stmt->kind = STMT_FOR;
	stmt->token = tok;
	ScopePush();

	if(TokenFetch() != '(')
		TokenError(tok, "Expected '(', got %s", TokenStr(tok));

	TokenFetch();
	stmt->for_loop.preloop = ParseVarDecl(TRUE);

	if(stmt->for_loop.preloop == NULL)
		TokenError(tok, "Expected iteration variable declaration, got %s", TokenStr(tok));

	stmt->for_loop.condition = ParseExpression();

	if(stmt->for_loop.condition == NULL)
		TokenError(tok, "Expected for loop condition expression, got %s", TokenStr(tok));

	if(tok->tok != ';')
		TokenError(tok, "Expected ';', got %s", TokenStr(tok));

	TokenFetch();
	stmt->for_loop.postloop = Alloc(sizeof(struct Statement));
	stmt->for_loop.postloop->kind = STMT_EXPRESSION;
	stmt->for_loop.postloop->token = tok;
	struct ExprNode *node = ParseExpression();
	stmt->for_loop.postloop->expr = node;

	if(node == NULL)
		TokenError(tok, "Expected expression, got %s", TokenStr(tok));

	if(tok->tok != ')')
		TokenError(tok, "Expected ')', got %s", TokenStr(tok));

	TokenFetch();
	stmt->for_loop.br_true = ParseStatement();

	if(stmt->for_loop.br_true == NULL)
		TokenError(tok, "Expected for loop body, got %s", TokenStr(tok));

	ScopePop();
	return stmt;
}

struct Statement *ParseSwitch()
{
	if(tok->tok != TK_KWORD || tok->hash != KW_SWITCH)
		return NULL;

	struct Statement *stmt = Alloc(sizeof(struct Statement));
	stmt->kind = STMT_SELECTION;
	stmt->token = tok;

	if(TokenFetch() != '(')
		TokenError(tok, "Expected '(', got %s", TokenStr(tok));

	TokenFetch();
	stmt->selection.expr = ParseExpression();

	if(stmt->selection.expr == NULL)
		TokenError(tok, "Expected switch expression, got %s", TokenStr(tok));

	if(tok->tok != ')')
		TokenError(tok, "Expected ')', got %s", TokenStr(tok));

	if(TokenFetch() != '{')
		TokenError(tok, "Expected '{', got %s", TokenStr(tok));

	TokenFetch();
	ScopePush();
	struct Statement *head = NULL;

	while(TRUE) {
		if(tok->tok == '}')
			break;

		struct ExprNode *label = ParseExpression();

		if(label == NULL)
			TokenError(tok, "Expected case label, got %s", TokenStr(tok));

		if(!ExprNodeConst(label))
			TokenError(tok, "Case label has to be constant");

		if(tok->tok != ':')
			TokenError(tok, "Expected ':', got %s", TokenStr(tok));

		TokenFetch();
		struct Statement *case_stmt = ParseStatement();

		if(case_stmt == NULL)
			TokenError(tok, "Expected statement after case label");

		ExprNodeEvaluate(label, &case_stmt->case_label);

		if(head == NULL) {
			head = case_stmt;
			stmt->selection.case_list = head;
		} else {
			Assert(head->next == NULL);
			head->next = case_stmt;
			head = case_stmt;
		}
	}

	ScopePop();
	TokenFetch();
	return stmt;
}

struct Statement *ParseJump()
{
	if(tok->tok != TK_KWORD)
		return NULL;

	uint64 kind = 0;

	switch(tok->hash)
	{
	case KW_GOTO:
		kind = STMT_GOTO;
		break;
	case KW_CONTINUE:
		kind = STMT_CONTINUE;
		break;
	case KW_BREAK:
		kind = STMT_BREAK;
		break;
	case KW_RETURN:
		kind = STMT_RETURN;
		break;
	default:
		return NULL;
	}

	struct Statement *stmt = Alloc(sizeof(struct Statement));
	stmt->kind = kind;
	stmt->token = tok;

	if(kind == STMT_CONTINUE || kind == STMT_BREAK) {
		if(TokenFetch() != ';')
			TokenError(tok, "Expected ';' after continue or break, got %s", TokenStr(tok));

		TokenFetch();
		return stmt;
	}

	if(kind == STMT_GOTO) {
		if(TokenFetch() != TK_IDENT)
			TokenError(tok, "Expected label name, got %s", TokenStr(tok));

		stmt->goto_label = tok->str;

		if(TokenFetch() != ';')
			TokenError(tok, "Expected ';' after goto, got %s", TokenStr(tok));

		TokenFetch();
		return stmt;
	}

	if(TokenFetch() == ';') {
		TokenFetch();
		return stmt;
	}

	stmt->expr = ParseExpression();

	if(stmt->expr == NULL)
		TokenError(tok, "Expected return expression, got %s", TokenStr(tok));

	if(tok->tok != ';')
		TokenError(tok, "Expected ';' after return, got %s", TokenStr(tok));

	TokenFetch();
	return stmt;
}

struct Statement *ParseStatement()
{
	char *stmt_label = NULL;

	if(tok->tok == TK_IDENT) {
		struct TokenList *list_head = toklist_head;
		stmt_label = tok->str;

		if(TokenFetch() != ':') {
			toklist_head = list_head;
			tok = toklist_head->tok;
			stmt_label = NULL;
		} else {
			TokenFetch();
		}
	}

	if(tok->tok == ';') {
		struct Statement *stmt = Alloc(sizeof(struct Statement));
		stmt->kind = STMT_EXPRESSION;
		stmt->token = tok;
		stmt->label = stmt_label;
		stmt->expr = Alloc(sizeof(struct ExprNode));
		stmt->expr->token = tok;
		stmt->expr->kind = EXPR_CONST;
		stmt->expr->num = 0;
		TokenFetch();
		return stmt;
	}

	struct Statement *stmt = ParseCompound();

	if(stmt == NULL)
		stmt = ParseConditional();

	if(stmt == NULL)
		stmt = ParseWhile();

	if(stmt == NULL)
		stmt = ParseDoWhile();

	if(stmt == NULL)
		stmt = ParseFor();

	if(stmt == NULL)
		stmt = ParseSwitch();

	if(stmt == NULL)
		stmt = ParseJump();

	if(stmt == NULL)
		stmt = ParseExpressionStatement();

	if(stmt == NULL && stmt_label != NULL)
		TokenError(tok, "Expected statement, got %s", TokenStr(tok));

	if(stmt != NULL)
		stmt->label = stmt_label;

	return stmt;
}

struct Statement *ParseVarDecl(bool allow_def)
{
	bool is_const = FALSE;

	if(tok->tok == TK_KWORD && tok->hash == KW_CONST) {
		is_const = TRUE;
		TokenFetch();
	}

	struct Type type;
	struct TokenList *list_head = toklist_head;

	if(!ParseType(&type)) {
		if(is_const)
			TokenError(tok, "Expected constant type, got %s", TokenStr(tok));

		return NULL;
	}

	if(tok->tok != TK_IDENT && !is_const) {
		toklist_head = list_head;
		tok = toklist_head->tok;
		return NULL;
	}

	if(tok->tok != TK_IDENT && is_const)
		TokenError(tok, "Expected constant variable name, got %s", TokenStr(tok));

	if(TypeSize(type) == 0)
		TokenError(tok, "Can't declare a variable of incomplete type");

	struct Object *var = Alloc(sizeof(struct Object));
	var->name = tok->str;
	var->type = type;
	var->count = 1;
	var->is_const = is_const;
	struct Statement *stmt = Alloc(sizeof(struct Statement));
	stmt->kind = STMT_VAR_DECL;
	stmt->token = tok;
	stmt->var_decl.obj = var;
	TokenFetch();

	if(GlblEnumTableFind(var->name) != -1ULL)
		TokenError(tok, "Variable %s shadows enumeration constant of the same name", var->name);

	if(!ScopeInsert(var))
		TokenError(tok, "Variable %s already exists", var->name);

	var->is_local = GlblEnumTableFind(var->name) == -1ULL;

	if(tok->tok == ';') {
		if(is_const)
			TokenError(tok, "Constant variable has to be assigned to");

		TokenFetch();
		return stmt;
	}

	if(tok->tok == '[') {
		TokenFetch();
		struct ExprNode *count_node = ParseExpression();

		if(count_node == NULL)
			TokenError(tok, "Expected array size expression, got %s", TokenStr(tok));

		if(!ExprNodeEvaluate(count_node, &var->count))
			TokenError(tok, "Array size has to be a constant unsigned integer");

		if(var->count <= 1)
			TokenError(tok, "Array size has to be greater than 1");

		if(tok->tok != ']')
			TokenError(tok, "Expected ']', got %s", TokenStr(tok));

		TokenFetch();
	}

	if(tok->tok == ';') {
		if(is_const)
			TokenError(tok, "Constant variable has to be assigned to");

		TokenFetch();
		return stmt;
	}

	if(tok->tok != '=')
		TokenError(tok, "Expected '=', got %s", TokenStr(tok));

	TokenFetch();
	struct ExprList *expr = NULL;

	if(var->count > 1) {
		expr = ParseExpressionList(var->count);

		if(expr == NULL)
			TokenError(tok, "Expected expression list, got %s", TokenStr(tok));
	} else {
		expr = Alloc(sizeof(struct ExprList));
		expr->node = ParseExpression();

		if(expr->node == NULL)
			TokenError(tok, "Expected expression, got %s", TokenStr(tok));
	}

	if(is_const) {
		struct ExprList *cur = expr;

		while(cur != NULL) {
			if(!ExprNodeConst(cur->node))
				TokenError(tok, "Expected constant expression list for constant variable");

			cur = cur->next;
		}

		var->expr = expr;
	} else {
		stmt->var_decl.expr = expr;

		if(!allow_def)
			TokenError(tok, "Definition of non-const variable in global scope is not allowed");
	}

	if(tok->tok != ';')
		TokenError(tok, "Expected ';', got %s", TokenStr(tok));

	TokenFetch();
	return stmt;
}

struct Statement *ParseStructDecl()
{
	if(tok->tok != TK_KWORD || (tok->hash != KW_STRUCT && tok->hash != KW_UNION))
		return NULL;

	struct Statement *stmt = Alloc(sizeof(struct Statement));
	stmt->kind = STMT_STRUC_DECL;
	stmt->token = tok;
	char *name = NULL;
	bool is_union = FALSE;

	if(tok->hash == KW_STRUCT) {
		struct TokenList *list_head = toklist_head;

		if(TokenFetch() != TK_IDENT)
			TokenError(tok, "Expected struct name, got %s", TokenStr(tok));

		name = tok->str;

		if(TokenFetch() == ';') {
			TokenFetch();

			if(StructFind(name) != -1ULL) {
				stmt->struc = StructGet(StructFind(name));
				return stmt;
			}

			struct Struct *struc = Alloc(sizeof(struct Struct) + sizeof(struct Member) * 128);
			struc->name = name;
			StructInsert(struc);
			stmt->struc = struc;
			return stmt;
		}

		if(tok->tok != '{') {
			toklist_head = list_head;
			tok = list_head->tok;
			return NULL;
		}

		TokenFetch();
	} else {
		if(TokenFetch() != TK_KWORD || tok->hash != KW_STRUCT)
			TokenError(tok, "Expected 'struct' after 'union', got %s", TokenStr(tok));

		if(TokenFetch() != TK_IDENT)
			TokenError(tok, "Expected struct name, got %s", TokenStr(tok));

		name = tok->str;

		if(TokenFetch() == ';')
			TokenError(tok, "union implies struct definition");

		if(tok->tok != '{')
			TokenError(tok, "Expected '{', got %s", TokenStr(tok));

		TokenFetch();
		is_union = TRUE;
	}

	struct Struct *struc = StructGet(StructFind(name));

	if(struc == NULL) {
		struc = Alloc(sizeof(struct Struct) + sizeof(struct Member) * 128);
		struc->name = name;
		StructInsert(struc);
	} else {
		if(struc->count != 0)
			TokenError(tok, "struct %s is already defined", name);
	}

	while(TRUE) {
		if(struc->count >= 128)
			TokenError(tok, "Too many struct members!");

		struct Member *memb = &struc->members[struc->count++];

		if(!ParseType(&memb->type))
			TokenError(tok, "Expected type, got %s", TokenStr(tok));

		if(TypeSize(memb->type) == 0)
			TokenError(tok, "Struct member has incomplete type");

		if(tok->tok == TK_IDENT) {
			memb->name = tok->str;
			TokenFetch();
		}

		if(tok->tok == '[') {
			if(TokenFetch() == ']') {
				memb->count = 0;
				TokenFetch();
			} else {
				struct ExprNode *count = ParseExpression();

				if(!ExprNodeConst(count))
					TokenError(tok, "Array size must be constant");

				if(!ExprNodeEvaluate(count, &memb->count))
					TokenError(tok, "Array size must be constant (compiler bug)");

				if(tok->tok != ']')
					TokenError(tok, "Expected ']', got %s", TokenStr(tok));

				TokenFetch();
			}
		} else {
			memb->count = 1;
		}

		memb->offset = is_union ? 0 : struc->size;
		struc->size += TypeSize(memb->type) * memb->count;

		if(tok->tok != ';')
			TokenError(tok, "Expected ';', got %s", TokenStr(tok));

		TokenFetch();

		if(memb->count == 0 && tok->tok != '}')
			TokenError(tok, "Expected '}' after FAM, got %s", TokenStr(tok));

		if(tok->tok == '}')
			break;
	}

	if(TokenFetch() != ';')
		TokenError(tok, "Expected ';', got %s", TokenStr(tok));

	TokenFetch();
	stmt->struc = struc;
	return stmt;
}

struct Statement *ParseFuncDecl()
{
	bool is_static = FALSE;
	bool is_inline = FALSE;

	if(tok->tok == TK_KWORD && tok->hash == KW_STATIC) {
		is_static = TRUE;
		TokenFetch();
	}

	if(tok->tok == TK_KWORD && tok->hash == KW_INLINE) {
		is_inline = TRUE;
		TokenFetch();
	}

	struct TokenList *list_head = toklist_head;
	struct Type ret_type;
	char *name = NULL;

	if(!ParseType(&ret_type)) {
		if(is_static || is_inline)
			TokenError(tok, "Expected function return type, got %s", TokenStr(tok));

		return NULL;
	}

	if(tok->tok != TK_IDENT) {
		if(is_static || is_inline)
			TokenError(tok, "Expected function name, got %s", TokenStr(tok));

		toklist_head = list_head;
		tok = list_head->tok;
		return NULL;
	}

	name = tok->str;

	if(TokenFetch() != '(') {
		if(is_static || is_inline)
			TokenError(tok, "Expected function arguments, got %s", TokenStr(tok));

		toklist_head = list_head;
		tok = list_head->tok;
		return NULL;
	}

	struct Statement *stmt = Alloc(sizeof(struct Statement));
	stmt->kind = STMT_FUNC_DECL;
	stmt->token = tok;
	struct Function *func_templ = Alloc(sizeof(struct Function));
	char *arg_names[8] = { NULL };
	func_templ->type = ret_type;
	TokenFetch();

	while(tok->tok != ')') {
		if(tok->tok == '...') {
			if(TokenFetch() != ')')
				TokenError(tok, "Expected ')', got %s", TokenStr(tok));

			func_templ->variadic = TRUE;
			break;
		}

		if(!ParseType(&func_templ->args[func_templ->count]))
			TokenError(tok, "Expected argument type, got %s", TokenStr(tok));

		if(TypeSize(func_templ->args[func_templ->count]) == 0)
			TokenError(tok, "Incomplete function argument type");

		if(tok->tok != TK_IDENT)
			TokenError(tok, "Expected argument name, got %s", TokenStr(tok));

		if(func_templ->count == FUNC_MAX_ARGC)
			TokenError(tok, "Too many function arguments");

		arg_names[func_templ->count++] = tok->str;
		TokenFetch();

		if(tok->tok == ',') {
			if(TokenFetch() == ')')
				TokenError(tok, "Expected argument, got %s", TokenStr(tok));

			continue;
		}

		if(tok->tok != ')')
			TokenError(tok, "Expected argument or ')', got %s", TokenStr(tok));
	}

	TokenFetch();
	uint64 func_idx = FunctionFind(func_templ);
	func_templ = FunctionGet(func_idx);
	struct Object *obj = GlblObjectTableFind(name);

	if(obj == NULL) {
		obj = Alloc(sizeof(struct Object));
		obj->name = name;
		obj->type = (struct Type) { KIND_FUNC, 1, func_idx };
		obj->count = 1;
		obj->is_const = TRUE;
		obj->func.is_static = is_static;
		obj->func.is_inline = is_inline;
		Assert(GlblObjectTableInsert(obj));
	} else {
		if(memcmp(&obj->type, &(struct Type) { KIND_FUNC, 1, func_idx }, sizeof(struct Type)))
			TokenError(tok, "Function template does not match original");

		if(obj->func.is_static != is_static || obj->func.is_inline != is_inline)
			TokenError(tok, "Function qualifiers do not match original");
	}

	stmt->func = obj;

	if(tok->tok == ';') {
		TokenFetch();
		return stmt;
	}

	if(tok->tok != '{')
		TokenError(tok, "Expected function body or '{', got %s", TokenStr(tok));

	if(obj->func.body != NULL)
		TokenError(tok, "Redefinition of function %s", name);

	ScopePush();

	for(uint64 i = 0; i < func_templ->count; i++) {
		struct Object *arg_obj = Alloc(sizeof(struct Object));
		arg_obj->name = arg_names[i];
		arg_obj->type = func_templ->args[i];
		arg_obj->count = 1;
		obj->func.args[i] = arg_obj;

		if(!ScopeInsert(arg_obj))
			TokenError(tok, "Could not create object for argument %s", arg_names[i]);
	}

	obj->func.body = ParseStatement();
	ScopePop();
	return stmt;
}

struct Statement *ParseEnumDecl()
{
	if(tok->tok != TK_KWORD || tok->hash != KW_ENUM)
		return NULL;

	struct Statement *stmt = Alloc(sizeof(struct Statement));
	stmt->kind = STMT_ENUM_DECL;
	stmt->token = tok;

	if(TokenFetch() != '{')
		TokenError(tok, "Expected '{' after enum, got %s", TokenStr(tok));

	TokenFetch();
	uint64 val = 0;

	while(tok->tok != '}') {
		if(tok->tok != TK_IDENT)
			TokenError(tok, "Expected name of enumeration constant, got %s", TokenStr(tok));

		if(!GlblEnumTableInsert(tok->str, val++))
			TokenError(tok, "Enumeration constant %s already exists", tok->str);

		TokenFetch();
	}

	if(TokenFetch() != ';')
		TokenError(tok, "Expected ';', got %s", TokenStr(tok));

	TokenFetch();
	return stmt;
}

void ParseIncludes()
{
	while(TRUE) {
		if(tok->tok != TK_KWORD || tok->hash != KW_INCLUDE)
			break;

		if(TokenFetch() != TK_STR)
			TokenError(tok, "Expected include file name as string, got %s", TokenStr(tok));

		char *file_name = tok->str;

		if(TokenFetch() != ';')
			TokenError(tok, "Expected ';', got %s", TokenStr(tok));

		if(!LexFile(file_name))
			TokenError(tok, "File %s can't be included", file_name);

		TokenFetch();
	}
}

struct Statement *ParseDecl()
{
	struct Statement *stmt = ParseStructDecl();

	if(stmt == NULL)
		stmt = ParseEnumDecl();

	if(stmt == NULL)
		stmt = ParseFuncDecl();

	if(stmt == NULL)
		stmt = ParseVarDecl(FALSE);

	return stmt;
}

struct Statement *Parse()
{
	ParseIncludes();
	struct Statement *stmt = ParseDecl();
	struct Statement *cur = stmt;

	while(cur != NULL) {
		ParseIncludes();
		cur->next = ParseDecl();
		cur = cur->next;
	}

	if(tok->tok != TK_EOF)
		TokenError(tok, "Expected declaration or end of file, got %s", TokenStr(tok));

	return stmt;
}

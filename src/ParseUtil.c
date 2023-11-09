#include "Parse.h"


#define GLBL_OBJECT_TABLE_SIZE 2048


struct Scope
{
	struct Scope     *next;
	uint64           count;
	struct Object *objects[64];
};


static struct Scope *scope_head = NULL;
static struct Object *glbl_object_tab[GLBL_OBJECT_TABLE_SIZE] = { NULL };


static void Indent(uint64 ind)
{
	for(uint64 i = 0; i < ind; i++)
		printf("    ");
}

static void StatementPrintInd(struct Statement *stmt, uint64 ind)
{
	Indent(ind);

	if(stmt == NULL) {
		printf("(nil)");
		return;
	}

	if(stmt->label != NULL)
		printf("%s: ", stmt->label);

	switch(stmt->kind)
	{
	case STMT_COMPOUND:
		printf("{\n");
		StatementPrintInd(stmt->compound, ind + 1);
		Indent(ind);
		printf("}\n");
		break;
	case STMT_EXPRESSION:
		ExprNodePrint(stmt->expr);
		printf(";\n");
		break;
	case STMT_CONDITIONAL:
		printf("if(");
		ExprNodePrint(stmt->conditional.condition);
		printf(")\n");
		StatementPrintInd(stmt->conditional.br_true, ind + 1);

		if(stmt->conditional.br_false != NULL) {
			Indent(ind);
			printf("else\n");
			StatementPrintInd(stmt->conditional.br_false, ind + 1);
		}

		break;
	case STMT_SELECTION: {
		printf("switch(");
		ExprNodePrint(stmt->selection.expr);
		printf(")\n");
		Indent(ind);
		printf("{\n");
		struct Statement *cur = stmt->selection.case_list;

		while(cur != NULL) {
			Indent(ind);
			printf("case %lu:\n", cur->case_label);
			struct Statement *next = cur->next;
			cur->next = NULL;
			StatementPrintInd(cur, ind + 1);
			cur->next = next;
			cur = next;
		}

		Indent(ind);
		printf("}\n");
		break;
	}
	case STMT_WHILE:
		printf("while(");
		ExprNodePrint(stmt->while_loop.condition);
		printf(")\n");
		StatementPrintInd(stmt->while_loop.br_true, ind + 1);
		break;
	case STMT_DO_WHILE:
		printf("do\n");
		StatementPrintInd(stmt->while_loop.br_true, ind + 1);
		Indent(ind);
		printf("while(");
		ExprNodePrint(stmt->while_loop.condition);
		printf(");\n");
		break;
	case STMT_FOR:
		printf("{\n");
		StatementPrintInd(stmt->for_loop.preloop, ind + 1);
		Indent(ind + 1);
		printf("while(");
		ExprNodePrint(stmt->for_loop.condition);
		printf(") {\n");
		StatementPrintInd(stmt->for_loop.br_true, ind + 2);
		StatementPrintInd(stmt->for_loop.postloop, ind + 2);
		Indent(ind + 1);
		printf("}\n");
		break;
	case STMT_GOTO:
		printf("goto %s;\n", stmt->goto_label);
		break;
	case STMT_CONTINUE:
		printf("continue;\n");
		break;
	case STMT_BREAK:
		printf("break;\n");
		break;
	case STMT_RETURN:
		printf("return");

		if(stmt->expr != NULL) {
			printf(" ");
			ExprNodePrint(stmt->expr);
		}

		printf(";\n");
		break;
	case STMT_VAR_DECL:
		printf("%s %s", TypeStr(stmt->var_decl.obj->type), stmt->var_decl.obj->name);

		if(stmt->var_decl.obj->count > 1)
			printf("[%lu]", stmt->var_decl.obj->count);

		if(stmt->var_decl.expr != NULL || stmt->var_decl.obj->is_const) {
			printf(" = ");
			struct ExprList *cur = NULL;

			if(stmt->var_decl.expr != NULL) {
				cur = stmt->var_decl.expr;
			} else if(stmt->var_decl.obj->expr != NULL) {
				cur = stmt->var_decl.obj->expr;
			} else {
				printf("<buf>;\n");
				break;
			}

			if(cur->next == NULL) {
				ExprNodePrint(cur->node);
				printf(";\n");
				break;
			}

			printf("{ ");

			while(cur != NULL) {
				ExprNodePrint(cur->node);

				if(cur->next != NULL)
					printf(", ");

				cur = cur->next;
			}

			printf(" }");
		}

		printf(";\n");
		break;
	case STMT_FUNC_DECL:
		if(stmt->func->func.is_static)
			printf("static ");

		if(stmt->func->func.is_inline)
			printf("inline ");

		printf("%s %s", TypeStr(stmt->func->type), stmt->func->name);

		if(stmt->func->func.body != NULL) {
			printf("\n");
			StatementPrintInd(stmt->func->func.body, ind);
		} else {
			printf(";\n");
		}

		break;
	case STMT_STRUC_DECL:
		printf("struct %s", stmt->struc->name);

		if(stmt->struc->count == 0) {
			printf(";\n");
			break;
		}

		printf(" {\n");

		for(uint64 i = 0; i < stmt->struc->count; i++) {
			struct Member *memb = &stmt->struc->members[i];
			Indent(ind + 1);
			printf("%s", TypeStr(memb->type));

			if(memb->name != NULL)
				printf(" %s", memb->name);

			if(memb->count == 0)
				printf("[]");

			if(memb->count > 1)
				printf("[%lu]", memb->count);

			printf(";\n");
		}

		Indent(ind);
		printf("}\n");
		break;
	}

	if(stmt->next != NULL)
		StatementPrintInd(stmt->next, ind);
}

void StatementPrint(struct Statement *stmt)
{
	StatementPrintInd(stmt, 0);
}

void ExprNodePrint(struct ExprNode *node)
{
	if(node == NULL) {
		printf("(nil)");
		return;
	}

	printf("%s(", TypeStr(node->type));

	switch(node->kind)
	{
	case EXPR_CONST:
		printf("%lu", node->num);
		break;
	case EXPR_OBJECT:
		printf("%s", node->obj->name);
		break;
	case EXPR_FUNCALL: {
		ExprNodePrint(node->lhs);
		printf("(");
		struct ExprList *cur = node->list;

		while(cur != NULL) {
			ExprNodePrint(cur->node);
			cur = cur->next;

			if(cur != NULL)
				printf(", ");
		}

		printf(")");
		break;
	}
	case EXPR_CAST:
		printf("%s(", TypeStr(node->type));
		ExprNodePrint(node->lhs);
		printf(")");
		break;
	case EXPR_POST_INC:
		ExprNodePrint(node->lhs);
		printf("++");
		break;
	case EXPR_POST_DEC:
		ExprNodePrint(node->lhs);
		printf("--");
		break;
	case EXPR_MEMBER:
		ExprNodePrint(node->lhs);
		printf(".%s", node->member);
		break;
	case EXPR_MEMBER_DREF:
		ExprNodePrint(node->lhs);
		printf("->%s", node->member);
		break;
	case EXPR_NEG:
		printf("-");
		ExprNodePrint(node->lhs);
		break;
	case EXPR_NOT:
		printf("~");
		ExprNodePrint(node->lhs);
		break;
	case EXPR_LNOT:
		printf("!");
		ExprNodePrint(node->lhs);
		break;
	case EXPR_ADDROF:
		printf("&");
		ExprNodePrint(node->lhs);
		break;
	case EXPR_DREF:
		printf("*");
		ExprNodePrint(node->lhs);
		break;
	case EXPR_PRE_INC:
		printf("++");
		ExprNodePrint(node->lhs);
		break;
	case EXPR_PRE_DEC:
		printf("--");
		ExprNodePrint(node->lhs);
		break;
	case EXPR_TERNARY:
		printf("(");
		ExprNodePrint(node->tern);
		printf(" ? ");
		ExprNodePrint(node->lhs);
		printf(" : ");
		ExprNodePrint(node->rhs);
		printf(")");
		break;
	default:
		printf("(");
		ExprNodePrint(node->lhs);
		printf(" %s ", ExprNodeStr(node->kind));
		ExprNodePrint(node->rhs);
		printf(")");
		break;
	}

	printf(")");
}

char *ExprNodeStr(uint64 kind)
{
	switch(kind)
	{
	case EXPR_SHL:    return "<<";
	case EXPR_SHR:    return ">>";
	case EXPR_MUL:    return "*";
	case EXPR_DIV:    return "/";
	case EXPR_MOD:    return "%";
	case EXPR_AND:    return "&";
	case EXPR_XOR:    return "^";
	case EXPR_OR:     return "|";
	case EXPR_ADD:    return "+";
	case EXPR_SUB:    return "-";
	case EXPR_LT:     return "<";
	case EXPR_LTE:    return "<=";
	case EXPR_GT:     return ">";
	case EXPR_GTE:    return ">=";
	case EXPR_EQ:     return "==";
	case EXPR_NEQ:    return "!=";
	case EXPR_LAND:   return "&&";
	case EXPR_LOR:    return "||";
	case EXPR_ASSIGN: return "=";
	}

	return NULL;

}

uint64 ExprNodeFromToken(uint64 token)
{
	switch(token)
	{
	case '<<': return EXPR_SHL;
	case '>>': return EXPR_SHR;
	case '*':  return EXPR_MUL;
	case '/':  return EXPR_DIV;
	case '%':  return EXPR_MOD;
	case '&':  return EXPR_AND;
	case '^':  return EXPR_XOR;
	case '|':  return EXPR_OR;
	case '+':  return EXPR_ADD;
	case '-':  return EXPR_SUB;
	case '<':  return EXPR_LT;
	case '<=': return EXPR_LTE;
	case '>':  return EXPR_GT;
	case '>=': return EXPR_GTE;
	case '==': return EXPR_EQ;
	case '!=': return EXPR_NEQ;
	case '&&': return EXPR_LAND;
	case '||': return EXPR_LOR;
	}

	return -1ULL;
}

uint64 ExprNodePrec(uint64 binop)
{
	switch(binop)
	{
	case EXPR_SHL:  return 7;
	case EXPR_SHR:  return 7;
	case EXPR_MUL:  return 6;
	case EXPR_DIV:  return 6;
	case EXPR_MOD:  return 6;
	case EXPR_AND:  return 5;
	case EXPR_XOR:  return 5;
	case EXPR_OR:   return 5;
	case EXPR_ADD:  return 4;
	case EXPR_SUB:  return 4;
	case EXPR_LT:   return 3;
	case EXPR_LTE:  return 3;
	case EXPR_GT:   return 3;
	case EXPR_GTE:  return 3;
	case EXPR_EQ:   return 2;
	case EXPR_NEQ:  return 2;
	case EXPR_LAND: return 1;
	case EXPR_LOR:  return 0;
	}

	return -1ULL;
}

bool ExprNodeConst(struct ExprNode *node)
{
	switch(node->kind)
	{
	case EXPR_CONST:
		return TRUE;
	case EXPR_OBJECT:
		return node->obj->is_const;
	case EXPR_FUNCALL:
	case EXPR_POST_INC:
	case EXPR_POST_DEC:
	case EXPR_MEMBER_DREF:
	case EXPR_ADDROF:
	case EXPR_DREF:
	case EXPR_PRE_INC:
	case EXPR_PRE_DEC:
	case EXPR_ASSIGN:
		return FALSE;
	case EXPR_MEMBER:
	case EXPR_NEG:
	case EXPR_NOT:
	case EXPR_LNOT:
	case EXPR_CAST:
		return ExprNodeConst(node->lhs);
	case EXPR_TERNARY:
		return ExprNodeConst(node->lhs) && ExprNodeConst(node->rhs) && ExprNodeConst(node->tern);
	default:
		return ExprNodeConst(node->lhs) && ExprNodeConst(node->rhs);
	}
}

bool ExprNodeEvaluate(struct ExprNode *node, uint64 *res)
{
	uint64 tmp0 = 0;
	uint64 tmp1 = 0;

	switch(node->kind)
	{
	case EXPR_CONST:
		*res = node->num;
		return TRUE;
	case EXPR_CAST:
		return ExprNodeEvaluate(node->lhs, res);
	case EXPR_OBJECT:
		if(node->obj->is_const && node->obj->count == 1) {
			if(node->obj->expr != NULL)
				return ExprNodeEvaluate(node->obj->expr->node, res);
		}

		return FALSE;
	case EXPR_FUNCALL:
	case EXPR_POST_INC:
	case EXPR_POST_DEC:
	case EXPR_MEMBER_DREF:
	case EXPR_MEMBER:
	case EXPR_ADDROF:
	case EXPR_DREF:
	case EXPR_PRE_INC:
	case EXPR_PRE_DEC:
	case EXPR_ASSIGN:
		return FALSE;
	case EXPR_NEG:
		if(!ExprNodeEvaluate(node->lhs, res))
			return FALSE;

		*res = -*res;
		return TRUE;
	case EXPR_NOT:
		if(!ExprNodeEvaluate(node->lhs, res))
			return FALSE;

		*res = ~*res;
		return TRUE;
	case EXPR_LNOT:
		if(!ExprNodeEvaluate(node->lhs, res))
			return FALSE;

		*res = !*res;
		return TRUE;
	case EXPR_TERNARY:
		if(!ExprNodeEvaluate(node->tern, res))
			return FALSE;

		if(!ExprNodeEvaluate(node->lhs, &tmp0))
			return FALSE;

		if(!ExprNodeEvaluate(node->rhs, &tmp1))
			return FALSE;

		*res = *res ? tmp0 : tmp1;
		return TRUE;
	}

	if(!ExprNodeEvaluate(node->lhs, &tmp0))
		return FALSE;

	if(!ExprNodeEvaluate(node->lhs, &tmp1))
		return FALSE;

	if(tmp1 == 0 && (node->kind == EXPR_DIV || node->kind == EXPR_MOD))
		TokenError(node->token, "Division by zero is undefined");

	switch(node->kind)
	{
	case EXPR_SHL:  *res = tmp0 << tmp1; break;
	case EXPR_SHR:  *res = tmp0 >> tmp1; break;
	case EXPR_MUL:  *res = tmp0 *  tmp1; break;
	case EXPR_DIV:  *res = tmp0 /  tmp1; break;
	case EXPR_MOD:  *res = tmp0 %  tmp1; break;
	case EXPR_ADD:  *res = tmp0 +  tmp1; break;
	case EXPR_SUB:  *res = tmp0 -  tmp1; break;
	case EXPR_AND:  *res = tmp0 &  tmp1; break;
	case EXPR_XOR:  *res = tmp0 ^  tmp1; break;
	case EXPR_OR:   *res = tmp0 |  tmp1; break;
	case EXPR_LT:   *res = tmp0 <  tmp1; break;
	case EXPR_LTE:  *res = tmp0 <= tmp1; break;
	case EXPR_GT:   *res = tmp0 >  tmp1; break;
	case EXPR_GTE:  *res = tmp0 >= tmp1; break;
	case EXPR_EQ:   *res = tmp0 == tmp1; break;
	case EXPR_NEQ:  *res = tmp0 != tmp1; break;
	case EXPR_LAND: *res = tmp0 && tmp1; break;
	case EXPR_LOR:  *res = tmp0 || tmp1; break;
	}

	return TRUE;
}

void ScopePush()
{
	struct Scope *scope = Alloc(sizeof(struct Scope));
	scope->next = scope_head;
	scope_head = scope;
}

bool ScopeInsert(struct Object *obj)
{
	if(scope_head == NULL)
		return GlblObjectTableInsert(obj);

	Assert(scope_head->count < 64);

	if(ScopeFind(obj->name) != NULL)
		return FALSE;

	scope_head->objects[scope_head->count++] = obj;
	return TRUE;
}

struct Object *ScopeFind(char *name)
{
	struct Scope *scope = scope_head;

	while(scope != NULL) {
		for(uint64 i = 0; i < scope->count; i++) {
			if(!strcmp(scope->objects[i]->name, name))
				return scope->objects[i];
		}

		scope = scope->next;
	}

	return NULL;
}

void ScopePop()
{
	Assert(scope_head != NULL);
	scope_head = scope_head->next;
}

bool GlblObjectTableInsert(struct Object *obj)
{
	uint64 hash = Hash(obj->name, strlen(obj->name)) % GLBL_OBJECT_TABLE_SIZE;
	struct Object *cur = glbl_object_tab[hash];

	if(cur == NULL) {
		glbl_object_tab[hash] = obj;
		return TRUE;
	}

	do {
		if(!strcmp(cur->name, obj->name))
			return FALSE;

		if(cur->next == NULL)
			break;

		cur = cur->next;
	} while(1);

	cur->next = obj;
	return TRUE;
}

struct Object *GlblObjectTableFind(char *name)
{
	uint64 hash = Hash(name, strlen(name)) % GLBL_OBJECT_TABLE_SIZE;
	struct Object *cur = glbl_object_tab[hash];

	while(cur != NULL) {
		if(!strcmp(cur->name, name))
			return cur;

		cur = cur->next;
	}

	return NULL;
}

struct Object **GlblObjectTable()
{
	return glbl_object_tab;
}

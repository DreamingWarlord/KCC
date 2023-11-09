#include "Parse.h"


#define MAX_GOTOS 256


static struct Function *ctx_func = NULL;
static char *ctx_labels[MAX_GOTOS] = { NULL };
static uint64 ctx_label_count = 0;
static char *ctx_gotos[MAX_GOTOS] = { NULL };
static uint64 ctx_goto_count = 0;
static uint64 ctx_goto_match_count = 0;
static uint64 ctx_switch_count = 0;
static uint64 ctx_loop_count = 0;


bool ExprNodeRef(struct ExprNode *node)
{
	switch(node->kind)
	{
	case EXPR_OBJECT:
		return !node->obj->is_const;
	case EXPR_TERNARY:
		return ExprNodeRef(node->lhs) && ExprNodeRef(node->rhs);
	case EXPR_MEMBER:
	case EXPR_MEMBER_DREF:
	case EXPR_DREF:
		return TRUE;
	}

	return FALSE;
}

void ExprNodeBuildType(struct ExprNode *node)
{
	if(node == NULL)
		return;

	if(node->type.kind != KIND_VOID || node->type.ptrc > 0)
		return;

	switch(node->kind)
	{
	case EXPR_FUNCALL:
		ExprNodeBuildType(node->lhs);

		if(node->lhs->type.kind != KIND_FUNC || node->lhs->type.ptrc != 1)
			TokenError(node->token, "Cannot call a non-function pointer");

		struct Function *func = FunctionGet(node->lhs->type.idx);
		node->type = func->type;
		struct ExprList *cur = node->list;
		uint64 argc = 0;

		while(cur != NULL) {
			if(argc >= func->count && !func->variadic)
				TokenError(cur->node->token, "Too many arguments passed to function call");

			ExprNodeBuildType(cur->node);

			if(argc < func->count && !TypeCastable(cur->node->type, func->args[argc]))
				TokenError(cur->node->token, "Function call argument type does not match");

			cur = cur->next;
			argc++;
		}

		break;
	case EXPR_CAST:
		ExprNodeBuildType(node->lhs);

		if(!TypeCastable(node->lhs->type, node->type))
			TokenError(node->token, "Can't cast %s to %s", TypeStr(node->lhs->type), TypeStr(node->type));

		break;
	case EXPR_CAST_EXT:
		ExprNodeBuildType(node->lhs);

		if(!TypeCastable(node->lhs->type, node->type))
			TokenError(node->token, "Can't cast %s to %s", TypeStr(node->lhs->type), TypeStr(node->type));

		if(TypeSize(node->lhs->type) >= TypeSize(node->type))
			TokenError(node->token, "Sign extended cast must be only done when upscaling a type");

		break;
	case EXPR_POST_INC:
	case EXPR_POST_DEC:
	case EXPR_PRE_INC:
	case EXPR_PRE_DEC:
		ExprNodeBuildType(node->lhs);
		node->type = node->lhs->type;

		if(node->lhs->type.ptrc == 0 && !TypeInt(node->lhs->type))
			TokenError(node->token, "Cannot increment non integer / pointer type");

		if(!ExprNodeRef(node->lhs))
			TokenError(node->token, "Cannot increment rvalue");

		break;
	case EXPR_MEMBER:
	case EXPR_MEMBER_DREF:
		ExprNodeBuildType(node->lhs);

		if(node->kind == EXPR_MEMBER_DREF) {
			if(node->lhs->type.ptrc != 1 || node->lhs->type.kind != KIND_STRUCT)
				TokenError(node->token, "Cannot take member of non-struct value");
		} else {
			if(node->lhs->type.ptrc != 0 || node->lhs->type.kind != KIND_STRUCT)
				TokenError(node->token, "Cannot take member of non-struct value");
		}

		struct Struct *struc = StructGet(node->lhs->type.idx);
		struct Member *memb = NULL;

		for(uint64 i = 0; i < struc->count; i++) {
			if(!strcmp(struc->members[i].name, node->member)) {
				memb = &struc->members[i];
				break;
			}
		}

		if(memb == NULL)
			TokenError(node->token, "Struct %s does not contain %s", struc->name, node->member);

		node->type = memb->type;

		if(memb->count != 1)
			node->type.ptrc++;

		break;
	case EXPR_NEG:
	case EXPR_NOT:
	case EXPR_LNOT:
		ExprNodeBuildType(node->lhs);
		node->type = node->lhs->type;
		break;
	case EXPR_ADDROF:
		ExprNodeBuildType(node->lhs);

		if(!ExprNodeRef(node->lhs))
			TokenError(node->token, "Cannot take the address of an rvalue");

		node->type = node->lhs->type;
		node->type.ptrc++;
		break;
	case EXPR_DREF:
		ExprNodeBuildType(node->lhs);

		if(node->lhs->type.ptrc == 0)
			TokenError(node->token, "Cannot dereference non pointer type");

		if(node->lhs->type.kind == KIND_VOID)
			TokenError(node->token, "Cannot dereference void");

		if(node->lhs->type.kind == KIND_FUNC && node->lhs->type.ptrc == 1)
			TokenError(node->token, "Cannot dereference function pointer");

		node->type = node->lhs->type;
		node->type.ptrc--;
		break;
	case EXPR_ASSIGN:
		ExprNodeBuildType(node->lhs);
		ExprNodeBuildType(node->rhs);

		if(!ExprNodeRef(node->lhs))
			TokenError(node->token, "Cannot assign to an rvalue");

		if(!TypeCastable(node->lhs->type, node->rhs->type))
			TokenError(node->token, "Incompatible types when assigning");

		node->type = node->lhs->type;
		break;
	case EXPR_TERNARY:
		ExprNodeBuildType(node->lhs);
		ExprNodeBuildType(node->rhs);
		ExprNodeBuildType(node->tern);

		if(!TypeCastable(node->lhs->type, node->rhs->type))
			TokenError(node->token, "Both sides of ternary operator must be the same type");

		node->type = node->lhs->type;
		break;
	case EXPR_SHL...EXPR_OR:
		ExprNodeBuildType(node->lhs);
		ExprNodeBuildType(node->rhs);

		if(!TypeCastable(node->lhs->type, node->rhs->type))
			TokenError(node->token, "Incompatible types in binary operation");

		if(node->lhs->type.ptrc == 0 && !TypeInt(node->lhs->type))
			TokenError(node->token, "Binary operations are only doable on pointers or integers");

		node->type = node->lhs->type;
		break;
	case EXPR_LT...EXPR_NEQ:
		ExprNodeBuildType(node->lhs);
		ExprNodeBuildType(node->rhs);

		if(!TypeCastable(node->lhs->type, node->rhs->type))
			TokenError(node->token, "Incompatible types in comparison");

		if(node->lhs->type.ptrc == 0 && !TypeInt(node->lhs->type))
			TokenError(node->token, "Comparison is only doable on pointers or integers");

		node->type = node->lhs->type;
		break;
	case EXPR_LAND...EXPR_LOR:
		ExprNodeBuildType(node->lhs);
		ExprNodeBuildType(node->rhs);

		if(!TypeInt(node->lhs->type) || !TypeInt(node->rhs->type))
			TokenError(node->token, "Both sides of logic operation must be integers");

		node->type = (struct Type) { KIND_INT64, 0, 0 };
		break;
	}
}

void StatementValidate(struct Statement *stmt)
{
	if(stmt == NULL)
		return;

	if(stmt->label != NULL) {
		ctx_labels[ctx_label_count++] = stmt->label;

		for(uint64 i = 0; i < ctx_goto_count; i++) {
			if(!strcmp(ctx_gotos[i], stmt->label))
				ctx_goto_match_count++;
		}
	}

	switch(stmt->kind)
	{
	case STMT_COMPOUND:
		StatementValidate(stmt->compound);
		break;
	case STMT_EXPRESSION:
		ExprNodeBuildType(stmt->expr);
		break;
	case STMT_CONDITIONAL:
		ExprNodeBuildType(stmt->conditional.condition);
		StatementValidate(stmt->conditional.br_true);
		StatementValidate(stmt->conditional.br_false);
		break;
	case STMT_SELECTION:
		ExprNodeBuildType(stmt->selection.expr);
		ctx_switch_count++;
		StatementValidate(stmt->selection.case_list);
		ctx_switch_count--;
		break;
	case STMT_WHILE:
	case STMT_DO_WHILE:
		ExprNodeBuildType(stmt->while_loop.condition);
		ctx_loop_count++;
		StatementValidate(stmt->while_loop.br_true);
		ctx_loop_count--;
		break;
	case STMT_FOR:
		StatementValidate(stmt->for_loop.preloop);
		ExprNodeBuildType(stmt->for_loop.condition);
		StatementValidate(stmt->for_loop.postloop);
		StatementValidate(stmt->for_loop.br_true);
		break;
	case STMT_GOTO: {
		bool match = FALSE;

		for(uint64 i = 0; i < ctx_label_count; i++) {
			if(!strcmp(ctx_labels[i], stmt->goto_label)) {
				match = TRUE;
				break;
			}
		}

		if(!match)
			ctx_gotos[ctx_goto_count++] = stmt->goto_label;

		break;
	}
	case STMT_CONTINUE:
		if(ctx_loop_count == 0)
			TokenError(stmt->token, "`continue` can only be used in a loop scope");

		break;
	case STMT_BREAK:
		if(ctx_loop_count == 0 && ctx_switch_count == 0)
			TokenError(stmt->token, "`break` can only be used in a loop or switch scope");

		break;
	case STMT_RETURN:
		ExprNodeBuildType(stmt->expr);

		if(stmt->expr != NULL && !TypeCastable(stmt->expr->type, ctx_func->type))
			TokenError(stmt->token, "Return type doesn't match function signature");

		if(stmt->expr == NULL && (ctx_func->type.ptrc != 0 || ctx_func->type.kind != KIND_VOID))
			TokenError(stmt->token, "Return type doesn't match function signature");

		break;
	case STMT_VAR_DECL: {
		struct ExprList *cur = stmt->var_decl.expr;

		while(cur != NULL) {
			ExprNodeBuildType(cur->node);

			if(!TypeCastable(cur->node->type, stmt->var_decl.obj->type))
				TokenError(stmt->token, "Initialization type does not match variable's");

			cur = cur->next;
		}

		break;
	}
	case STMT_FUNC_DECL:
		ctx_func = FunctionGet(stmt->func->type.idx);
		StatementValidate(stmt->func->func.body);

		if(ctx_goto_match_count != ctx_goto_count)
			TokenError(stmt->token, "Unresolved goto statements in function body");

		ctx_label_count = 0;
		ctx_goto_match_count = 0;
		ctx_goto_count = 0;
		break;
	}

	StatementValidate(stmt->next);
}

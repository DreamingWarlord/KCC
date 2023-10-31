#include "Parse.h"
#include "Type.h"


int main(int argc, char *argv[])
{
	if(argc < 2) {
		printf("Expected source file name\n");
		return 1;
	}

	if(!LexFile(argv[1])) {
		printf("Can't parse source file %s\n", argv[1]);
		return 1;
	}

	TokenFetch();
	struct Statement *stmt = Parse();
	StatementPrint(stmt);
	return 0;
}

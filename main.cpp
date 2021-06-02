#include <fstream>
#include <iostream>
#include "ASTNode.h"

int errorLexFlag;
NBlock* programBlock;
int errorSyntaxFlag;

extern int yylineno;
extern int yylex(void);
extern int yyparse(void);
extern int yyerror(char*);

int main(int argc, char* argv[])
{
    yyparse();
}
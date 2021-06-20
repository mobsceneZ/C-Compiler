/*
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
*/

#include <iostream>
#include <fstream>
#include "ASTNode.h"
#include "BuildObj.h"

int errorLexFlag;
int errorSyntaxFlag;
NBlock* programBlock;
extern int yyparse();

int main(int argc, char **argv) {
    yyparse();

    BuildContext context;
    context.buildIR(*programBlock);
    
    legacy::PassManager passManager;
    passManager.add(createPrintModulePass(outs()));
    passManager.run(*context.themodule.get());
    
    buildObj(context);

    return 0;
}

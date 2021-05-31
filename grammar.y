%{
    #include <cstdio>
    #include <memory>
    #include <cstdlib>
    #include "ASTNode.h"

    shared_ptr<NBlock> programBlock;
    extern int errorLexFlag;
    extern int errorSyntaxFlag;
    extern int yylex();

%}

 /* Possible Type of terminals and non-terminals */
%union
{
    shared_ptr<NBlock> block;
    shared_ptr<NExpression> expr;
    shared_ptr<NStatement> stmt;
    shared_ptr<NIdentifier> ident;
    shared_ptr<NVariableDeclaration> var_decl;
    shared_ptr<vector<shared_ptr<NVariableDeclaration>>> varvec;
    shared_ptr<vector<shared_ptr<NExpression>>> exprvec;
    int token;
    shared_ptr<string> string;
}

%token <token> TPLUS TMINUS TMUL TDIV TMOD TLSHIFT TRSHIFT 
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE TAND TOR TXOR
%token <token> SEMI COMMA TLBRACKET TRBRACKET TLPAREN TRPAREN TEQUAL
%token <token> TLBRACE TRBRACE TRETURN TIF TWHILE TELSE TSTRUCT TDOT
%token <string> TIDENTIFIER TLITERAL TVDOUBLE TVINTEGER TEXTERN
%token <string> TYDOUBLE TYCHAR TYSTRING TYVOID TYFLOAT TYBOOL TYINT

%type <exprvec> Args
%type <var_decl> ParamDec
%type <varvec> FunDec VarList
%type <token> comparison arithmetic
%type <expr> expr assignment array_element
%type <block> Program ExtDefList CompSt DefList StmtList
%type <ident> Specifier identifier BASE_TYPE ARRAY_TYPE STRUCT_TYPE STRUCT_TAG
%type <stmt> var_declar struct_declar func_declar ExtDef Stmt if_stmt while_stmt


 /* Precedence and associativity */
%right TEQUAL
%left TOR
%left TAND
%left TCGE TCGT TCLE TCLT TCNE TCEQ
%left TPLUS TMINUS
%left TMUL TDIV TMOD
%left TLPAREN TRPAREN TLBRACKET TRBRACKET TDOT

%start Program

%%

Program : ExtDefList { programBlock = $1; }
        ;

ExtDefList : ExtDefList ExtDef { $1->AddStatement(shared_ptr<NStatement>($2)); $$ = $1; }
           | ExtDef {$$ = make_shared<NBlock>(); $$->AddStatement(shared_ptr<NStatement>($1)); }
           ;

ExtDef : var_declar { $$ = $1; }
       | struct_declar { $$ = $1; }
       | func_declar { $$ = $1; }
       | error SEMI { errorSyntaxFlag = 1; }
       ;

 /* Variable Declaration Starts */
 /* Supports ARRAY, STRUCT two complex type. */
 /* Supports int, double, string, float, char and etc. */
var_declar : Specifier identifier SEMI { $$ = make_shared<NVariableDeclaration>(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($2), nullptr); }
           | Specifier identifier TEQUAL expr SEMI { $$ = make_shared<NVariableDeclaration>(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($2), shared_ptr<NExpression>($4)); }
           | Specifier identifier TEQUAL TLBRACKET Args TRBRACKET SEMI { $$ = make_shared<NVariableDeclaration>(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($2), shared_ptr<ExpressionList>($5)); }
           | error SEMI { errorSyntaxFlag = 1; }
           ;

Specifier : BASE_TYPE { $$ = $1; }
          | ARRAY_TYPE { $$ = $1; }
          | STRUCT_TYPE { $$ = $1; }
          ;

BASE_TYPE : TYINT { $$ = make_shared<NIdentifier>(*$1); $$->SetType(); }
          | TYDOUBLE { $$ = make_shared<NIdentifier>(*$1); $$->SetType(); }
          | TYBOOL { $$ = make_shared<NIdentifier>(*$1); $$->SetType(); }
          | TYCHAR { $$ = make_shared<NIdentifier>(*$1); $$->SetType(); }
          | TYSTRING { $$ = make_shared<NIdentifier>(*$1); $$->SetType(); }
          | TYFLOAT { $$ = make_shared<NIdentifier>(*$1); $$->SetType(); }
          | TYVOID { $$ = make_shared<NIdentifier>(*$1); $$->SetType(); }
          ;

ARRAY_TYPE : ARRAY_TYPE TLBRACKET TVINTEGER TRBRACKET { $1->AddDimension(make_shared<NInteger>(stoll($3))); $$ = $1; }
           | BASE_TYPE TLBRACKET TVINTEGER TRBRACKET { $1->SetArray(); $1->AddDimension(make_shared<NInteger>(stoll($3))); $$ = $1; }
           ;

STRUCT_TYPE : TSTRUCT STRUCT_TAG { $2->SetType(); $$ = $2; }
            ;

STRUCT_TAG : TIDENTIFIER { $$ = make_shared<NIdentifier>(*$1); }
           ;

identifier : TIDENTIFIER { $$ = make_shared<NIdentifier>(*$1); }
           ;

 /* Variable Declaration Ends */

 /* Function Declaration Starts */
func_declar : Specifier identifier FunDec CompSt { $$ = make_shared<NFunctionDeclaration>(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($2), shared_ptr<VariableList>($3), shared_ptr<NBlock>($4)); }
            | TEXTERN Specifier identifier FunDec SEMI { $$ = make_shared<NFunctionDeclaration>(shared_ptr<NIdentifier>($2), shared_ptr<NIdentifier>($3), shared_ptr<VariableList>($4), nullptr, true); }
            ;

FunDec : TLPAREN TRPAREN { $$ = make_shared<VariableList>(); }
       | TLPAREN VarList TRPAREN { $$ = $2; }
       | error TRPAREN { errorSyntaxFlag = 1; }
       ;

VarList : VarList COMMA ParamDec { $1->push_back(shared_ptr<NVariableDeclaration>($3)); $$ = $1; }
        | ParamDec { $$ = make_shared<VariableList>(); $$->push_back(shared_ptr<NVariableDeclaration>($1)); }
        ;

ParamDec : Specifier identifier { $$ = make_shared<NVariableDeclaration>(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($2)); }
         ;

 /* which means the definition should be before statements. */
CompSt : TLBRACE DefList StmtList TRBRACE {
                                                $$ = make_shared<NBlock>();
                                                for(auto it=($2)->statements->begin();it!=($2)->statements->end(); it++)
                                                        $$->AddStatement(*it);
                                                for(auto it=($3)->statements->begin();it!=($3)->statements->end(); it++)
                                                        $$->AddStatement(*it);
                                          }
       | TLBRACE error TRBRACE { errorSyntaxFlag = 1; }
       ;

DefList : DefList var_declar { $1->AddStatement(shared_ptr<NStatement>($2)); $$ = $1; }
        | { $$ = make_shared<NBlock>(); }
        ;

StmtList : StmtList Stmt { $1->AddStatement(shared_ptr<NStatement>($2)); $$ = $1; }
         | { $$ = make_shared<NBlock>(); }
         ;

Stmt : expr SEMI { $$ = make_shared<NExpressionStatement>(shared_ptr<NExpression>($1)); }
     | TRETURN expr SEMI { $$ = make_shared<NReturnStatement>(shared_ptr<NExpression>($2)); }
     | if_stmt { $$ = $1; }
     | while_stmt { $$ = $1; }
     | error SEMI { errorSyntaxFlag = 1; }
     ;

if_stmt : TIF TLPAREN expr TRPAREN CompSt { $$ = make_shared<NIfStatement>(shared_ptr<NExpression>($3), shared_ptr<NBlock>($5)); }
        | TIF TLPAREN expr TRPAREN CompSt TELSE CompSt { $$ = make_shared<NIfStatement>(shared_ptr<NExpression>($3), shared_ptr<NBlock>($5), shared_ptr<NBlock>($7)); }
        | TIF TLPAREN expr TRPAREN CompSt TELSE if_stmt {
                                                                auto blk = make_shared<NBlock>();
                                                                blk->AddStatement(shared_ptr<NStatement>($7));
                                                                $$ = make_shared<NIfStatement>(shared_ptr<NExpression>($3), shared_ptr<NBlock>($5), shared_ptr<NBlock>(blk));
                                                        }
        ;

while_stmt : TWHILE TLPAREN expr TRPAREN CompSt  { $$ = make_shared<NWhileStatement>(shared_ptr<NBlock>($5), shared_ptr<NExpression>($3)); }

 /* Expression Part. */
expr : assignment { $$ = $1; }
     | identifier TLPAREN Args TRPAREN { $$ = make_shared<NMethodCall>(shared_ptr<NIdentifier>($1), shared_ptr<ExpressionList>($3)); }
     | identifier { $$ = $1; }
     | STRUCT_TAG TDOT identifier { $$ = make_shared<NStructMember>(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($3)); }
     | TVINTEGER { $$ = make_shared<NInteger>(stoll($1)); }
     | TVDOUBLE { $$ = make_shared<NDouble>(stod($1)); }
     | expr comparison expr { $$ = make_shared<NBinaryOperator>(($2), shared_ptr<NExpression>($1), shared_ptr<NExpression>($3)); }
     | expr arithmetic expr { $$ = make_shared<NBinaryOperator>(($2), shared_ptr<NExpression>($1), shared_ptr<NExpression>($3)); }
     | TLPAREN expr TRPAREN { $$ = $2; }
     | array_element { $$ = $1; }
     | TLITERAL { $$ = make_shared<NLiteral>(*$1); }
     ;

assignment : identifier TEQUAL expr { $$ = make_shared<NAssignment>(shared_ptr<NIdentifier>($1), shared_ptr<NExpression>($3)); }
           | array_element TEQUAL expr { $$ = make_shared<NArrayAssign>(shared_ptr<NArrayIndex>($1), shared_ptr<NExpression>($3)); }
           | STRUCT_TAG TDOT identifier TEQUAL expr {
                                                        auto member = make_shared<NStructMember>(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($3));
                                                        $$ = make_shared<NStructAssign>(shared_ptr<NStructMember>(member), shared_ptr<NExpression>($5));
                                                    }
           ;

comparison : TCEQ | TCNE | TCLT | TCLE | TCGE | TCGT
           | TAND | TOR | TXOR
           ;

arithmetic : TPLUS | TMINUS | TMUL | TDIV | TMOD
            | TLSHIFT | TRSHIFT
            ;

array_element : identifier TLBRACKET expr TRBRACKET { $$ = make_shared<NArrayIndex>(shared_ptr<NIdentifier>($1), shared_ptr<NExpression>($3)); }
              | array_element TLBRACKET expr TRBRACKET { $1->expressions->push_back(shared_ptr<NExpression>($3)); $$ = $1; }
              ;

Args : Args COMMA expr { $1->push_back(shared_ptr<NExpression>($3)); $$ = $1; }
     | expr { $$ = make_shared<ExpressionList>(); $$->push_back(shared_ptr<NExpression>($1)); }
     | { $$ = make_shared<ExpressionList>(); }
     ;

 /* Function Declaration Ends */

 /* Structure Declaration Starts */
struct_declar : TSTRUCT STRUCT_TAG TLBRACE DefList TRBRACE SEMI {
                                                                        auto var_list = make_shared<VariableList>();
                                                                        for(auto it=($4)->statements->begin(); it!=($4)->statements->end(); it++)
                                                                                var_list->push_back(shared_ptr<NVariableDeclaration>(*it));
                                                                        $$ = make_shared<NStructDeclaration>(shared_ptr<NIdentifier>($2), shared_ptr<VariableList>(var_list));
                                                                }
              ;

 /* Structure Declaration Ends */

 %%

void yyerror(char* msg)
{
    if(errorLexFlag==0)
        fprintf(stderr, "Error at line %d: %s\n", yylineno, msg);
}
%locations

%{
    #include <cstdio>
    #include <memory>
    #include <cstdlib>
    #include "ASTNode.h"

    extern NBlock* programBlock;
    extern int errorLexFlag;
    extern int errorSyntaxFlag;
    extern int yylineno;
    extern int yylex();
    void yyerror(char* msg);
%}

 /* Possible Type of terminals and non-terminals */
%union
{
    NBlock* block;
    NExpression* expr;
    NStatement* stmt;
    NIdentifier* ident;
    NVariableDeclaration* var_decl;
    vector<shared_ptr<NVariableDeclaration>>* varvec;
    vector<shared_ptr<NExpression>>* exprvec;
    int token;
    string* strings;
    NArrayIndex* array_index;
}

%token <token> TPLUS TMINUS TMUL TDIV TMOD TLSHIFT TRSHIFT 
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE TAND TOR TXOR
%token <token> SEMI COMMA TLBRACKET TRBRACKET TLPAREN TRPAREN TEQUAL
%token <token> TLBRACE TRBRACE TRETURN TIF TWHILE TELSE TSTRUCT TDOT
%token <strings> TIDENTIFIER TLITERAL TVDOUBLE TVINTEGER TEXTERN
%token <strings> TYDOUBLE TYCHAR TYSTRING TYVOID TYFLOAT TYBOOL TYINT

%type <exprvec> Args
%type <var_decl> ParamDec
%type <expr> expr assignment
%type <varvec> FunDec VarList
%type <array_index> array_element
%type <token> comparison arithmetic
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
%left TLSHIFT TRSHIFT
%left TLPAREN TRPAREN TLBRACKET TRBRACKET TDOT

%start Program

%%

Program : ExtDefList { programBlock = $1; }
        ;

ExtDefList : ExtDefList ExtDef { $1->AddStatement(shared_ptr<NStatement>($2)); $$ = $1; }
           | ExtDef {$$ = new NBlock(); $$->AddStatement(shared_ptr<NStatement>($1)); }
           ;

ExtDef : var_declar { $$ = $1; }
       | struct_declar { $$ = $1; }
       | func_declar { $$ = $1; }
       | error SEMI { errorSyntaxFlag = 1; }
       ;

 /* Variable Declaration Starts */
 /* Supports ARRAY, STRUCT two complex type. */
 /* Supports int, double, string, float, char and etc. */
var_declar : Specifier identifier SEMI { $$ = new NVariableDeclaration(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($2)); }
           | Specifier identifier TEQUAL expr SEMI { $$ = new NVariableDeclaration(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($2), shared_ptr<NExpression>($4)); }
           | Specifier identifier TEQUAL TLBRACKET Args TRBRACKET SEMI { $$ = new NVariableDeclaration(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($2), shared_ptr<ExpressionList>($5)); }
           | error SEMI { errorSyntaxFlag = 1; }
           ;

Specifier : BASE_TYPE { $$ = $1; }
          | ARRAY_TYPE { $$ = $1; }
          | STRUCT_TYPE { $$ = $1; }
          ;

BASE_TYPE : TYINT { $$ = new NIdentifier(*$1); $$->SetType(); delete $1; }
          | TYDOUBLE { $$ = new NIdentifier(*$1); $$->SetType(); delete $1; }
          | TYBOOL { $$ = new NIdentifier(*$1); $$->SetType(); delete $1; }
          | TYCHAR { $$ = new NIdentifier(*$1); $$->SetType(); delete $1; }
          | TYSTRING { $$ = new NIdentifier(*$1); $$->SetType(); delete $1; }
          | TYFLOAT { $$ = new NIdentifier(*$1); $$->SetType(); delete $1; }
          | TYVOID { $$ = new NIdentifier(*$1); $$->SetType(); delete $1; }
          ;

ARRAY_TYPE : ARRAY_TYPE TLBRACKET TVINTEGER TRBRACKET { $1->AddDimension(make_shared<NInteger>(stoll(*$3))); delete $3; $$ = $1; }
           | BASE_TYPE TLBRACKET TVINTEGER TRBRACKET { $1->SetArray(); $1->AddDimension(make_shared<NInteger>(stoll(*$3))); delete $3; $$ = $1; }
           ;

STRUCT_TYPE : TSTRUCT STRUCT_TAG { $2->SetType(); $$ = $2; }
            ;

STRUCT_TAG : TIDENTIFIER { $$ = new NIdentifier(*$1); delete $1; }
           ;

identifier : TIDENTIFIER { $$ = new NIdentifier(*$1); delete $1; }
           ;

 /* Variable Declaration Ends */

 /* Function Declaration Starts */
func_declar : Specifier identifier FunDec CompSt { $$ = new NFunctionDeclaration(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($2), shared_ptr<VariableList>($3), shared_ptr<NBlock>($4)); }
            | TEXTERN Specifier identifier FunDec SEMI { $$ = new NFunctionDeclaration(shared_ptr<NIdentifier>($2), shared_ptr<NIdentifier>($3), shared_ptr<VariableList>($4), nullptr, true); delete $1; }
            ;

FunDec : TLPAREN TRPAREN { $$ = new VariableList(); }
       | TLPAREN VarList TRPAREN { $$ = $2; }
       | error TRPAREN { errorSyntaxFlag = 1; }
       ;

VarList : VarList COMMA ParamDec { $1->push_back(shared_ptr<NVariableDeclaration>($3)); $$ = $1; }
        | ParamDec { $$ = new VariableList(); $$->push_back(shared_ptr<NVariableDeclaration>($1)); }
        ;

ParamDec : Specifier identifier { $$ = new NVariableDeclaration(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($2)); }
         ;

 /* which means the definition should be before statements. */
CompSt : TLBRACE DefList StmtList TRBRACE {
                                                $$ = new NBlock();
                                                for(auto it=($2)->statements->begin();it!=($2)->statements->end(); it++)
                                                        $$->AddStatement(*it);
                                                for(auto it=($3)->statements->begin();it!=($3)->statements->end(); it++)
                                                        $$->AddStatement(*it);
                                          }
       | TLBRACE error TRBRACE { errorSyntaxFlag = 1; }
       ;

DefList : DefList var_declar { $1->AddStatement(shared_ptr<NStatement>($2)); $$ = $1; }
        | { $$ = new NBlock(); }
        ;

StmtList : StmtList Stmt { $1->AddStatement(shared_ptr<NStatement>($2)); $$ = $1; }
         | { $$ = new NBlock(); }
         ;

Stmt : expr SEMI { $$ = new NExpressionStatement(shared_ptr<NExpression>($1)); }
     | TRETURN expr SEMI { $$ = new NReturnStatement(shared_ptr<NExpression>($2)); }
     | if_stmt { $$ = $1; }
     | while_stmt { $$ = $1; }
     | error SEMI { errorSyntaxFlag = 1; }
     ;

if_stmt : TIF TLPAREN expr TRPAREN CompSt { $$ = new NIfStatement(shared_ptr<NExpression>($3), shared_ptr<NBlock>($5)); }
        | TIF TLPAREN expr TRPAREN CompSt TELSE CompSt { $$ = new NIfStatement(shared_ptr<NExpression>($3), shared_ptr<NBlock>($5), shared_ptr<NBlock>($7)); }
        | TIF TLPAREN expr TRPAREN CompSt TELSE if_stmt {
                                                                auto blk = new NBlock();
                                                                blk->AddStatement(shared_ptr<NStatement>($7));
                                                                $$ = new NIfStatement(shared_ptr<NExpression>($3), shared_ptr<NBlock>($5), shared_ptr<NBlock>(blk));
                                                        }
        ;

while_stmt : TWHILE TLPAREN expr TRPAREN CompSt  { $$ = new NWhileStatement(shared_ptr<NBlock>($5), shared_ptr<NExpression>($3)); }

 /* Expression Part. */
expr : assignment { $$ = $1; }
     | identifier TLPAREN Args TRPAREN { $$ = new NMethodCall(shared_ptr<NIdentifier>($1), shared_ptr<ExpressionList>($3)); }
     | identifier { $$ = $1; }
     | STRUCT_TAG TDOT identifier { $$ = new NStructMember(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($3)); }
     | TVINTEGER { $$ = new NInteger(stoll(*$1)); delete $1; }
     | TVDOUBLE { $$ = new NDouble(stod(*$1)); delete $1; }
     | expr comparison expr { $$ = new NBinaryOperator(($2), shared_ptr<NExpression>($1), shared_ptr<NExpression>($3)); }
     | expr arithmetic expr { $$ = new NBinaryOperator(($2), shared_ptr<NExpression>($1), shared_ptr<NExpression>($3)); }
     | TLPAREN expr TRPAREN { $$ = $2; }
     | array_element { $$ = $1; }
     | TLITERAL { $$ = new NLiteral(*$1); delete $1; }
     ;

assignment : identifier TEQUAL expr { $$ = new NAssignment(shared_ptr<NIdentifier>($1), shared_ptr<NExpression>($3)); }
           | array_element TEQUAL expr { $$ = new NArrayAssign(shared_ptr<NArrayIndex>($1), shared_ptr<NExpression>($3)); }
           | STRUCT_TAG TDOT identifier TEQUAL expr {
                                                        auto member = new NStructMember(shared_ptr<NIdentifier>($1), shared_ptr<NIdentifier>($3));
                                                        $$ = new NStructAssign(shared_ptr<NStructMember>(member), shared_ptr<NExpression>($5));
                                                    }
           ;

comparison : TCEQ | TCNE | TCLT | TCLE | TCGE | TCGT
           | TAND | TOR | TXOR
           ;

arithmetic : TPLUS | TMINUS | TMUL | TDIV | TMOD
            | TLSHIFT | TRSHIFT
            ;

array_element : identifier TLBRACKET expr TRBRACKET { $$ = new NArrayIndex(shared_ptr<NIdentifier>($1), shared_ptr<NExpression>($3)); }
              | array_element TLBRACKET expr TRBRACKET { $1->expressions->push_back(shared_ptr<NExpression>($3)); $$ = $1; }
              ;

Args : Args COMMA expr { $1->push_back(shared_ptr<NExpression>($3)); $$ = $1; }
     | expr { $$ = new ExpressionList(); $$->push_back(shared_ptr<NExpression>($1)); }
     | { $$ = new ExpressionList(); }
     ;

 /* Function Declaration Ends */

 /* Structure Declaration Starts */
struct_declar : TSTRUCT STRUCT_TAG TLBRACE DefList TRBRACE SEMI {
                                                                        auto var_list = new VariableList();
                                                                        for(auto it=($4)->statements->begin(); it!=($4)->statements->end(); it++)
                                                                                var_list->push_back(shared_ptr<NVariableDeclaration>(dynamic_pointer_cast<NVariableDeclaration>(*it)));
                                                                        $$ = new NStructDeclaration(shared_ptr<NIdentifier>($2), shared_ptr<VariableList>(var_list));
                                                                }
              ;

 /* Structure Declaration Ends */

 %%

void yyerror(char* msg)
{
    if(errorLexFlag==0)
        fprintf(stderr, "Error at line %d: %s\n", yylineno, msg);
}
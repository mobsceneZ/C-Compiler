%{
    #include <cstdio>
    #include <cstdlib>

    extern int errorLexFlag;
    extern int errorSyntaxFlag;
    extern int yylex();

%}

%union
{
    int token;
    shared_ptr<string> string;
}

%token <token> TPLUS TMINUS TMUL TDIV TMOD TLSHIFT TRSHIFT 
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE TAND TOR TXOR
%token <token> SEMI COMMA TLBRACKET TRBRACKET TLPAREN TRPAREN TEQUAL
%token <token> TLBRACE TRBRACE TRETURN TIF TWHILE TELSE TSTRUCT TDOT
%token <string> TIDENTIFIER TLITERAL TVDOUBLE TVINTEGER TEXTERN
%token <string> TYDOUBLE TYCHAR TYSTRING TYVOID TYFLOAT TYBOOL TYINT

%nonassoc TCEQ TCNE TCLT TCLE TCGT TCGE

%start Program

%%

Program : ExtDefList
        ;

ExtDefList : ExtDefList ExtDef
             
           |
           ;

ExtDef : var_declar
       | struct_declar
       | func_declar
       | error SEMI
       ;

 /* Variable Declaration Starts */
 /* Supports ARRAY, STRUCT two complex type. */
 /* Supports int, double, string, float, char and etc. */
var_declar : Specifier ExtDecList SEMI
           | error SEMI
           ;

Specifier : BASE_TYPE
          | ARRAY_TYPE
          | STRUCT_TYPE
          ;

BASE_TYPE : TYINT
          | TYDOUBLE
          | TYBOOL
          | TYCHAR
          | TYSTRING
          | TYFLOAT
          | TYVOID
          ;

ARRAY_TYPE : ARRAY_TYPE TLBRACKET TVINTEGER TRBRACKET
           | BASE_TYPE TLBRACKET TVINTEGER TRBRACKET
           ;

STRUCT_TYPE : TSTRUCT STRUCT_TAG
            ;

STRUCT_TAG : TIDENTIFIER
           ;

identifier : TIDENTIFIER
           ;

ExtDecList : identifier 
           | identifier TEQUAL expr
           | identifier TEQUAL TLBRACKET expr TRBRACKET
           ;

 /* Variable Declaration Ends */

 /* Function Declaration Starts */
func_declar : Specifier FunDec CompSt
            | TEXTERN Specifier FunDec SEMI
            ;

FunDec : identifier TLPAREN TRPAREN
       | identifier TLPAREN VarList TRPAREN
       | error TRPAREN
       ;

VarList : ParamDec COMMA VarList
        | ParamDec
        ;

ParamDec : Specifier identifier
         ;

 /* which means the definition should be before statements. */
CompSt : TLBRACE DefList StmtList TRBRACE
       | TLBRACE error TRBRACE
       ;

DefList : DefList var_declar
        |
        ;

StmtList : StmtList Stmt
         |
         ;

Stmt : expr SEMI
     | TRETURN expr SEMI
     | if_stmt
     | while_stmt
     | error SEMI
     ;

if_stmt : TIF TLPAREN expr TRPAREN CompSt
        | TIF TLPAREN expr TRPAREN CompSt TELSE CompSt
        | TIF TLPAREN expr TRPAREN CompSt TELSE if_stmt
        ;

while_stmt : TWHILE TLPAREN expr TRPAREN CompSt

 /* Expression Part. */
expr : assignment 
     | identifier TLPAREN Args TRPAREN
     | identifier
     | STRUCT_TAG TDOT identifier
     | TVINTEGER
     | TVDOUBLE
     | expr comparison expr
     | expr arithemetic expr
     | TLPAREN expr TRPAREN
     | TMINUS expr
     | array_element
     | TLITERAL
     ;

assignment : identifier TEQUAL expr
           | array_element TEQUAL expr
           | STRUCT_TAG TDOT identifier TEQUAL expr
           ;

comparison : TCEQ | TCNE | TCLT | TCLE | TCGE | TCGT
           | TAND | TOR | TXOR
           ;

arithemetic : TPLUS | TMINUS | TMUL | TDIV | TMOD
            | TLSHIFT | TRSHIFT
            ;

array_element : identifier TLBRACKET expr TRBRACKET
              | array_element TLBRACKET expr TRBRACKET
              ;

Args : expr COMMA Args
     | expr
     |
     ;

 /* Function Declaration Ends */

 /* Structure Declaration Starts */
struct_declar : TSTRUCT STRUCT_TAG TLBRACE DefList TRBRACE COMMA
              ;

 /* Structure Declaration Ends */

 %%

void yyerror(char* msg)
{
    if(errorLexFlag==0)
        fprintf(stderr, "Error at line %d: %s\n", yylineno, msg);
}
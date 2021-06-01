%{
    #include <cstdio>
    #include <string>
    #include <cstdlib>
    #include "ASTNode.h"
    #include "grammar.tab.h"

    extern int errorLexFlag;

    int yycolumn = 1;
    #define YY_USER_ACTION \
		  yylloc.first_line = yylloc.last_line = yylineno; \
		  yylloc.first_column = yycolumn; \
		  yylloc.last_column = yycolumn + yyleng - 1; \
		  yycolumn += yyleng;
    
    #define STRING_TOKEN ( yylval.string = make_shared<string>(yytext) )
    #define INT_TOKEN(t) ( yylval.token = t )
%}

%option yylineno
%option noyywrap

%%

[ \t]+                                                { }
"//"                                                  { while(input() != '\n'); }
"/*"([^\*]|(\*)*[^\*/])*(\*)*"*/"                     { }
"if"                                                  { return INT_TOKEN(TIF); }
"else"                                                { return INT_TOKEN(TELSE); }
"while"                                               { return INT_TOKEN(TWHILE); }
"return"                                              { return INT_TOKEN(TRETURN); }
"struct"                                              { return INT_TOKEN(TSTRUCT); }
"int"                                                 { STRING_TOKEN; return TYINT; }
"double"                                              { STRING_TOKEN; return TYDOUBLE; }
"char"                                                { STRING_TOKEN; return TYCHAR; }
"float"                                               { STRING_TOKEN; return TYFLOAT; }
"bool"                                                { STRING_TOKEN; return TYBOOL; }
"string"                                              { STRING_TOKEN; return TYSTRING; }
"void"                                                { STRING_TOKEN; return TYVOID; }
"extern"                                              { STRING_TOKEN; return TEXTERN; }
[A-Za-z_][A-Za-z0-9_]*                                { STRING_TOKEN; return TIDENTIFIER; }
-?(0|[1-9][0-9]*)                                     { STRING_TOKEN; return TVINTEGER; }
[0-9]+\.[0-9]+                                        { STRING_TOKEN; return TVDOUBLE; }
\"(\\.|[^\"])*\"                                      { STRING_TOKEN; return TLITERAL; }
"+"                                                   { return INT_TOKEN(TPLUS); }
"-"                                                   { return INT_TOKEN(TMINUS); }
"*"                                                   { return INT_TOKEN(TMUL); }
"/"                                                   { return INT_TOKEN(TDIV); }
"%"                                                   { return INT_TOKEN(TMOD); }
"<<"                                                  { return INT_TOKEN(TLSHIFT); }
">>"                                                  { return INT_TOKEN(TRSHIFT); }
"=="                                                  { return INT_TOKEN(TCEQ); }
"!="                                                  { return INT_TOKEN(TCNE); }
"<"                                                   { return INT_TOKEN(TCLT); }
"<="                                                  { return INT_TOKEN(TCLE); }
">"                                                   { return INT_TOKEN(TCGT); }
">="                                                  { return INT_TOKEN(TCGE); }
"&"                                                   { return INT_TOKEN(TAND); }
"|"                                                   { return INT_TOKEN(TOR); }
"^"                                                   { return INT_TOKEN(TXOR); }
";"                                                   { return INT_TOKEN(SEMI); }
","                                                   { return INT_TOKEN(COMMA); }
"["                                                   { return INT_TOKEN(TLBRACKET); }
"]"                                                   { return INT_TOKEN(TRBRACKET); }
"("                                                   { return INT_TOKEN(TLPAREN); }
")"                                                   { return INT_TOKEN(TRPAREN); }
"="                                                   { return INT_TOKEN(TEQUAL); }
"{"                                                   { return INT_TOKEN(TLBRACE); }
"}"                                                   { return INT_TOKEN(TRBRACE); }
"."                                                   { return INT_TOKEN(TDOT); }
"\n"                                                  { yycolumn = 1; }
.                                                     { errorLexFlag = 1; printf("Mysterious character at line %d: \'%s\'\n", yylineno, yytext); }
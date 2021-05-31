%{

    #include <cstdio>
    #include <cstdlib>

    extern int errorLexFlag;

    int yycolumn = 1;
    #define YY_USER_ACTION \
		  yylloc.first_line = yylloc.last_line = yylineno; \
		  yylloc.first_column = yycolumn; \
		  yylloc.last_column = yycolumn + yyleng - 1; \
		  yycolumn += yyleng;
    
%}

%option yylineno
%option noyywrap


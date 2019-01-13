%{
	#include "y.tab.h"

	int lineNo = 1;
	int colNo = 1;
%}


%%
"FOR"				     { colNo+=3; return TOK_FOR; }
"DO"                                 { colNo+=2; return TOK_DO; }
"TO"                                 { colNo+=2; return TOK_TO; }
"READ"				     { colNo+=strlen(yytext); return TOK_READ; }
"WRITE"			             { colNo+=5; return TOK_WRITE; }
[ ]				     { colNo++; }
"DIV"                                { colNo+=3; return TOK_DIV; }
"INTEGER"			     { colNo+=strlen(yytext); return TOK_INTEGER; }
"PROGRAM" { colNo+=strlen(yytext); return TOK_PROGRAM; }
"VAR"                                { colNo+=strlen(yytext); return TOK_DECLARE; }
"BEGIN"				     { colNo+=strlen(yytext); return TOK_BEGIN; }
"END"                                { colNo+=strlen(yytext); return TOK_END; }
[a-zA-Z_][a-zA-Z0-9_]*	             { yylloc.first_line=lineNo; yylloc.first_column= colNo; colNo+=strlen(yytext); yylval.sir=new char[strlen(yytext)+1]; strcpy(yylval.sir, yytext) ; return TOK_ID; }
":"				     { colNo++; return TOK_ATTRIB; }
";"				     { colNo++; return ';'; }
","                                  { colNo++; return ','; }
":="                                 { colNo++; return TOK_EQU; }
"+"				     { colNo++; return TOK_PLUS; }
"-"				     { colNo++; return TOK_MINUS; }
"*"				     { colNo++; return TOK_MULTIPLY; }
"("				     { colNo++; return TOK_LEFT; }
")"				     { colNo++; return TOK_RIGHT; }
0|[1-9][0-9]*                   { yylloc.first_line= lineNo; yylloc.first_column= colNo; colNo+=strlen(yytext); yylval.val=atoi(yytext); return TOK_INT; }

\r\n			             { lineNo++; colNo=1; }
\n				     { lineNo++; colNo=1; }
.				     { colNo++; return TOK_ERROR; }

%%

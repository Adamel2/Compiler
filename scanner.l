%{
#include <stdio.h>
#include <stdlib.h>
#include "y.tab.h"

extern int yylineno;
int yyerror(const char *str);

%}

%x CC
Num [0-9]
Id [a-zA-Z][0-9_]*
blank [ \t\n]+
%%

[\t\f\v]  {/*spaces*/}
[\n]  {yylineno++;}    

"//".*[\n] {/*comment*/}

"/%" {BEGIN(CC);}
<CC>"%/" { BEGIN(INITIAL); }
<CC>. { }
<CC>\n   {yylineno++;} 

"bool"	    {return BOOL;}
"char" 	 	{return CHAR;}
"int" 	  	{return INT;}
"real"      {return REAL;}
"string" 	{return STRING;}
"int*" 		{return INTP;}
"char*" 	{return CHARP;}
"real*"     {return REALP;}
"if" 		{return IF;}
"else" 		{return ELSE;}
"while" 	{return WHILE;}
"for" 		{return FOR;}
"var"       {return VAR;}
"func"      {return FUNC;}
"proc"      {return PROC;}
"return"    {return RETURN;}
"null"      {return NIL;}

"&&" 		{return AND;}
"/" 		{return DIV;}
"=" 		{return ASSIGN;}
"==" 		{return EQ;}
">" 		{return GREATER;}
">=" 		{return GREATER_EQ;}
"<" 		{return LOWER;}
"<=" 		{return LOWER_EQ;}
"-" 		{return MINUS;}
"!" 		{return NOT;}
"!=" 		{return NOT_EQ;}
"||" 		{return OR;}
"+" 		{return PLUS;} 
"*"		    {return MULT;}
"&" 		{return REFERENCE;}
"^" 		{return DEREFERENCE;}

";" 		{return SEMICOLON;}
":"         {return COLON;}
"," 		{return COMMA;}
"|" 		{return PIPE;}
"{" 		{return BRACE_OPEN;}
"}" 		{return BRACE_CLOSE;}
"(" 		{return BRACKET_OPEN;}
")" 		{return BRACKET_CLOSE;}
"[" 		{return SQUARE_OPEN;}
"]" 		{return SQUARE_CLOSE;}

"true" 		{return TRUE;}
"false"		{return FALSE;}



{Num}+    {return CONST_INT;}


{Id}+ {yylval.string=strdup(yytext); return ID;}

"'"."'" { yylval.string=strdup(yytext) ;return CONST_CHAR;}

\"([^\\\"]|\\.)*\" {yylval.string=strdup(yytext); return CONST_STRING;}

{blank} {;}

.  {yyerror("Error while scanning!"); exit(1);}


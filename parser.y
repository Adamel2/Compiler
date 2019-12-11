%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lex.yy.c"

typedef struct Tree 
 {
   char* string;
   struct Tree *n1, *n2, *n3, *n4;
 } Tree;

Tree* make4Tree(char*, Tree*, Tree*, Tree*, Tree*);
Tree* make3Tree(char*, Tree*, Tree*, Tree*);
Tree* make2Tree(char*, Tree*, Tree*);
Tree* make1Tree(char*, Tree*);
Tree* makeLeaf(char*);
void printTree(Tree*,int);
void deleteTree(Tree*);

extern int yylex();
extern int yylineno;
extern char *yytext;

int yyerror(const char *str);
%}

%union {
  char* string;
  struct Tree* tree;
}

%start S
%token  BOOL INT INTP CHAR CHARP STRING REAL REALP VAR 
%token  IF ELSE WHILE FOR 
%token  FUNC RETURN PROC NIL
%token  AND ASSIGN EQ GREATER GREATER_EQ LOWER LOWER_EQ NOT NOT_EQ OR REFERENCE DEREFERENCE
%token  MINUS PLUS MULT DIV 
%token  SEMICOLON COLON COMMA PIPE BRACE_OPEN BRACE_CLOSE BRACKET_OPEN BRACKET_CLOSE SQUARE_OPEN SQUARE_CLOSE S_COMMENT E_COMMENT
%token  TRUE FALSE
%token <string> CONST_CHAR  CONST_STRING
%token <string> ID
%token <string> CONST_INT

%type <tree> S Program Input ParameterType ArraySize Operator FuncBody ProcBody Variable Assign CallFunc
%type <tree> OneOP MulOPs For UpdateFor HelpFor Stmt StmtProc Else_Statement ForProc Else_StatementProc Proc Func
%type <tree> HelpCondition ConditionOper Brace StatementProc Condition HelpOperator Expression Operation Return Id
%type <tree> FuncParams ProcBlock FuncBlock FuncReturn FuncType MainProc  Statement Const Nul

%left AND EQ GREATER GREATER_EQ LOWER LOWER_EQ NOT_EQ OR MINUS PLUS MULT DIV
%right ASSIGN NOT REFERENCE DEREFERENCE

%%
S : Program {printf("(CODE");  printTree($1,0); printf(")\n");}
;
Program :Program Func {$$=make2Tree("",$1,$2);}|Program Proc {$$=make2Tree("",$1,$2);}|Func {$$=$1;}|Proc {$$=$1;}
;
MainProc: PROC "Main" BRACE_OPEN BRACE_CLOSE ProcBlock {$$=make1Tree("MAIN",$5);}
;
Func : FUNC Id Input FuncReturn FuncBlock {$$=make4Tree("FUNC",$2,$3,$4,$5);}
;
Proc : PROC Id Input ProcBlock {$$=make3Tree("PROC",$2,$3,$4);}
        | MainProc {$$=make1Tree("",$1);}
;
Id: ID {$$=makeLeaf($1);} 

;
Input :BRACKET_OPEN FuncParams BRACKET_CLOSE {$$=make1Tree("ARGS",$2); }
        | BRACKET_OPEN BRACKET_CLOSE {$$=makeLeaf("ARGS NONE");}
;
FuncParams:Variable COLON ParameterType {$$=make2Tree("",$1,$3);}
        |Variable COLON ParameterType SEMICOLON FuncParams {$$=make3Tree("",$1,$3,$5);}
;
ParameterType: BOOL {$$=makeLeaf("BOOL");}
        | INT {$$=makeLeaf("INT");}
        | CHAR {$$=makeLeaf("CHAR");}
        | INTP {$$=makeLeaf("INTP");}
        | CHARP {$$=makeLeaf("CHARP");}
        | REAL {$$=makeLeaf("REAL");}
        | REALP {$$=makeLeaf("REALP");}
        | STRING {$$=makeLeaf("STRING");}
        | ParameterType SQUARE_OPEN ArraySize SQUARE_CLOSE {$$=makeLeaf("STRING[]");}
;
FuncType: BOOL {$$=makeLeaf("BOOL");}
        | INT {$$=makeLeaf("INT");}
        | CHAR {$$=makeLeaf("CHAR");}
        | INTP {$$=makeLeaf("INTP");}
        | CHARP {$$=makeLeaf("CHARP");}
        | REAL {$$=makeLeaf("REAL");}
        | REALP {$$=makeLeaf("REALP");}
        ;
ArraySize : Const {$$=make1Tree("",$1);}
        | Const Operator Const {$$=make3Tree("",$1,$2,$3);}
;
Operator : MULT {$$=makeLeaf("MUL");}
        | MINUS {$$=makeLeaf("MINUS");}
        | PLUS  {$$=makeLeaf("PLUS");}
        | DIV   {$$=makeLeaf("DIV");}
;
FuncBlock: BRACE_OPEN FuncBody RETURN Return SEMICOLON BRACE_CLOSE {$$=make2Tree("BODY",$2,$4);}
;

FuncBody : Program FuncBody {$$=make2Tree("",$1,$2);}
        | VAR Variable COLON ParameterType SEMICOLON FuncBody {$$=make3Tree("BODY",$2,$4,$6);}
        | CallFunc SEMICOLON FuncBody {$$=make2Tree("",$1,$3);}
        | Expression FuncBody {$$=make2Tree("",$1,$2);}
        | Stmt FuncBody {$$=make2Tree("",$1,$2);}
        | For FuncBody {$$=make2Tree("",$1,$2);}
        | Brace FuncBody {$$=make2Tree("",$1,$2);}| {$$=makeLeaf("");}
;

ProcBlock: BRACE_OPEN ProcBody BRACE_CLOSE {$$=make1Tree("BODY",$2);};

ProcBody : Program ProcBody {$$=make2Tree("",$1,$2);}
        | VAR Variable COLON ParameterType SEMICOLON ProcBody {$$=make3Tree("BODY",$2,$4,$6);}
        | CallFunc SEMICOLON ProcBody {$$=make2Tree("",$1,$3);}
        | Expression ProcBody {$$=make2Tree("",$1,$2);}
        | StmtProc ProcBody {$$=make2Tree("",$1,$2);}
        | ForProc ProcBody {$$=make2Tree("",$1,$2);}
        | Brace ProcBody {$$=make2Tree("",$1,$2);}| {$$=makeLeaf("");}
;
Variable : Id {$$=$1;}
        |Id COMMA Variable {$$=make2Tree("",$1,$3);}
; 
Const:CONST_INT {$$=makeLeaf($1);}| CONST_STRING {$$=makeLeaf($1);}| CONST_CHAR {$$=makeLeaf($1);}
;
         
Nul: NIL {$$=makeLeaf("NULL");}
;
Assign: ASSIGN Nul {$$=make1Tree("=",$2);} 
        | ASSIGN Id{$$=make1Tree("=",$2);}
        | ASSIGN REFERENCE Id {$$=make1Tree("=",$3);}
        | ASSIGN DEREFERENCE Id {$$=make1Tree("=",$3);}
        | ASSIGN DEREFERENCE DEREFERENCE Id {$$=make1Tree("=",$4);}
        | ASSIGN Id BRACKET_OPEN BRACKET_CLOSE {$$=make1Tree("=",$2);}
        | ASSIGN Id SQUARE_OPEN Operation SQUARE_CLOSE {$$=make2Tree("=",$2,$4);}
        | ASSIGN REFERENCE Id SQUARE_OPEN Operation SQUARE_CLOSE {$$=make2Tree("=",$3,$5);}
        | ASSIGN PIPE Id PIPE {$$=make1Tree("=",$3);}
        | ASSIGN DEREFERENCE BRACKET_OPEN Operation BRACKET_CLOSE{$$=make1Tree("=",$4);}
        | ASSIGN Const {$$=make1Tree("=",$2);}
        | ASSIGN Id Operator Id {$$=make2Tree("=",$2,$4);}
        | ASSIGN CallFunc {$$=make1Tree("=",$2);}
        | ASSIGN Operation{$$=make1Tree("=",$2);}
;
Expression : Id Assign SEMICOLON {$$=make2Tree("",$1,$2);}
        |REFERENCE Assign Id SEMICOLON {$$=make2Tree("&",$2,$3);}
        |Id SQUARE_OPEN Operation SQUARE_CLOSE Assign SEMICOLON{$$=make3Tree("",$1,$3,$5);}
        |UpdateFor SEMICOLON{$$=make1Tree("",$1);}
;  
Operation : Id Operator Id {$$=make3Tree("",$1,$2,$3);}
        |Const Operator Const {$$=make3Tree("",$1,$2,$3);}
        |CONST_INT {$$=makeLeaf("");}
        |Id Operator Const {$$=make3Tree("",$1,$2,$3);}
        |Const Operator Id{$$=make3Tree("",$1,$2,$3);}
        |Const COMMA Operation {$$=make2Tree("",$1,$3);}
        |Variable COMMA Operation{$$=make2Tree("",$1,$3);}
        |Id {$$=make1Tree("",$1);}
;

FuncReturn: RETURN FuncType {$$=make1Tree("RET",$2);}
;

Return : Id {$$=make1Tree("RET",$1);}
        |Id Operator Id {$$=make3Tree("RET",$1,$2,$3);}
        |CONST_STRING {$$=makeLeaf($1);}
        |CONST_CHAR {$$=makeLeaf($1);}
        |REFERENCE Id {$$=make1Tree("RET",$2);}
        |Id DEREFERENCE {$$=make1Tree("RET",$1);}
        |CONST_INT {$$=makeLeaf($1);}
        |Id SQUARE_OPEN CONST_INT SQUARE_CLOSE {$$=make1Tree("RET",$1);}
        |Id SQUARE_OPEN Id SQUARE_CLOSE {$$=make2Tree("RET",$1,$3);}
        |CallFunc {$$=make1Tree("RET",$1);}
;
CallFunc : Id BRACKET_OPEN Operation BRACKET_CLOSE {$$ = make2Tree("FUNC CALL",$1,$3);}
;
////////////////
OneOP : RETURN Return SEMICOLON {$$=make1Tree("RET",$2);}
        | Expression {$$=make1Tree("",$1);}
;

MulOPs : FuncBody RETURN Return SEMICOLON  {$$=make2Tree("",$1,$3);}
        | FuncBody {$$=make1Tree("",$1);}
;
For : FOR BRACKET_OPEN HelpFor SEMICOLON Condition SEMICOLON UpdateFor BRACKET_CLOSE OneOP{$$=make4Tree("FOR",$3,$5,$7,$9);}
        | FOR BRACKET_OPEN HelpFor SEMICOLON Condition SEMICOLON UpdateFor BRACKET_CLOSE BRACE_OPEN MulOPs BRACE_CLOSE{$$=make4Tree("FOR",$3,$5,$7,$10);}
;

UpdateFor: Id Operator Id {$$=make3Tree("",$1,$2,$3);}
        | Id Operator CONST_INT {$$=make2Tree("",$1,$2);}
        | Id PLUS PLUS {$$=make1Tree("",$1);}
        | Id MINUS MINUS {$$=make1Tree("",$1);}
        | Id ASSIGN UpdateFor {$$=make2Tree("",$1,$3);}
        | Id ASSIGN UpdateFor COMMA UpdateFor {$$=make3Tree("",$1,$3,$5);}
;

HelpFor : Id ASSIGN CONST_INT  {$$=make1Tree("=",$1);}
        | Id ASSIGN Id {$$=make2Tree("=",$1,$3);}
        | Id ASSIGN CONST_INT COMMA HelpFor {$$=make2Tree("=",$1,$5);}
        | Id ASSIGN Id COMMA HelpFor{$$=make3Tree("=",$1,$3,$5);}
;
Stmt :  IF Statement {$$=make1Tree("IF",$2);}
        |IF Statement ELSE Else_Statement {$$=make2Tree("IF-ELSE",$2,$4);}
        |WHILE Statement {$$=make1Tree("WHILE",$2);}
;
StmtProc : IF StatementProc {$$=make1Tree("IF",$2);}
        |IF StatementProc ELSE Else_StatementProc {$$=make2Tree("IF-ELSE",$2,$4);}
        |WHILE StatementProc {$$=make1Tree("WHILE",$2);}
;
Else_Statement : BRACE_OPEN MulOPs BRACE_CLOSE{$$=make1Tree("ELSE",$2);}
                | OneOP {$$=make1Tree("ELSE",$1);}
;

Statement : BRACKET_OPEN Condition BRACKET_CLOSE BRACE_OPEN MulOPs BRACE_CLOSE{$$=make2Tree("",$2,$5);}
              | BRACKET_OPEN Condition BRACKET_CLOSE OneOP{$$=make2Tree("",$2,$4);}
;
ForProc : FOR BRACKET_OPEN HelpFor SEMICOLON Condition SEMICOLON UpdateFor BRACKET_CLOSE Expression{$$=make4Tree("FOR",$3,$5,$7,$9);}
        | FOR BRACKET_OPEN HelpFor SEMICOLON Condition SEMICOLON UpdateFor BRACKET_CLOSE BRACE_OPEN ProcBody BRACE_CLOSE{$$=make4Tree("FOR",$3,$5,$7,$10);}
;
Else_StatementProc : BRACE_OPEN ProcBody BRACE_CLOSE{$$=make1Tree("ELSE",$2);}
                | Expression{$$=make1Tree("ELSE",$1);}
;

StatementProc : BRACKET_OPEN Condition  BRACKET_CLOSE BRACE_OPEN ProcBody BRACE_CLOSE{$$=make2Tree("",$2,$5);}
              | BRACKET_OPEN Condition BRACKET_CLOSE Expression{$$=make2Tree("",$2,$4);}
;

Condition:HelpCondition ConditionOper HelpCondition{$$=make3Tree("",$1,$2,$3);}
        | NOT HelpCondition ConditionOper HelpCondition   {$$=make3Tree("!",$2,$3,$4);}
        | FALSE {$$=makeLeaf("FALSE");}
        | TRUE {$$=makeLeaf("TRUE");}
        | NOT Id {$$=make1Tree("!",$2);}
        | Id {$$=make1Tree("",$1);}
        |Condition  HelpOperator Condition {$$=make3Tree("",$1,$2,$3);}


;
HelpOperator : AND {$$=makeLeaf("&&");}
              |OR {$$=makeLeaf("||");}

;

HelpCondition:  CONST_INT {$$=makeLeaf($1);}
                | Id {$$=make1Tree("",$1);}

;
ConditionOper : EQ {$$=makeLeaf("==");}
        | GREATER {$$=makeLeaf(">");}
        | GREATER_EQ {$$=makeLeaf(">=");}
        | LOWER {$$=makeLeaf("<");}
        | LOWER_EQ {$$=makeLeaf("<=");}
        | NOT_EQ {$$=makeLeaf("!=");}
;
Brace : BRACE_OPEN BRACE_CLOSE {$$=makeLeaf("[]");}
        | BRACE_OPEN Brace BRACE_CLOSE {$$=make1Tree("[]",$2);}
;

%%

int main() {
  yyparse();
  return 0;
}

int yyerror(const char *str)
{
	fprintf(stderr, "%s , line number: %d\n", str,yylineno);
}

Tree* make4Tree(char* str, Tree* n1, Tree* n2, Tree* n3, Tree* n4)
{
   Tree *result= (Tree*) malloc (sizeof(Tree));
   result->n1=n1;
    result->n2=n2;
    result->n3=n3;
     result->n4=n4;
    result->string=str;
   return result;
}

Tree* make3Tree(char* str, Tree* n1, Tree* n2, Tree* n3)
{
   return make4Tree(str,n1,n2,n3,NULL);
}
Tree* make2Tree(char* str, Tree* n1, Tree* n2)
{
    return make4Tree(str,n1,n2,NULL,NULL);
}
Tree* make1Tree(char* str, Tree* n1)
{
   return make4Tree(str,n1,NULL,NULL,NULL);
}
Tree* makeLeaf(char* str)
{
   return make4Tree(str,NULL,NULL,NULL,NULL);
}

void printTree(Tree* t,int offset)
{ 
  int i=0,j;
  if(t->n1!=NULL)i++;
  if(t->n2!=NULL)i++;

  if (i==0)
    printf("%s ",t->string);
  else if (i==1)
  {
       printf("(%s ",t->string);
       printTree(t->n1,offset);
       printf(")");        
  }
  else
  {
      printf("\n");
      for(j=0;j<offset;j++)
      printf("   ");
      printf("(%s ",t->string);
      printTree(t->n1,offset+1);
      printTree(t->n2,offset+1);
      if(t->n3!=NULL)
          printTree(t->n3,offset+1);
      if(t->n4!=NULL)
          printTree(t->n4,offset+1);
      printf(")\n");
       for(j=0;j<offset-1;j++)
      printf("   ");
    }
}


void deleteTree(Tree* t)
{
  int i;
  deleteTree(t->n1);
  deleteTree(t->n2);
  deleteTree(t->n3);
  deleteTree(t->n4);
   free(t);
}


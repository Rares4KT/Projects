%{
#include <iostream>
#include <string>
#include "lang.tab.h"
#include "structures.h"
using namespace std;
%}

%option noyywrap
%option yylineno

%%

[ \t\r\n]+        ;
"//".* ; 
"/*"([^*]|\*+[^*/])*\*+"/"   ;

"int"             { return INT; }
"float"           { return FLOAT; }
"string"          { return STRING; }
"bool"            { return BOOL; }
"void"            { return VOID; }

"clasa"           { return CLASS; }
"daca"            { return IF; }
"altfel"          { return ELSE; }
"cat_timp"        { return WHILE; }
"pentru"          { return FOR; }
"returneaza"      { return RETURN; }
"blocul_principal" { return MAIN; }
"nou"             { return NEW; }

"Print"           { return TOK_PRINT; }
"scrie"           { return TOK_SCRIE; }

"inceput"         { return LBRACE; }
"sfarsit"         { return RBRACE; }

"adevarat"        { yylval.strVal = new string("true"); return BOOLVAL; }
"fals"            { yylval.strVal = new string("false"); return BOOLVAL; }

[a-zA-Z_][a-zA-Z0-9_]* { yylval.strVal = new string(yytext); return ID; }
[0-9]+"."[0-9]+   { yylval.strVal = new string(yytext); return FLOATVAL; }
[0-9]+            { yylval.strVal = new string(yytext); return INTVAL; }
\"[^"]*\"         { 
    string s = yytext; 
    yylval.strVal = new string(s.substr(1, s.length()-2)); 
    return STRINGVAL; 
}

"=="              { return EQ; }
"!="              { return NEQ; }
"<="              { return LE; }
">="              { return GE; }
"&&"              { return AND; }
"||"              { return OR; }
"<"               { return LT; }
">"               { return GT; }
"!"               { return NOT; }

"+"               { return PLUS; }
"-"               { return MINUS; }
"*"               { return MUL; }
"/"               { return DIV; }

"<-"              { return ASSIGN; }

"~"               { return SEMICOLON; }
","               { return COMMA; }
"#"               { return COMMA; }
":"               { return COLON; }
"::"              { return DOUBLE_COLON; }
"("               { return LPAREN; }
")"               { return RPAREN; }

.                 { cout << "Caracter necunoscut: " << yytext << endl; }

%%
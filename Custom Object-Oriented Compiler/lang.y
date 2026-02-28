%{
#include <iostream>
#include <string>
#include <vector>
#include <cstdio>
#include <cstdlib> 
#include "structures.h"

using namespace std;

extern int yylex();
extern int yyparse();
extern int yylineno;
extern FILE* yyin;
void yyerror(const char* s);

map<string, SymbolTable*> SymbolTable::classScopes; 

SymbolTable* globalScope = new SymbolTable("Global");
SymbolTable* currentScope = globalScope;

struct ParamTemp { string name; ValType type; string customType; };
vector<ParamTemp> tempParams;
vector<ASTNode*> argTemp;
string currentCustomType = "";
string tempReturnCustomType = ""; 
%}

%code requires {
    #include "structures.h"
}

%union {
    std::string* strVal;
    ASTNode* node;
    ValType valType;
}

%token <strVal> ID INTVAL FLOATVAL BOOLVAL STRINGVAL
%token INT FLOAT BOOL STRING VOID
%token CLASS IF ELSE WHILE FOR RETURN MAIN NEW
%token TOK_PRINT TOK_SCRIE
%token SEMICOLON COMMA COLON DOUBLE_COLON ASSIGN
%token LPAREN RPAREN LBRACE RBRACE 
%token PLUS MINUS MUL DIV LT GT LE GE EQ NEQ AND OR NOT

%left OR
%left AND
%left EQ NEQ LT LE GT GE
%left PLUS MINUS
%left MUL DIV
%right NOT
%left COLON DOUBLE_COLON
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%type <node> program elements element
%type <node> class_def func_def main_block global_var_decl 
%type <node> func_body_list stm_list_no_decl statement_common code_block_no_decl
%type <node> assignment if_stmt while_stmt for_stmt print_stmt return_stmt
%type <node> expr arith_expr bool_expr func_call member_access instantiation
%type <node> arg_list_opt arg_list param_list_opt
%type <valType> type

%start program

%%

program : elements ;
elements : element | elements element ;
element : class_def | func_def | global_var_decl | main_block ;

type : INT { $$ = TYPE_INT; }
     | FLOAT { $$ = TYPE_FLOAT; }
     | BOOL { $$ = TYPE_BOOL; }
     | STRING { $$ = TYPE_STRING; }
     | VOID { $$ = TYPE_VOID; }
     | ID { 
         if (SymbolTable::classScopes.find(*$1) == SymbolTable::classScopes.end()) {
             cout << "Eroare Semantica (linia " << yylineno << "): Tip necunoscut '" << *$1 << "'." << endl; exit(1);
         }
         $$ = TYPE_OBJ; currentCustomType = *$1;
     }
     ;

global_var_decl : type ID SEMICOLON {
    string typeName = ($1 == TYPE_OBJ) ? currentCustomType : "";
    if (!currentScope->addSymbol(*$2, $1, typeName)) {
        cout << "Eroare Semantica (linia " << yylineno << "): Variabila globala '" << *$2 << "' redeclarata." << endl; exit(1);
    }
    $$ = nullptr;
};

var_decl : type ID SEMICOLON { 
    string typeName = ($1 == TYPE_OBJ) ? currentCustomType : "";
    if (!currentScope->addSymbol(*$2, $1, typeName)) {
        cout << "Eroare Semantica (linia " << yylineno << "): Variabila locala '" << *$2 << "' redeclarata." << endl; exit(1);
    }
};

class_def : CLASS ID LBRACE {
        if (SymbolTable::classScopes.find(*$2) != SymbolTable::classScopes.end()) {
             cout << "Eroare Semantica (linia " << yylineno << "): Clasa '" << *$2 << "' deja definita." << endl; exit(1);
        }
        currentScope = new SymbolTable("Class_" + *$2, currentScope);
        SymbolTable::classScopes[*$2] = currentScope; 
    } class_members RBRACE {
        currentScope = currentScope->getParent();
    };

class_members : | class_members class_member ;
class_member : global_var_decl | func_def ;

func_def : type ID { 
        if ($1 == TYPE_OBJ) tempReturnCustomType = currentCustomType;
        else tempReturnCustomType = ""; 
    } LPAREN param_list_opt RPAREN {
        vector<ValType> pTypes; vector<string> pNames; vector<string> pCustomTypes;
        for(auto& p : tempParams) { 
            pTypes.push_back(p.type);
            pNames.push_back(p.name); 
            pCustomTypes.push_back(p.customType);
        }
        if (!currentScope->addFunction(*$2, $1, tempReturnCustomType, pTypes, pNames, pCustomTypes)) {
            cout << "Eroare Semantica (linia " << yylineno << "): Functia '" << *$2 << "' redeclarata." << endl; exit(1);
        }
        currentScope = new SymbolTable("Func_" + *$2, currentScope);
        for(auto& p : tempParams) currentScope->addSymbol(p.name, p.type, p.customType);
        tempParams.clear();
    } LBRACE func_body_list RBRACE {
        SymbolTable* parent = currentScope->getParent();
        parent->setFuncBody(*$2, $9);
        currentScope = currentScope->getParent();
    };

func_body_list : { $$ = nullptr; }
               | func_body_list statement_common { 
                   if ($1 == nullptr) $$ = $2;
                   else if ($2 == nullptr) $$ = $1; 
                   else $$ = new ASTNode(NODE_SEQ, $1, $2, TYPE_NONE);
               }
               | func_body_list var_decl { $$ = $1; } 
               ;

param_list_opt : { $$ = nullptr; } | param_list { $$ = nullptr; };
param_list : param | param_list COMMA param ;
param : type ID { 
    string cType = ($1 == TYPE_OBJ) ? currentCustomType : "";
    tempParams.push_back({*$2, $1, cType}); 
};

main_block : MAIN LBRACE { 
                 currentScope = new SymbolTable("Main", globalScope);
             } 
             stm_list_no_decl RBRACE {
                 cout << "--- START EXECUTION (Main Block) ---" << endl;
                 if ($4) $4->execute(currentScope); 
                 cout << "--- END EXECUTION ---" << endl;
                 currentScope = currentScope->getParent();
             };

stm_list_no_decl : { $$ = nullptr; }
                 | stm_list_no_decl statement_common {
                     if ($1 == nullptr) $$ = $2;
                     else if ($2 == nullptr) $$ = $1;
                     else $$ = new ASTNode(NODE_SEQ, $1, $2, TYPE_NONE);
                 }
                 ;

statement_common : assignment SEMICOLON { $$ = $1; }
                 | print_stmt SEMICOLON { $$ = $1; }
                 | if_stmt { $$ = $1; }     
                 | while_stmt { $$ = $1; }
                 | for_stmt { $$ = $1; }
                 | func_call SEMICOLON { $$ = $1; } 
                 | member_access SEMICOLON { $$ = $1; }
                 | return_stmt SEMICOLON { $$ = nullptr; } 
                 | code_block_no_decl { $$ = $1; } 
                 ;

code_block_no_decl : LBRACE stm_list_no_decl RBRACE { $$ = $2; } ;

assignment : ID ASSIGN expr {
    SymbolInfo* s = currentScope->lookup(*$1);
    if (!s) { cout << "Eroare Semantica (linia " << yylineno << "): Var '" << *$1 << "' nedeclarata." << endl; exit(1); }
    if (s->type != $3->dataType) { 
        cout << "Eroare Semantica (linia " << yylineno << "): Asignare invalida (" << typeToString(s->type) << " vs " << typeToString($3->dataType) << ")." << endl; exit(1); 
    }
    ASTNode* node = new ASTNode(NODE_ID, s->type); node->idName = *$1;
    $$ = new ASTNode(NODE_ASSIGN, node, $3, s->type);
}
| member_access ASSIGN expr {
    $$ = new ASTNode(NODE_ASSIGN, $1, $3, $1->dataType);
};

if_stmt : IF LPAREN bool_expr RPAREN statement_common %prec LOWER_THAN_ELSE {
             $$ = new ASTNode(NODE_IF, $3, $5, nullptr);
          }
        | IF LPAREN bool_expr RPAREN statement_common ELSE statement_common {
             $$ = new ASTNode(NODE_IF, $3, $5, $7);
          };

while_stmt : WHILE LPAREN bool_expr RPAREN statement_common {
                 $$ = new ASTNode(NODE_WHILE, $3, $5, TYPE_NONE);
             };

for_stmt : FOR LPAREN assignment SEMICOLON bool_expr SEMICOLON assignment RPAREN statement_common {
               ASTNode* bodyWithStep = new ASTNode(NODE_SEQ, $9, $7, TYPE_NONE);
               ASTNode* whileNode = new ASTNode(NODE_WHILE, $5, bodyWithStep, TYPE_NONE);
               ASTNode* forNode = new ASTNode(NODE_SEQ, $3, whileNode, TYPE_NONE);
               $$ = forNode;
           };

print_stmt : TOK_PRINT LPAREN expr RPAREN { $$ = new ASTNode(NODE_PRINT, $3, nullptr, TYPE_NONE); }
           | TOK_SCRIE LPAREN expr RPAREN { $$ = new ASTNode(NODE_PRINT, $3, nullptr, TYPE_NONE); };

return_stmt : RETURN expr { $$ = nullptr; };

func_call : ID LPAREN { argTemp.clear(); } arg_list_opt RPAREN {
     SymbolInfo* s = currentScope->lookup(*$1);
     if (!s || !s->isFunction) { cout << "Eroare Semantica: Functie '" << *$1 << "' nedefinita." << endl; exit(1); }
     ASTNode* node = new ASTNode(NODE_OTHER, s->type);
     if (s->type == TYPE_OBJ) node->customTypeName = s->customTypeName;
     $$ = node;
};

instantiation : NEW ID LPAREN RPAREN {
     if (SymbolTable::classScopes.find(*$2) == SymbolTable::classScopes.end()) { cout << "Eroare: Clasa inexistenta." << endl; exit(1); }
     $$ = new ASTNode(NODE_OTHER, TYPE_OBJ); $$->customTypeName = *$2;
};

member_access : ID DOUBLE_COLON ID {
    SymbolInfo* s = currentScope->lookup(*$1);
    if (!s || s->type != TYPE_OBJ) { cout << "Eroare: Nu e obiect." << endl; exit(1); }
    SymbolInfo* mem = SymbolTable::classScopes[s->customTypeName]->lookupMember(*$3);
    if (!mem) { cout << "Eroare: Membru inexistent." << endl; exit(1); }
    if (mem->isFunction) { cout << "Eroare: Metoda apelata gresit." << endl; exit(1); }

    ASTNode* node = new ASTNode(NODE_OTHER, mem->type); node->idName = *$3;
    ASTNode* obj = new ASTNode(NODE_ID, TYPE_OBJ); obj->idName = *$1; obj->customTypeName = s->customTypeName;
    node->left = obj; $$ = node;
}
| ID COLON ID LPAREN { argTemp.clear(); } arg_list_opt RPAREN {
    SymbolInfo* s = currentScope->lookup(*$1);
    if (!s || s->type != TYPE_OBJ) { cout << "Eroare: Nu e obiect." << endl; exit(1); }
    SymbolInfo* mem = SymbolTable::classScopes[s->customTypeName]->lookupMember(*$3);
    if (!mem || !mem->isFunction) { cout << "Eroare: Metoda inexistenta." << endl; exit(1); }
    
    ASTNode* node = new ASTNode(NODE_OTHER, mem->type);
    if (mem->type == TYPE_OBJ) node->customTypeName = mem->customTypeName;
    $$ = node;
};

arg_list_opt : { $$ = nullptr; } | arg_list { $$ = $1; } ;
arg_list : expr { argTemp.push_back($1); } | arg_list COMMA expr { argTemp.push_back($3); } ;

expr : arith_expr { $$ = $1; } | bool_expr { $$ = $1; } | instantiation { $$ = $1; } ;

arith_expr : INTVAL { $$ = new ASTNode(NODE_CONST, TYPE_INT); $$->constVal = EvalResult(stod(*$1)); }
           | FLOATVAL { $$ = new ASTNode(NODE_CONST, TYPE_FLOAT); $$->constVal = EvalResult(stod(*$1), true); }
           | STRINGVAL { $$ = new ASTNode(NODE_CONST, TYPE_STRING); $$->constVal = EvalResult(*$1); }
           | ID {
               SymbolInfo* s = currentScope->lookup(*$1);
               if (!s) { cout << "Eroare: '" << *$1 << "' nedefinit." << endl; exit(1); }
               $$ = new ASTNode(NODE_ID, s->type); $$->idName = *$1;
               if(s->type == TYPE_OBJ) $$->customTypeName = s->customTypeName;
           }
           | member_access { $$ = $1; }
           | arith_expr PLUS arith_expr { 
               if ($1->dataType != $3->dataType) { cout << "Eroare tip +." << endl; exit(1); }
               $$ = new ASTNode(NODE_OP_ARITH, $1, $3, $1->dataType); $$->op = "+"; 
           }
           | arith_expr MINUS arith_expr { 
               if ($1->dataType != $3->dataType) { cout << "Eroare tip -." << endl; exit(1); }
               $$ = new ASTNode(NODE_OP_ARITH, $1, $3, $1->dataType); $$->op = "-"; 
           }
           | arith_expr MUL arith_expr { 
               if ($1->dataType != $3->dataType) { cout << "Eroare tip *." << endl; exit(1); }
               $$ = new ASTNode(NODE_OP_ARITH, $1, $3, $1->dataType); $$->op = "*"; 
           }
           | arith_expr DIV arith_expr { 
               if ($1->dataType != $3->dataType) { cout << "Eroare tip /." << endl; exit(1); }
               $$ = new ASTNode(NODE_OP_ARITH, $1, $3, $1->dataType); $$->op = "/"; 
           }
           | LPAREN arith_expr RPAREN { $$ = $2; }
           | func_call { $$ = $1; } 
           ;

bool_expr : BOOLVAL { $$ = new ASTNode(NODE_CONST, TYPE_BOOL); $$->constVal = EvalResult(*$1 == "true"); }
          | bool_expr AND bool_expr { $$ = new ASTNode(NODE_OP_LOGIC, $1, $3, TYPE_BOOL); $$->op = "&&"; }
          | bool_expr OR bool_expr { $$ = new ASTNode(NODE_OP_LOGIC, $1, $3, TYPE_BOOL); $$->op = "||"; }
          | NOT bool_expr { $$ = new ASTNode(NODE_NOT, $2, nullptr, TYPE_BOOL); }
          | arith_expr LT arith_expr { $$ = new ASTNode(NODE_OP_REL, $1, $3, TYPE_BOOL); $$->op = "<"; }
          | arith_expr GT arith_expr { $$ = new ASTNode(NODE_OP_REL, $1, $3, TYPE_BOOL); $$->op = ">"; }
          | arith_expr LE arith_expr { $$ = new ASTNode(NODE_OP_REL, $1, $3, TYPE_BOOL); $$->op = "<="; }
          | arith_expr GE arith_expr { $$ = new ASTNode(NODE_OP_REL, $1, $3, TYPE_BOOL); $$->op = ">="; }
          | arith_expr EQ arith_expr { $$ = new ASTNode(NODE_OP_REL, $1, $3, TYPE_BOOL); $$->op = "=="; }
          | arith_expr NEQ arith_expr { $$ = new ASTNode(NODE_OP_REL, $1, $3, TYPE_BOOL); $$->op = "!="; }
          | LPAREN bool_expr RPAREN { $$ = $2; }
          ;

%%
void yyerror(const char* s) { cout << "Eroare sintaxa (linia " << yylineno << "): " << s << endl; }
int main(int argc, char** argv) {
    if (argc > 1) yyin = fopen(argv[1], "r");
    if (yyparse() == 0) {
        ofstream outFile("tables.txt");
        globalScope->printTable(outFile);
        outFile.close();
        cout << "Compilare reusita. tables.txt generat." << endl;
    }
    return 0;
}
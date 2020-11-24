%{
#include<stdlib.h>
#include<stdio.h>
#define YYDEBUG 1
#include"syntabs.h" // pour syntaxe abstraite
extern n_prog *n;   // pour syntaxe abstraite
extern FILE *yyin;    // declare dans compilo
extern int yylineno;  // declare dans analyseur lexical
int yylex();          // declare dans analyseur lexical
int yyerror(char *s); // declare ci-dessous
%}


//...
//TODO: compléter avec la liste des terminaux

%token POINT_VIRGULE 
%token PLUS 
%token MOINS 
%token FOIS 
%token DIVISE 
%token PARENTHESE_OUVRANTE 
%token PARENTHESE_FERMANTE 
%token CROCHET_OUVRANT 
%token CROCHET_FERMANT 
%token ACCOLADE_OUVRANTE 
%token ACCOLADE_FERMANTE 
%token EGAL 
%token INFERIEUR 
%token ET 
%token OU 
%token NON 
%token SI 
%token ALORS 
%token SINON 
%token TANTQUE 
%token FAIRE 
%token ENTIER 
%token RETOUR 
%token VIRGULE 
%token IDENTIF
%token NOMBRE

%union{ n_prog *n_prog; n_instr *n_instr; n_l_instr *n_l_instr; n_exp *n_exp; n_l_exp *n_l_exp; n_var *n_var; n_l_dec *n_l_dec; n_dec *n_dec; n_appel *n_appel; char *sval; double dval}

%type <char> IDENTIF;
%type<double> NOMBRE;

%type <n_l_instr> linstr;

%type <n_instr> instr iaffect isi sinonopt itantque iappel iretour ibloc;

%type <n_exp> expression e1 e2 e3 e4 e5 e6;

%type <n_l_exp> lexp lexpbis

%type <n_var> var;

%type <n_l_dec> ldecvar ldecfct ldecvarbis ldecvaropt largopt;

%type <n_dec> decvar decfct

%type <n_prog> programme;

%type <n_appel> appelfct;

%start programme
%%

programme : ldecvaropt ldecfct {$$=cree_n_prog($1, $2);};

ldecvaropt : ldecvar POINT_VIRGULE {$$ = $1;}
	| {$$ = NULL;};
	
ldecvar : decvar ldecvarbis {$$ = cree_n_l_dec($1, $2);};

ldecvarbis : VIRGULE decvar ldecvarbis {$$ = cree_n_l_dec($2, $3);}
	| {$$ = NULL;} ;
	
decvar : ENTIER IDENTIF {$$=cree_n_dec_var($2);}
	| ENTIER IDENTIF CROCHET_OUVRANT NOMBRE CROCHET_FERMANT {cree_n_dec_tab($2, $4);};


ldecfct : decfct ldecfct {$$ = cree_n_l_dec($1, $2);}
	| decfct {$$ = cree_n_l_dec($1, NULL);};
	
decfct : IDENTIF PARENTHESE_OUVRANTE largopt PARENTHESE_FERMANTE ldecvaropt ibloc {$$=cree_n_dec_fonc($1, $3, $5, $6)};

largopt : ldecvar {$$ = $1;}
	| {$$ = NULL;};


ibloc : ACCOLADE_OUVRANTE linstr ACCOLADE_FERMANTE {$$ = cree_n_instr_bloc($2);};

linstr : instr linstr {$$ = cree_n_l_instr($1, $2);}
	| {$$ = NULL;}; ;

instr : iaffect {$$ = $1;}
	| isi {$$ = $1;}
	| itantque {$$ = $1;}
	| iappel {$$ = $1;}
	| iretour {$$ = $1;};
	
iaffect : var EGAL expression POINT_VIRGULE {$$ = cree_n_instr_affect($1, $3);};

isi : SI expression ALORS ibloc sinonopt {$$=cree_n_instr_si($2, $4, $5);}

sinonopt : SINON iBloc {$$ = $2;}
	| {$$ = NULL;};
	
itantque : TANTQUE expression FAIRE ibloc {$$=cree_n_instr_tantque($2, $4);}

iappel : appelfct POINT_VIRGULE {$$=cree_n_instr_appel($1);}

iretour : RETOUR expression POINT_VIRGULE {$$=cree_n_instr_retour($2);};

appelfct : IDENTIF PARENTHESE_OUVRANTE lexp PARENTHESE_FERMANTE {$$=cree_n_appel($1, $3);}; 

lexp :	expression lexpbis {$$=cree_n_l_exp($1, $2);}
	| {$$ = NULL;};

lexpbis : VIRGULE expression lexpbis {$$ = cree_n_l_exp($2, $3);}
	| {$$ = NULL;};

expression : expression OU e1 {$$ = cree_n_exp_op(ou, $1, $3);}
	| e1 {$$ = cree_n_exp_lire();};

e1 : e1 ET e2 {$$ = cree_n_exp_op(et, $1, $3);}
	| e2{$$ = cree_n_exp_lire();};

e2 : e2 EGAL e3 {$$ = cree_n_exp_op(egal, $1, $3);}
	| e2 INFERIEUR e3 {$$ = cree_n_exp_op(inferieur, $1, $3);}
	| e3{$$ = cree_n_exp_lire();};

e3 : e3 PLUS e4 {$$ = cree_n_exp_op(plus, $1, $3);}
	| e3 MOINS e4 {$$ = cree_n_exp_op(moins, $1, $3);}
	| e4{$$ = cree_n_exp_lire();};

e4 : e4 FOIS e5 {$$ = cree_n_exp_op(fois, $1, $3);}
	| e4 DIVISE e5 {$$ = cree_n_exp_op(divise, $1, $3);}
	| e5{$$ = cree_n_exp_lire();};

e5 : NON e5 {$$ = cree_n_exp_op(non, $2, NULL);}
	| e6{$$ = cree_n_exp_lire();};

e6 : PARENTHESE_OUVRANTE e6 PARENTHESE_FERMANTE {$$ = ($2);}
	| NOMBRE {$$=cree_n_exp_entier($1);}
	| appelfct {$$=cree_n_exp_appel($1);}
	| var {$$=cree_n_exp_var($1);};

var : IDENTIF {$$ = cree_n_var_simple($1);}
	| IDENTIF CROCHET_OUVRANT expression CROCHET_FERMANT {$$ = cree_n_var_indicee($1, $3);};
	| appelfct {$$ = cree_n_exp_appel($1);};
	
//TODO: compléter avec les productions de la grammaire

%%

int yyerror(char *s) {
  fprintf(stderr, "erreur de syntaxe ligne %d\n", yylineno);
  fprintf(stderr, "%s\n", s);
  fclose(yyin);
  exit(1);
}

%{ open Oasis_ast %}

%token IF THEN ELSE
%token INDENT DEDENT
%token COLON PLUSCOLON DOLLARCOLON
%token NOT AND OR RPAREN LPAREN
%token EOF
%token <string> STRING
%token <string> IDENT

%right    OR
%right    AND
%nonassoc NOT

%start main
%type <Oasis_ast.top_stmt list> main

%%

main:
  list(top_stmt) EOF { [] }

top_stmt:
  | id=IDENT name=id_or_string s=stmt
    { Section (id, name, s) }
  | s=simple_stmt { Stmt s }
  | INDENT l=list(top_stmt) DEDENT { TopBlock l }

stmt:
  | s=simple_stmt { s }
  | INDENT l=list(stmt) DEDENT { Block l }

simple_stmt:
  | field=IDENT o=field_op { Field (field, o) }
  | IF e=expr THEN s1=stmt ELSE s2=stmt
    { IfThenElse (e, s1, s2) }

field_op:
  | COLON s=STRING { FSet s }
  | PLUSCOLON s=STRING { FAdd s }
  | DOLLARCOLON e=expr { FEval e }

expr:
  | NOT e=expr { Not e }
  | e1=expr f=binop e2=expr { f e1 e2 }
  | id=IDENT { Ident id }
  | LPAREN e=expr RPAREN { e }
  | f=IDENT LPAREN x=IDENT RPAREN { Test (f,x) }

%inline binop:
  | AND { fun e1 e2 -> And (e1, e2) }
  | OR  { fun e1 e2 -> Or (e1, e2) }

id_or_string:
  | s=IDENT | s=STRING { s }

list_with_indent(X):
  | /* nothing */
    { [] }
  | x=X xs=list_with_indent(X)
    { x :: xs }
  | INDENT l=list_with_indent(X) DEDENT { l }

/* The parser definition */
%{

let parse_error s =
  Printf.printf "%s\n" s ;
  flush stdout

%}

/* Keywords */
%token AUTO BREAK CASE CHAR CONST DEFAULT DO DOUBLE
%token ELSE ENUM EXTERN FLOAT FOR GOTO IF INT LONG
%token REGISTER RESTRICT RETURN STATIC SHORT SIGNED 
%token SIZEOF STRUCT SWITCH TYPEDEF UNION UNSIGNED VOID 
%token VOLATILE WHILE EOF

%token INLINE

/* Punctuators */
%token LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE DOT ARROW 
%token INC DEC AND STAR PLUS MINUS NEG BANG DIV MOD XOR OR
%token LSHIFT RSHIFT LT GT LTEQ GTEQ EQEQ NEQ ANDAND OROR
%token TERNARY COLON SEMI DOTS EQ MULTEQ DIVEQ MODEQ PLUSEQ 
%token MINUSEQ LSHIFTEQ RSHIFTEQ ANDEQ OREQ XOREQ COMMA

/* Extra */
%token UNKNOWN

%token <Ast.value> RUNELIT
%token <Ast.value> CHARLIT
%token <Ast.value> STRLIT
%token <Ast.value> INTLIT
%token <Ast.value> FLOATLIT

%token <string> IDENT 
%token <string> TYPE

%start implementation
%type <Ast.node list> implementation

%%
implementation
  : external_decls EOF { $1 }
  ; 

external_decls
  : external_decl { [$1] }
  | external_decls external_decl { $2 :: $1 }
  ;

external_decl
  : decl SEMI { $1 }
  ;

decl
  : decl_specifiers init_declarators_opt { Ast.Nil }
  ;

decl_specifiers
  : storage_specifier       { ([$1],[],[],[],[]) }
  | type_specifier          { ([],[$1],[],[],[]) }
  | type_qualifier          { ([],[],[$1],[],[]) }
  | func_specifier          { ([],[],[],[$1],[]) }
  /*| alignment_specifier     { ([],[],[],[],[$1]) }*/
  | decl_specifiers storage_specifier
    { let (a,b,c,d,e) = $1 in ($2::a,b,c,d,e) }
  | decl_specifiers type_specifier
    { let (a,b,c,d,e) = $1 in (a,$2::b,c,d,e) }
  | decl_specifiers type_qualifier
    { let (a,b,c,d,e) = $1 in (a,b,$2::c,d,e) }
  | decl_specifiers func_specifier
    { let (a,b,c,d,e) = $1 in (a,b,c,$2::d,e) }
  /*| decl_specifiers alignment_specifier
    { let (a,b,c,d,e) = $1 in (a,b,c,d,$2::e) }*/
  ;
  
storage_specifier
  : EXTERN    { Ast.Extern_sto }
  | STATIC    { Ast.Static_sto }
  | AUTO      { Ast.Auto_sto }
  | REGISTER  { Ast.Register_sto }
  | TYPEDEF   { Ast.Typedef_sto }
  ;

type_specifier
  : struct_spec   { $1 }
  | type_node     { $1 }
  ;

type_node
  : TYPE          { Ast.Nil }
  ;

type_qualifier
  : CONST     { Ast.Const_qual } 
  | RESTRICT  { Ast.Restrict_qual }
  | VOLATILE  { Ast.Volatile_qual }
  ;

type_qualifiers
  : /* empty */   { [] }
  | type_qualifiers type_qualifier { $2 :: $1 }
  ;

func_specifier
  : INLINE { Ast.Inline_spec }
  ;

struct_spec
  : STRUCT name { Ast.Nil }
  | STRUCT name_opt LBRACE struct_decls_opt RBRACE {Ast.Nil}
  ;

struct_decls_opt
  : /* empty */   { None }
  | struct_decls  { Some $1 }
  ; 

struct_decls
  : struct_decl { [$1] }
  | struct_decls struct_decl {$2 :: $1 }
  ;

struct_decl
  : specifier_qualifiers struct_declarator_opt SEMI { Ast.Nil }
  ;

specifier_qualifiers
  : type_specifier { ([$1],[]) }
  | type_qualifier { ([],[$1]) }
  | specifier_qualifiers type_specifier
    { let (a,b) = $1 in ($2 :: a, b) }
  | specifier_qualifiers type_qualifier
    { let (a,b) = $1 in (a, $2 :: b) }
  ; 

struct_declarator_opt
  : /* empty */ { None }
  | struct_declarators { Some $1 }
  ; 

struct_declarators
  : struct_declarator { [$1] }
  | struct_declarators COMMA struct_declarator { $3 :: $1 }
  ;

struct_declarator
  : init_declarator { $1 }
  ;
    
init_declarators_opt
  : /* empty */ { None }
  | init_declarators { Some $1 }
  ; 

init_declarators
  : init_declarator { [$1] }
  | init_declarators COMMA init_declarator { $3 :: $1 }
  ;

init_declarator
  : declarator          { Ast.Nil }
  /*| declarator EQ init  { Ast.Nil }*/
  ;

declarator
  : pointer_opt direct_declarator { Ast.Nil }
  ;

/*
init
  : EOF { Ast.Nil }
  ;*/

pointer_opt
  : /* empty */ { None }
  | pointer   { Some $1 }
  ; 

pointer
  : STAR type_qualifiers            { Ast.Nil }
  | STAR type_qualifiers pointer  { Ast.Nil }
  ;

direct_declarator
  : name { $1 }
  ;

name: 
  IDENT { Ast.Nil } 
  ;

name_opt
  : /* empty */ { Ast.Nil } 
  | name        { $1 }
  ; 

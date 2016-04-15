/* The parser definition */
%{

open Compile

(* Stateful parsing for declaration_specifiers: Semantic information
 * has too little order to pass up from semantics actions. We would
 * need a 5 tuple. *)
let curr_storage_spec = ref Ast.Nil_sto
let curr_type_quals : (Ast.type_qual list ref) = ref [] 
let curr_type_specs : (Ast.node list ref) = ref []
let curr_func_specs : (Ast.func_spec list ref) = ref []

let curr_decl_valid = ref true

let reset_decl_state () =
  curr_storage_spec := Ast.Nil_sto ;
  curr_type_quals := [] ;
  curr_type_specs := [] ;
  curr_func_specs := [] ;
  curr_decl_valid := true
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
  : decl SEMI { reset_decl_state () ; $1 }
  ;

decl
  : decl_specifiers init_declarators_opt { Ast.Nil }
  ;

decl_specifiers
  : storage_specifier       
    { curr_storage_spec := $1 }
  | type_specifier          
    { curr_type_specs := $1 :: !curr_type_specs }
  | type_qualifier          
    { curr_type_quals := $1 :: !curr_type_quals }
  | func_specifier          
    { curr_func_specs := $1 :: !curr_func_specs }
  | decl_specifiers storage_specifier
    { match !curr_storage_spec with
      | Ast.Nil_sto -> curr_storage_spec := $2 
      | _ -> 
        Errors.errorf errors (Location.curr_yacc $startpos) "multiple storage classes in declaration specifiers" ; 
        curr_decl_valid := false } 
  | decl_specifiers type_specifier
    { curr_type_specs := $2 :: !curr_type_specs }
  | decl_specifiers type_qualifier
    { curr_type_quals := $2 :: !curr_type_quals }
  | decl_specifiers func_specifier
    { curr_func_specs := $2 :: !curr_func_specs }
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
  : STRUCT symstr=name 
    { 
      let sym = Decl.new_struct_name symstr in
      Ast.Name sym
    }
    
  | nme=struct_spec_head LBRACE fds=struct_decls_opt RBRACE
    {
      Ast.Struct (nme,fds)
    }
  ;

struct_spec_head
  : STRUCT name_opt 
    { 
      match $2 with
      | Some s ->
        let sym = Decl.new_struct_name s in
        let open Ast in
        sym.stype <- {typ=Incomplete_typ; quals=[]; } ;
        Some (Ast.Name sym)
      | None -> None 
    } 
  ; 

struct_decls_opt
  : /* empty */   { [] }
  | struct_decls  { $1 }
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
  : STAR type_qualifiers          { Ast.Nil }
  | STAR type_qualifiers pointer  { Ast.Nil }
  ;

direct_declarator
  : name { $1 }
  ;

name
  : IDENT { $1 } 
  ;

name_opt
  : /* empty */ { None } 
  | name        { Some $1 }
  ; 

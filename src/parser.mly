/* The parser definition */
%{

open Compile

(* Stateful parsing for declaration_specifiers: Semantic information
 * has too little order to pass up from semantics actions. We would
 * need a 5 tuple. *)
let curr_storage_spec = ref Ast.Nil_sto
let curr_type_quals : (Ast.type_qual list ref) = ref [] 
let lasttype = ref Decl.ctype_nil
let curr_func_specs : (Ast.func_spec list ref) = ref []

let curr_decl_valid = ref true

let reset_decl_state () =
  curr_storage_spec := Ast.Nil_sto ;
  curr_type_quals := [] ;
  lasttype := Decl.ctype_nil ;
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
%type <unit> implementation

%%
implementation
  : xdecls EOF { () }
  ; 

xdecls
  : xdecl { () }
  | xdecls xdecl { () }
  ;

xdecl
  : decl_specifiers SEMI { reset_decl_state () }
  | decl_specifiers xdecllist SEMI { reset_decl_state () }
  ;

decl_specifiers
  : storage_specifier       
    { curr_storage_spec := $1 }
  | types          
    { lasttype := $1 ; () }
  | tqual          
    { curr_type_quals := $1 :: !curr_type_quals }
  | func_specifier          
    { curr_func_specs := $1 :: !curr_func_specs }
  | decl_specifiers storage_specifier
    { match !curr_storage_spec with
      | Ast.Nil_sto -> curr_storage_spec := $2 
      | _ -> 
        Errors.errorf errors (Location.curr_yacc $startpos) 
          "multiple storage classes in declaration specifiers" ; 
        curr_decl_valid := false } 
  | decl_specifiers types
    { lasttype := $2 ; () }
  | decl_specifiers tqual
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

types
  : complex     { $1 }
  | tname       { $1 }
  ;

tname
  : TYPE          
    { 
      let open Ast in
      let symopt = Decl.lookup_sym $1 in
      match symopt with
      | Some sym  -> sym.stype
      | _         ->
        raise (Misc.Internal_error (Format.sprintf "expected to find symbol for %s" $1))
    }
  ;

tqual
  : CONST     { Ast.Const_qual } 
  | RESTRICT  { Ast.Restrict_qual }
  | VOLATILE  { Ast.Volatile_qual }
  ;

tquals
  : /* empty */   { [] }
  | tquals tqual { $2 :: $1 }
  ;

func_specifier
  : INLINE { Ast.Inline_spec }
  ;

complex
  : STRUCT IDENT
    { 
      let open Ast in
      let structid = "struct::" ^ $2 in
      let sym = Decl.declare_incomplete structid in
      sym.stype
    }
    
  | struct_head LBRACE struct_decls_opt RBRACE
    {
      let open Ast in
      let nd = Struct ($1.name,$3) in
      Decl.define_incomplete $1 nd ;
      $1.stype
    }
  ;

struct_head
  : STRUCT 
    { 
      let structid = Decl.gen_anon_struct () in
      Decl.declare_incomplete structid 
    }
  | STRUCT IDENT
    { 
      let structid = "struct::" ^ $2 in
      Decl.declare_incomplete structid 
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
  : types { ([$1],[]) }
  | tqual { ([],[$1]) }
  | specifier_qualifiers types
    { let (a,b) = $1 in ($2 :: a, b) }
  | specifier_qualifiers tqual
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
  : xdeclor { $1 }
  ;
    
xdecllist
  : xdeclor                 { Decl.declare $1 !lasttype ; () }
  | xdecllist COMMA xdeclor { Decl.declare $3 !lasttype ; () }
  ;

xdeclor
  : STAR tquals xdeclor  { Ast.Pointer_hp $3 }
  | xdeclor2      { $1 }
  ; 

xdeclor2
  : IDENT                           { Ast.Name_hp $1 }
  | LPAREN xdeclor RPAREN           { $2 }
  | xdeclor2 LPAREN arglist RPAREN  { Ast.Func_hp ($1,$3) }
  | xdeclor2 LBRACK expr RBRACK     { Ast.Array_hp $1 }
  ;

arglist
  : TYPE { [] }
  ;

expr
  : /* empty */ { Ast.Nil }
  








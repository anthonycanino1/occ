/* The parser definition */
%{

open Ast
open Compile

(* Stateful parsing for declaration_specifiers: Semantic information
 * has too little order to pass up from semantics actions. We would
 * need a 5 tuple. *)
let curr_type_quals : (type_qual list ref) = ref [] 

let lasttype = ref Decl.nilctype
let lastclass = ref Nil_sto
let lastdecl = ref Decl.nilctype
let lastdeclor = ref (Name_hp "")

(* Not sure if I need this yet *)
let lastdecl = ref 

let tcstack : (ctype * storage_class) list ref = ref []

(* Not very functional, not sure if I will keep *)
let pxdecls : node list ref = ref []

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

%left SEMI
%left COMMA
%right LBRACK LPAREN

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
  : tcqlist SEMI { () }
  | tcqlist xdlist SEMI { () }
  | fdeclopen fbody 
    { 
      Decl.pop_decls () ;
      let (Function (s, params, _)) = $1 in
      let nd = Function (s, params, $2) in
      pxdecls := nd :: !pxdecls
    }
  ;
  
tcqlist 
  : types 
    { 
      lasttype := $1
    }
  | cname 
    { 
      lastclass := $1
    }
  | qname { () }
  | tcqlist types { () }
  | tcqlist cname { () }
  | tcqlist qname { () }
  ; 

tqlist
  : types 
    { 
      lasttype := $1
    }
  | qname { () }
  | tqlist types { () }
  | tqlist qname { () }
  ; 

cname
  : EXTERN    { Extern_sto }
  | STATIC    { Static_sto }
  | AUTO      { Auto_sto }
  | REGISTER  { Register_sto }
  | TYPEDEF   { Typedef_sto }
  ;

types
  : complex     { $1 }
  | tname       { $1 }
  ;

tname
  : TYPE          
    { 
      let symopt = Decl.lookup_sym $1 in
      match symopt with
      | Some sym  -> sym.stype
      | _         ->
        raise (Misc.Internal_error (Format.sprintf "expected to find symbol for %s" $1))
    }
  ;

qname
  : CONST     { Constq } 
  | RESTRICT  { Restrictq }
  | VOLATILE  { Volatileq }
  ;

qlist
  : { () }
  | qlist qname   { () }
  ;

complex
  : STRUCT IDENT
    { 
      let structid = "struct::" ^ $2 in
      let sym = Decl.tag structid in
      sym.stype
    } 
  | struct_head sbody 
    {
      Decl.define_struct $1 $2 ;
      $1.stype
    }
  | UNION IDENT
    {
      let structid = "struct::" ^ $2 in
      let sym = Decl.tag structid in
      sym.stype
    }
  | union_head sbody
    {
      Decl.define_union $1 $2 ;
      $1.stype
    }

  ;

struct_head
  : STRUCT 
    { 
      let structid = Decl.gen_anon_struct () in
      Decl.tag structid 
    }
  | STRUCT IDENT
    { 
      let structid = "struct::" ^ $2 in
      Decl.tag structid 
    } 
  ; 

union_head
  : UNION 
    { 
      let structid = Decl.gen_anon_struct () in
      Decl.tag structid 
    }
  | UNION IDENT
    { 
      let structid = "struct::" ^ $2 in
      Decl.tag structid 
    } 
  ; 

sbody
  : LBRACE sbodyopen sdeclo RBRACE 
    {
      Decl.pop_decls () ;
      match !tcstack with
      | ((lt,lc) :: t) ->
        lasttype := lt ;
        lastclass := lc ;
        tcstack := t ;
        List.rev $3
      | [] -> raise (Misc.Internal_error "popping empty type stack") 
    }
  ;

sbodyopen
  : 
    {
      tcstack := (!lasttype, !lastclass) :: !tcstack ;
      Decl.mark_scope ()
    }
  ;

sdeclo
  : { [] }
  | sdecl  { $1 }
  ; 

sdecl
  : tqlist sdlist SEMI { $2 }
  | sdecl tqlist sdlist SEMI { $3 @ $1 }
  ;

sdlist
  : sdeclor 
    { 
      let st = Decl.declare Decl.sdeclare $1 !lasttype !lastclass in
      [st]
    }
  | sdlist COMMA sdeclor 
    {
      let st = Decl.declare Decl.sdeclare $3 !lasttype !lastclass in
      st :: $1
    }
  ;

sdeclor
  : xdeclor { $1 }
  ;
    
xdlist
  : xdeclor              { Decl.declare Decl.xdeclare $1 !lasttype !lastclass ; () }
  | xdlist COMMA xdeclor { Decl.declare Decl.xdeclare $3 !lasttype !lastclass ; () }
  ;

xdeclor
  : STAR qlist xdeclor  { Pointer_hp $3 }
  | xdeclor2      { $1 }
  ; 

/* TODO : Need to make sure arglist obeys scoping */
xdeclor2
  : IDENT                           { Name_hp $1 }
  | LPAREN xdeclor RPAREN           { $2 }
  | xdeclor2 LPAREN arglist RPAREN  { Func_hp ($1,$3) }
  | xdeclor2 LBRACK expr RBRACK     { Array_hp $1 }
  ;

arglist
  : arglistopen argdeclo 
    { 
      match !tcstack with
      | ((lt,lc) :: t) ->
        lasttype := lt ;
        lastclass := lc ;
        tcstack := t ;
        List.rev $2
      | [] -> raise (Misc.Internal_error "popping empty type stack") 
    }
  ;

arglistopen
  : 
    {
      tcstack := (!lasttype, !lastclass) :: !tcstack ;
    }
  ;

argdeclo
  : { [] }
  | argdecl     { $1 }
  ;

argdecl
  : argdecl1                { [$1] }
  | argdecl COMMA argdecl1  { $3 :: $1 }
  ;

argdecl1
  : tqlist
    { (Decl.nilsym, !lasttype) }
  | tqlist xdeclor 
    {
      Decl.declare Decl.nodeclare $2 !lasttype !lastclass 
    }
  | tqlist abdeclor
    {
      Decl.declare Decl.pdeclare $2 !lasttype !lastclass 
    }
  ; 

abdeclor
  : STAR qlist            { Pointer_hp (Proto_hp) }
  | STAR qlist abdeclor   { Pointer_hp ($3) }
  | abdeclor2             { $1 }
  ;

abdeclor2
  : abdeclor3                        { $1 }
  | abdeclor2 LPAREN arglist RPAREN  { Func_hp ($1,$3) }
  | abdeclor2 LBRACK expr RBRACK     { Array_hp $1 }
  ; 

abdeclor3
  : LPAREN RPAREN           { Func_hp (Proto_hp, []) }
  | LBRACK expr RBRACK      { Array_hp (Proto_hp) } 
  | LPAREN abdeclor RPAREN  { $2 }
  ;

/*
pdeclo
  : { [] }
  | pdecl       { $1 }
  ;

pdecl
  : tqlist pdeclor 
    { 
      let (_,typ) = Decl.declare Decl.sdeclare $2 !lasttype !lastclass in
      [typ]
    }
  | pdecl COMMA tqlist pdeclor 
    { 
      let (_,typ) = Decl.declare Decl.sdeclare $4 !lasttype !lastclass in
      typ :: $1
    } 
  ;
  */

pdeclor
  : xdeclor { $1 }
  ;

fdeclopen
  : tcqlist xdeclor 
    {
      let (s,t) = Decl.declare Decl.xdeclare $2 !lasttype !lastclass in
      match t.typ with
      | Function_typ (rtyp, ptyps) ->
        Decl.mark_scope () ;
        (* Push declarations back on the stack, its ugly *)
        List.iter (fun (s',_) -> Decl.push_sym s') ptyps ;
        let params = (List.fold_right (fun (s',_) ps -> s' :: ps) ptyps []) in
        Function (s, params, [])
      | _ ->
        Errors.errorf errors Location.dummy "%s not a function" s.name ; 
        Nil
    }
  ;

fbody
  : LBRACE RBRACE { [] }
  ;

expr
  : { Nil }

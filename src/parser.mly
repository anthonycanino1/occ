/* The parser definition */
%{

%}

/* Keywords */
%token AUTO BREAK CASE CHAR CONST DEFAULT DO DOUBLE
%token ELSE ENUM EXTERN FLOAT FOR GOTO IF INT LONG
%token REGISTER RETURN SHORT SIGNED SIZEOF STRUCT
%token SWITCH TYPEDEF UNION UNSIGNED VOID VOLATILE
%token WHILE EOF

/* Punctuators */
%token LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE DOT ARROW 
%token INC DEC AND STAR PLUS MINUS NEG BANG DIV MOD XOR OR
%token LSHIFT RSHIFT LT GT LTEQ GTEQ EQEQ NEQ ANDAND OROR
%token TERNARY COLON SEMI DOTS EQ MULTEQ DIVEQ MODEQ PLUSEQ 
%token MINUSEQ LSHIFTEQ RSHIFTEQ ANDEQ OREQ XOREQ COMMA

/* Extra */
%token UNKNOWN

%token <Type.value> RUNELIT
%token <Type.value> CHARLIT
%token <Type.value> STRLIT
%token <Type.value> INTLIT
%token <Type.value> FLOATLIT
%token <string> IDENT 

%start implementation
%type <unit> implementation

%%
implementation:
  CHARLIT { () }
  | error { () }
;


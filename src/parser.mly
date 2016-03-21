/* The parser definition */
%{
%}

/* Tokens */
%token AUTO
%token BREAK
%token CASE
%token CHAR
%token CONST
%token DEFAULT
%token DO
%token DOUBLE
%token ELSE
%token ENUM
%token EXTERN
%token FLOAT
%token FOR
%token GOTO
%token IF
%token INT
%token LONG
%token REGISTER
%token RETURN
%token SHORT
%token SIGNED
%token SIZEOF
%token STRUCT
%token SWITCH
%token TYPEDEF
%token UNION
%token UNSIGNED
%token VOID
%token VOLATILE
%token WHILE
%token EOF

%token IDENT

%start implementation
%type <unit> implementation

%%
implementation:
  EOF { () }
;


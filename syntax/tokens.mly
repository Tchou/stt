//Parameterized tokens
%token <Stt.Base.Hstring.t> ENUM
%token <Uchar.t> CHAR
%token <Stt.Base.Hstring.t> IDENT
%token <Z.t> INT
%token <Uchar.t array> STRING
%token <Stt.Base.Hstring.t> VAR

//Symbols
%token CAP "&"
%token COMMA ","
%token CUP "|"
%token DIFF "\\"
%token EQUAL "="
%token LP "("
%token LSB "["
%token MINUSGT "->"
%token MINUSMINUS "--"
%token NOT "~"
%token PLUS "+"
%token QMARK "?"
%token RP ")"
%token RSB "]"
%token SEMI ";"
%token STAR "*"

//Keywords
%token AND "and"
%token FROM "from"
%token TYPE "type"
%token WHERE "where"


//end of : file, phrase
%token EOF EOP

//Precedences:
//Most of the rules are writen in a stratified way to have explicit "levels"
//That one can jump into from other productions (e.g. reference only simple types from regexp)
//Token precedences are used only to simplify auxiliary rules when it does not
//Make sense to introduce an explicit stratification in between components.
%right "where" "and"
%%
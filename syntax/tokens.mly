%token <Z.t> INT
%token <Uchar.t> CHAR
%token <Stt.Base.Hstring.t> ATOM
%token <Stt.Base.Hstring.t> IDENT
%token <Stt.Base.Hstring.t> VAR
%token EQUAL "="
%token TYPE "type"
%token MINUSMINUS "--"
%token MINUSGT "->"
%token OR "|"
%token AND "&"
%token DIFF "\\"
%token NOT "~"
%token STAR "*"
%token PLUS "+"
%token QMARK "?"
%token COMMA ","
%token SEMI ";"
%token LP "("
%token RP ")"
%token LSB "["
%token RSB "]"
%token EOF EOP /* end of : file, phrase */
%%
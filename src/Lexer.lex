import java_cup.runtime.*;

%%

%class Lexer
%unicode
%line
%column
%cup


%{
    StringBuffer string = new StringBuffer();

    private Symbol symbol(int type) {
        return new Symbol(type, yyline, yycolumn);
    }

    private Symbol symbol(int type, Object value) {
        return new Symbol(type, yyline, yycolumn, value);
    }
%}

/* character class */
LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]

WhiteSpace = {LineTerminator} | [ \t\f]

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment}

TraditionalComment = "/#" [^#] ~"#/" | "/#" "#"+ "/"
EndOfLineComment = "#" {InputCharacter}* {LineTerminator}?

/* identifiers */
Identifier = [a-zA-Z][:jletterdigit:]*

/* integer literals */
IntegerLiteral = 0 | [1-9][0-9]*

/* boolean literals */
BooleanLiteral = "T" | "F"

/* floating point literals */
FloatLiteral = {IntegerLiteral} \. [0-9]*
RationalLiteral = ({WholeNumber}"_")? {IntegerLiteral}"/"{WholeNumber}
WholeNumber = [1-9][0-9]*

/* string and character literals */
StringCharacter = [^\r\n\"\\]
SingleCharacter = [^\r\n\'\\]

%state STRINGLITERAL, CHARLITERAL

%%

<YYINITIAL> {

  /* keywords */
  "int"                          { return symbol(sym.INT); }
  "char"                         { return symbol(sym.CHAR); }
  "float"                        { return symbol(sym.FLOAT); }
  "rat"                          { return symbol(sym.RAT); }
  "bool"                         { return symbol(sym.BOOL); }

  "seq"                          { return symbol(sym.SEQ); }
  "set"                          { return symbol(sym.SET); }
  "dict"                         { return symbol(sym.DICT); }

  "top"                          { return symbol(sym.TOP); }
  "thread"                       { return symbol(sym.THREAD); }
  "function"                     { return symbol(sym.FUNCTION); }

  "if"                           { return symbol(sym.IF); }
  "fi"                           { return symbol(sym.FI); }
  "then"                         { return symbol(sym.THEN); }
  "elif"                         { return symbol(sym.ELIF); }
  "else"                         { return symbol(sym.ELSE); }

  "while"                        { return symbol(sym.WHILE); }
  "forall"                       { return symbol(sym.FORALL); }
  "do"                           { return symbol(sym.DO); }
  "od"                           { return symbol(sym.OD); }
  "break"                        { return symbol(sym.BREAK); }
  "return"                       { return symbol(sym.RETURN); }
  "tdef"                         { return symbol(sym.TDEF); }
  "fdef"                         { return symbol(sym.FDEF); }
  "print"                        { return symbol(sym.PRINT); }
  "read"                         { return symbol(sym.READ); }
  "main"                         { return symbol(sym.MAIN); }
  "alias"                        { return symbol(sym.ALIAS); }
  "in"                           { return symbol(sym.IN); }

  /* separators */
  "("                            { return symbol(sym.LPAREN); }
  ")"                            { return symbol(sym.RPAREN); }
  "{"                            { return symbol(sym.LBRACE); }
  "}"                            { return symbol(sym.RBRACE); }
  "["                            { return symbol(sym.LBRACK); }
  "]"                            { return symbol(sym.RBRACK); }
  ";"                            { return symbol(sym.SEMI); }
  ","                            { return symbol(sym.COMMA); }
  \.                             { return symbol(sym.DOT); }
  
  /* operators */
  "!"                            { return symbol(sym.NOT);}
  "&&"                           { return symbol(sym.AND);}
  \|\|                           { return symbol(sym.OR);}
  "=="                           { return symbol(sym.EQEQ);}
  "!="                           { return symbol(sym.NOTEQ);}
  ":"                            { return symbol(sym.COLON);}
  "::"                           { return symbol(sym.CONS);}
  "+"                            { return symbol(sym.PLUS);}
  "-"                            { return symbol(sym.MINUS);}
  "*"                            { return symbol(sym.MULT);}
  "/"                            { return symbol(sym.DIV);}
  \^                             { return symbol(sym.EXP);}
  ":="                           { return symbol(sym.EQ);}
  \|                             { return symbol(sym.VBAR);}
  \&                             { return symbol(sym.INTERSECTION);}
  \\                             { return symbol(sym.DIFF);}
  "<"                            { return symbol(sym.LT);}
  "<="                           { return symbol(sym.LTEQ);}
  ">"                            { return symbol(sym.GT);}
  ">="                           { return symbol(sym.GTEQ);}
  "->"                           { return symbol(sym.ARROW);}

  /* boolean literal */
  {BooleanLiteral}               { return symbol(sym.BOOL_LITERAL, yytext()); }
  
  /* string literal */
  \"                             { yybegin(STRINGLITERAL); string.setLength(0); }

  /* character literal */
  \'                             { yybegin(CHARLITERAL); }

  /* numeric literals */
  
  {IntegerLiteral}               { return symbol(sym.INTEGER_LITERAL, yytext()); }
  
  {FloatLiteral}                 { return symbol(sym.FLOAT_LITERAL, yytext()); }

  {RationalLiteral}              { return symbol(sym.RAT_LITERAL, yytext()); }
  
  /* comments */
  {Comment}                      { /* ignore */ }

  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }

  /* identifiers */ 
  {Identifier}                   { return symbol(sym.IDENTIFIER, yytext()); }  
}

<STRINGLITERAL> {
  \"                             { yybegin(YYINITIAL); return symbol(sym.STRING_LITERAL, string.toString()); }
  
  {StringCharacter}+             { string.append( yytext() ); }
  
  /* escape sequences */
  "\\b"                          { string.append( '\b' ); }
  "\\t"                          { string.append( '\t' ); }
  "\\n"                          { string.append( '\n' ); }
  "\\f"                          { string.append( '\f' ); }
  "\\r"                          { string.append( '\r' ); }
  "\\\""                         { string.append( '\"' ); }
  "\\'"                          { string.append( '\'' ); }
  "\\\\"                         { string.append( '\\' ); }
  
  /* error cases */
  \\.                            { throw new RuntimeException("Illegal escape sequence \""+yytext()+"\""); }
  {LineTerminator}               { throw new RuntimeException("Unterminated string at end of line"); }
}

<CHARLITERAL> {
  {SingleCharacter}\'            { yybegin(YYINITIAL); return symbol(sym.CHAR_LITERAL, yytext().charAt(0)); }
  
  /* escape sequences */
  "\\b"\'                        { yybegin(YYINITIAL); return symbol(sym.CHAR_LITERAL, '\b');}
  "\\t"\'                        { yybegin(YYINITIAL); return symbol(sym.CHAR_LITERAL, '\t');}
  "\\n"\'                        { yybegin(YYINITIAL); return symbol(sym.CHAR_LITERAL, '\n');}
  "\\f"\'                        { yybegin(YYINITIAL); return symbol(sym.CHAR_LITERAL, '\f');}
  "\\r"\'                        { yybegin(YYINITIAL); return symbol(sym.CHAR_LITERAL, '\r');}
  "\\\""\'                       { yybegin(YYINITIAL); return symbol(sym.CHAR_LITERAL, '\"');}
  "\\'"\'                        { yybegin(YYINITIAL); return symbol(sym.CHAR_LITERAL, '\'');}
  "\\\\"\'                       { yybegin(YYINITIAL); return symbol(sym.CHAR_LITERAL, '\\');}
  
  /* error cases */
  \\.                            { throw new RuntimeException("Illegal escape sequence \""+yytext()+"\""); }
  {LineTerminator}               { throw new RuntimeException("Unterminated character literal at end of line"); }
}

/* error fallback */
[^]                              { throw new RuntimeException("Illegal character \"" + yytext() + "\" at line " + yyline + ", column " + yycolumn); }























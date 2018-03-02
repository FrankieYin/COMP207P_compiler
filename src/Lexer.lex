import java_cup.runtime.*;

%%

%class Lexer
%unicode
%cup
%line
%column


%eofval{
  return symbol(sym.EOF);
%eofval}

%{
  StringBuffer string = new StringBuffer();

  private Symbol symbol(int type) {
    return new Symbol(type, yyline, yycolumn);
  }

  private Symbol symbol(int type, Object value) {
    return new Symbol(type, yyline, yycolumn, value);
  }

	private void error(){
		throw new Error("Syntax error at line " + (yyline+1) + ", column "
				+ yycolumn);
	}
%}

/* main character classes */
LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]
WhiteSpace = {LineTerminator} | [ \t\f]

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment}

TraditionalComment   = "/#" ~"#/"
EndOfLineComment     = "#" {InputCharacter}* {LineTerminator}?

/* identifiers */
Identifier = [a-zA-z]][:jletterdigit:]*

/* string and character literals */
StringCharacter = [^\r\n\"\\]
SingleCharacter = [^\r\n\'\\]

/* numeric literals */
IntegerPart = 0 | [1-9][0-9]*
IntegerLiteral = [+-]? {IntegerPart}

RationalLiteral = ({IntegerLiteral}"_")? {IntegerPart} "/" {IntegerPart}
FloatLiteral = {IntegerLiteral} "." {IntegerPart}

%state STRINGLITERAL, CHARLITERAL

%%

<YYINITIAL> {
	/* keywords */
	"bool"												 { return symbol(sym.BOOLEAN); }
	"function"										 { return symbol(sym.FUNCTION); }
	"thread"										   { return symbol(sym.THREAD); }
	"dict"										     { return symbol(sym.DICT); }
	"set"										       { return symbol(sym.SET); }
	"seq"										       { return symbol(sym.SEQ); }
	"int"										       { return symbol(sym.INT); }
	"char"										     { return symbol(sym.CHAR); }
	"string"										   { return symbol(sym.STRING); }
	"top"										       { return symbol(sym.TOP); }
	"rat"										       { return symbol(sym.RAT); }
	"float"										     { return symbol(sym.FLOAT); }
	"in"										       { return symbol(sym.IN); }
	"tdef"										     { return symbol(sym.TDEF); }
	"fdef"										     { return symbol(sym.FDEF); }
	"alias"										     { return symbol(sym.ALIAS); }
	"return"										   { return symbol(sym.RETURN); }
	"if"										       { return symbol(sym.IF); }
	"fi"										       { return symbol(sym.FI); }
	"do"										       { return symbol(sym.DO); }
	"od"										       { return symbol(sym.OD); }
	"then"										     { return symbol(sym.THEN); }
	"elif"										     { return symbol(sym.ELIF); }
	"else"										     { return symbol(sym.ELSE); }
	"while"										     { return symbol(sym.WHILE); }
	"break"										     { return symbol(sym.BREAK); }
	"forall"									     { return symbol(sym.FORALL); }
	"print"										     { return symbol(sym.PRINT); }
	"read"										     { return symbol(sym.READ); }
	"main"										     { return symbol(sym.MAIN); }

	/* boolean literals */
	"T"										         { return symbol(sym.BOOLEAN_LITERAL, true); }
	"F"										         { return symbol(sym.BOOLEAN_LITERAL, false); }

	/* separators */
	"("                            { return symbol(sym.LPAREN); }
  ")"                            { return symbol(sym.RPAREN); }
  "{"                            { return symbol(sym.LBRACE); }
  "}"                            { return symbol(sym.RBRACE); }
  "["                            { return symbol(sym.LBRACK); }
  "]"                            { return symbol(sym.RBRACK); }
  ";"                            { return symbol(sym.SEMI); }
  ","                            { return symbol(sym.COMMA); }
  ":"                            { return symbol(sym.COLON); }
  "."                            { return symbol(sym.DOT); }

  /* operators */
  ":="                           { return symbol(sym.EQ); }
  "->"                           { return symbol(sym.RARROW); }
  ">"                            { return symbol(sym.GT); }
  "<"                            { return symbol(sym.LT); }
  "!"                            { return symbol(sym.NOT); }
  "&&"                           { return symbol(sym.ANDAND); }
  "||"                           { return symbol(sym.OROR); }
  "::"                           { return symbol(sym.CONS); }
  "=="                           { return symbol(sym.EQEQ); }
  "<="                           { return symbol(sym.LTEQ); }
  ">="                           { return symbol(sym.GTEQ); }
  "!="                           { return symbol(sym.NOTEQ); }
  "+"                            { return symbol(sym.PLUS); }
  "-"                            { return symbol(sym.MINUS); }
  "*"                            { return symbol(sym.MULT); }
  "/"                            { return symbol(sym.DIV); }
  "^"                            { return symbol(sym.EXP); }
  "&"                            { return symbol(sym.AND); }
  "|"                            { return symbol(sym.OR); }
  \\                             { return symbol(sym.DIFF); }

  /* string literal */
  \"                             { yybegin(STRINGLITERAL); string.setLength(0); }

  /* character literal */
  \'                             { yybegin(CHARLITERAL); }

  /* numeric literals */
  {IntegerLiteral}               { return symbol(sym.INTEGER_LITERAL, new Integer(yytext())); }
  {RationalLiteral}              { return symbol(sym.FLOAT_LITERAL, yytext()); }
  {FloatLiteral}                 { return symbol(sym.FLOAT_LITERAL, new Float(yytext())); }

  /* identifiers */ 
  {Identifier}                   { return symbol(sym.IDENTIFIER, yytext()); }

  /* comments */
  {Comment}                      { /* ignore */ }
 
  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }
}

<STRINGLITERAL> {
  \"                             { yybegin(YYINITIAL); 
  																 return symbol(sym.STRING_LITERAL, string.toString()); }

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
  \\.                            { error(); }
  {LineTerminator}               { error(); }
}

<CHARLITERAL> {
  {SingleCharacter}\'            { yybegin(YYINITIAL); return symbol(sym.CHARACTER_LITERAL, yytext().charAt(0)); }
  
  /* escape sequences */
  "\\b"\'                        { yybegin(YYINITIAL); return symbol(sym.CHARACTER_LITERAL, '\b');}
  "\\t"\'                        { yybegin(YYINITIAL); return symbol(sym.CHARACTER_LITERAL, '\t');}
  "\\n"\'                        { yybegin(YYINITIAL); return symbol(sym.CHARACTER_LITERAL, '\n');}
  "\\f"\'                        { yybegin(YYINITIAL); return symbol(sym.CHARACTER_LITERAL, '\f');}
  "\\r"\'                        { yybegin(YYINITIAL); return symbol(sym.CHARACTER_LITERAL, '\r');}
  "\\\""\'                       { yybegin(YYINITIAL); return symbol(sym.CHARACTER_LITERAL, '\"');}
  "\\'"\'                        { yybegin(YYINITIAL); return symbol(sym.CHARACTER_LITERAL, '\'');}
  "\\\\"\'                       { yybegin(YYINITIAL); return symbol(sym.CHARACTER_LITERAL, '\\'); }
  
  /* error cases */
  \\.                            { error(); }
  {LineTerminator}               { error(); }
}

/* error fallback */
[^]                              { error(); }

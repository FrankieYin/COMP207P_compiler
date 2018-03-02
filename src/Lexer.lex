import java_cup.runtime.*;

%%

%class Lexer
%unicode
%cup
%implements sym
%line
%column

%{
  StringBuffer string = new StringBuffer();

  private Symbol symbol(int type) {
    return new Symbol(type, yyline+1, yycolumn+1);
  }

  private Symbol symbol(int type, Object value) {
    return new Symbol(type, yyline+1, yycolumn+1, value);
  }

  private Double parseRational(String rstr){
  	int pos = rstr.indexOf('_');
  	Integer wholeNum = new Integer(rstr.substring(0, pos));
  	int op = rstr.indexOf('/');
  	Integer numerator = new Integer(rstr.substring(pos+1, op));
  	Integer denominator = new Integer(rstr.substring(op+1, rstr.length()));
  	return (Double) (wholeNum + numrator/denominator);
  }

	private void error(){
		report_error(
    	"Syntax error at line " + (yyline+1) + ", column "
				+ (yycolumn+1), null);
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
	"bool"												 { return symbol(BOOLEAN); }
	"function"										 { return symbol(FUNCTION); }
	"thread"										   { return symbol(THREAD); }
	"dict"										     { return symbol(DICT); }
	"set"										       { return symbol(SET); }
	"seq"										       { return symbol(SEQ); }
	"int"										       { return symbol(INT); }
	"char"										     { return symbol(CHAR); }
	"string"										   { return symbol(STRING); }
	"top"										       { return symbol(TOP); }
	"rat"										       { return symbol(RAT); }
	"float"										     { return symbol(FLOAT); }
	"in"										       { return symbol(IN); }
	"tdef"										     { return symbol(TDEF); }
	"fdef"										     { return symbol(FDEF); }
	"alias"										     { return symbol(ALIAS); }
	"return"										   { return symbol(RETURN); }
	"if"										       { return symbol(IF); }
	"fi"										       { return symbol(FI); }
	"do"										       { return symbol(DO); }
	"od"										       { return symbol(OD); }
	"then"										     { return symbol(THEN); }
	"elif"										     { return symbol(ELIF); }
	"else"										     { return symbol(ELSE); }
	"while"										     { return symbol(WHILE); }
	"break"										     { return symbol(BREAK); }
	"forall"									     { return symbol(FORALL); }
	"print"										     { return symbol(PRINT); }
	"read"										     { return symbol(READ); }
	"main"										     { return symbol(MAIN); }

	/* boolean literals */
	"T"										         { return symbol(BOOLEAN_LITERAL, true); }
	"F"										         { return symbol(BOOLEAN_LITERAL, false); }

	/* separators */
	"("                            { return symbol(LPAREN); }
  ")"                            { return symbol(RPAREN); }
  "{"                            { return symbol(LBRACE); }
  "}"                            { return symbol(RBRACE); }
  "["                            { return symbol(LBRACK); }
  "]"                            { return symbol(RBRACK); }
  ";"                            { return symbol(SEMI); }
  ","                            { return symbol(COMMA); }
  ":"                            { return symbol(COLON); }
  "."                            { return symbol(DOT); }

  /* operators */
  ":="                           { return symbol(EQ); }
  "->"                           { return symbol(RARROW); }
  ">"                            { return symbol(GT); }
  "<"                            { return symbol(LT); }
  "!"                            { return symbol(NOT); }
  "&&"                           { return symbol(ANDAND); }
  "||"                           { return symbol(OROR); }
  "::"                           { return symbol(CONS); }
  "=="                           { return symbol(EQEQ); }
  "<="                           { return symbol(LTEQ); }
  ">="                           { return symbol(GTEQ); }
  "!="                           { return symbol(NOTEQ); }
  "+"                            { return symbol(PLUS); }
  "-"                            { return symbol(MINUS); }
  "*"                            { return symbol(MULT); }
  "/"                            { return symbol(DIV); }
  "^"                            { return symbol(EXP); }
  "&"                            { return symbol(AND); }
  "|"                            { return symbol(OR); }
  \\                             { return symbol(DIFF); }

  /* string literal */
  \"                             { yybegin(STRING); string.setLength(0); }

  /* character literal */
  \'                             { yybegin(CHARLITERAL); }

  /* numeric literals */
  {IntegerLiteral}               { return symbol(INTEGER_LITERAL, new Integer(yytext())); }
  {RationalLiteral}              { return symbol(FLOAT_LITERAL, parseRational(yytext())); }
  {FloatLiteral}                 { return symbol(FLOAT_LITERAL, new Float(yytext())); }

  /* identifiers */ 
  {Identifier}                   { return symbol(IDENTIFIER, yytext()); }

  /* comments */
  {Comment}                      { /* ignore */ }
 
  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }
}

<STRINGLITERAL> {
  \"                             { yybegin(YYINITIAL); 
  																 return symbol(STRING_LITERAL, string.toString()); }

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
  \\.                            { error() }
  {LineTerminator}               { error() }
}

<CHARLITERAL> {
  {SingleCharacter}\'            { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, yytext().charAt(0)); }
  
  /* escape sequences */
  "\\b"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\b');}
  "\\t"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\t');}
  "\\n"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\n');}
  "\\f"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\f');}
  "\\r"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\r');}
  "\\\""\'                       { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\"');}
  "\\'"\'                        { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\'');}
  "\\\\"\'                       { yybegin(YYINITIAL); return symbol(CHARACTER_LITERAL, '\\'); }
  
  /* error cases */
  \\.                            { error() }
  {LineTerminator}               { error() }
}

/* error fallback */
[^]                              { error() }

<<EOF>>                          { return symbol(EOF); }

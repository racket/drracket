D                       [0-9]
L                       [a-zA-Z_]
H                       [a-fA-F0-9]
E                       [Ee][+-]?{D}+
FS                      (f|F|l|L)
IS                      (u|U|l|L)*

%{
#include <stdio.h>
#include <ctype.h>

void count();

static char yysourcefile[1024];

%}

%option yylineno

%%
"/*"                    { comment(); }
"//"                    { line_comment(); }
^" "*"#"                { cpp(); }

{L}({L}|{D})*           { count(); symbol(); }

0[xX]{H}+{IS}?          { count(); number(); }
0{D}+{IS}?              { count(); number(); }
{D}+{IS}?               { count(); number(); }
L?'(\\.|[^\\'])+'       { count(); character(); }

{D}+{E}{FS}?            { count(); number(); }
{D}*"."{D}+({E})?{FS}?  { count(); number(); }
{D}+"."{D}*({E})?{FS}?  { count(); number(); }

L?\"(\\.|[^\\"])*\"     { count(); string(); }

"#"                     { count(); h_symbol(); }
"##"                    { count(); hh_symbol(); }
"..."                   { count(); symbol(); }
">>="                   { count(); symbol(); }
"<<="                   { count(); symbol(); }
"+="                    { count(); symbol(); }
"-="                    { count(); symbol(); }
"*="                    { count(); symbol(); }
"/="                    { count(); symbol(); }
"%="                    { count(); symbol(); }
"&="                    { count(); symbol(); }
"^="                    { count(); symbol(); }
"|="                    { count(); boreq_symbol(); }
">>"                    { count(); symbol(); }
"<<"                    { count(); symbol(); }
"++"                    { count(); symbol(); }
"--"                    { count(); symbol(); }
"->"                    { count(); symbol(); }
"&&"                    { count(); symbol(); }
"||"                    { count(); or_symbol(); }
"<="                    { count(); symbol(); }
">="                    { count(); symbol(); }
"=="                    { count(); symbol(); }
"!="                    { count(); symbol(); }
";"                     { count(); xsymbol(); }
("{"|"<%")              { count(); start(); }
("}"|"%>")              { count(); stop('{'); }
","                     { count(); xsymbol(); }
"::"                    { count(); symbol(); }
":"                     { count(); symbol(); }
"="                     { count(); symbol(); }
"("                     { count(); start(); }
")"                     { count(); stop('('); }
("["|"<:")              { count(); start(); }
("]"|":>")              { count(); stop('['); }
"."                     { count(); xsymbol(); }
"&"                     { count(); symbol(); }
"!"                     { count(); symbol(); }
"~"                     { count(); symbol(); }
"-"                     { count(); symbol(); }
"+"                     { count(); symbol(); }
"*"                     { count(); symbol(); }
"/"                     { count(); symbol(); }
"%"                     { count(); symbol(); }
"<"                     { count(); symbol(); }
">"                     { count(); symbol(); }
"^"                     { count(); symbol(); }
"|"                     { count(); bor_symbol(); }
"?"                     { count(); symbol(); }

[ \t\v\n\f]             { count(); }
.                       { count(); error(); }

%%

yywrap()
{
        return(1);
}


comment()
{
        char c, c1;

loop:
        while ((c = input()) != '*' && c != 0)
	  ;

        if ((c1 = input()) != '/' && c != 0)
        {
	  unput(c1);
	  goto loop;
        }
}

line_comment()
{
  char c;
  
  while ((c = input()) != '\n' && c != 0)
    ;
}

cpp()
{
  char c, prev;
  int maybe_source = 0;

  /* Check for line-in-src annotation */
 loop:
  while ((c = input()) != '\n' && c != 0) {
    if (isdigit(c))
      maybe_source = (maybe_source * 10) + (c - '0');
    else if ((c == '"') && maybe_source) {
      char source_name[1024];
      int i = 0;

      while ((c = input()) != '\n' 
	     && (c != '"')
	     && (c != 0)) {
	if (i < 1024)
	  source_name[i++] = c;
	prev = c;
      }

      if (c == '"') {
	yylineno = maybe_source;
	source_name[i] = 0;
	strcpy(yysourcefile, source_name);
      }

      if (c != '"')
	break;
    }

    prev = c;
  }

  if (c && prev == '\\')
    goto loop;
}

int start_col = 0;
int start_line = 0;
int old_line = 0;
int column = 0;

void count()
{
        int i;

	start_col = column;
	start_line = old_line;
	old_line = yylineno;

        for (i = 0; yytext[i] != '\0'; i++)
                if (yytext[i] == '\n')
                        column = 0;
                else if (yytext[i] == '\t')
                        column += 8 - (column % 8);
                else
                        column++;
}

error()
{
  fprintf(stderr, "c-tokenize: [%d, %d] Unknown character in input: %s\n", yylineno, column, yytext);
  exit(-1);
}

print_rest(int close)
{
  if (yysourcefile[0]) {
    printf(" \"%s\" ", yysourcefile);
    yysourcefile[0] = 0;
  } else
    printf(" #f ");

  printf("%d %d%s\n", start_line, start_col, close ? ")" : "");
  
}

symbol()
{
  printf("(%s ", yytext);
  print_rest(1);
}

xsymbol()
{
  printf("(\\%s", yytext);
  print_rest(1);
}

or_symbol()
{
  printf("(\\|\\|");
  print_rest(1);
}

bor_symbol()
{
  printf("(\\|");
  print_rest(1);
}

boreq_symbol()
{
  printf("(\\|=");
  print_rest(1);
}

hh_symbol()
{
  printf("(\\#\\#");
  print_rest(1);
}

h_symbol()
{
  printf("(\\#");
  print_rest(1);
}

number()
{
  /* Check for octal: */
  int i;
  for (i = 0; yytext[i]; i++)
    if (!isdigit(yytext[i]))
      break;
  if (!yytext[i] && (yytext[0] == '0')) {
    printf("(#o%s", yytext);
    print_rest(1);
  } else
    return symbol();
}

character()
{
  int i;
  char *s;

  s = yytext;

  printf("(");

 again:

  for (i = 0; s[i]; i++)
    if (s[i] == '|')
      break;
  
  if (s[i]) {
    s[i] = 0;
    printf("|%s|\\|", s);
    s = s + i + 1;
    goto again;
  } else
    printf("|%s|", s);

  print_rest(1);
}

string()
{
  /* Produce a string such that (printf "\"~a\"" (read ...)) gives exactly the content
     of yytext. To do this, protect all backslashes. A backslash before
     a quote must be duplicated. */
  char *s;

  printf("(\"");
  for (s = yytext + 1; s[1]; s++) {
    if (*s == '\\') {
      putchar('\\');
      if (s[1] == '"' && s[2]) /* yytext ends with a non-escaped quote */
	putchar('\\');
    }
    putchar(*s);
  }

  printf("\"");
  print_rest(1);
}

start()
{
  printf("((\"%s\")", yytext);
  print_rest(0);
}

stop(int c)
{
  printf(")\n");
}

int main()
{
  printf("( ; start\n");
  yylex();
  printf(") ; end\n");
  return 0;
}

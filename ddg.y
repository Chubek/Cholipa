%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
%}

%token IDENTIFIER
%token INTEGER
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN SEMICOLON
%token EDGE ARROW DEPENDS

%%

program : /* empty */
        | program statement SEMICOLON

statement : node_declaration
          | edge_declaration
          | dependency_declaration

node_declaration : IDENTIFIER

edge_declaration : IDENTIFIER EDGE IDENTIFIER ARROW IDENTIFIER

dependency_declaration : IDENTIFIER DEPENDS IDENTIFIER

%%

int main() {
    yyparse();
    return 0;
}

int yyerror(const char *s) {
    fprintf(stderr, "Parser error: %s\n", s);
    return 1;
}

int yylex() {
    int c = getchar();

    if (c == EOF) {
        return 0; // End of file
    } else if (c == '\n' || c == ' ' || c == '\t') {
        return yylex(); // Ignore whitespace and newline
    } else if (c == '+') {
        return PLUS;
    } else if (c == '-') {
        return MINUS;
    } else if (c == '*') {
        return STAR;
    } else if (c == '/') {
        return SLASH;
    } else if (c == '(') {
        return LPAREN;
    } else if (c == ')') {
        return RPAREN;
    } else if (c == ';') {
        return SEMICOLON;
    } else if (c == '-') {
        c = getchar();
        if (c == '>') {
            return ARROW;
        } else {
            ungetc(c, stdin);
            return '-';
        }
    } else if (c == '=') {
        c = getchar();
        if (c == '>') {
            return DEPENDS;
        } else {
            ungetc(c, stdin);
            return '=';
        }
    } else if (isdigit(c)) {
        int value = c - '0';
        c = getchar();
        while (isdigit(c)) {
            value = value * 10 + (c - '0');
            c = getchar();
        }
        ungetc(c, stdin);
        yylval = value;
        return INTEGER;
    } else if (isalpha(c)) {
        char buffer[256];
        int i = 0;
        buffer[i++] = c;
        c = getchar();
        while (isalnum(c)) {
            buffer[i++] = c;
            c = getchar();
        }
        ungetc(c, stdin);
        buffer[i] = '\0';
        yylval = strdup(buffer);
        return IDENTIFIER;
    } else {
        return c;
    }
}


%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
%}

%token IDENTIFIER
%token LPAREN RPAREN SEMICOLON
%token ARROW

%%

program : /* empty */
        | program statement SEMICOLON

statement : node_declaration
          | edge_declaration
          | control_flow_statement

node_declaration : IDENTIFIER

edge_declaration : IDENTIFIER ARROW IDENTIFIER

control_flow_statement : IDENTIFIER LPAREN expression RPAREN ARROW IDENTIFIER

expression : IDENTIFIER
           | expression PLUS expression
           | expression MINUS expression
           | expression STAR expression
           | expression SLASH expression
           | LPAREN expression RPAREN

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
    } else if (isdigit(c)) {
        int value = c - '0';
        c = getchar();
        while (isdigit(c)) {
            value = value * 10 + (c - '0');
            c = getchar();
        }
        ungetc(c, stdin);
        yylval = value;
        return IDENTIFIER; // For simplicity, treating numbers as identifiers
    } else {
        return c;
    }
}


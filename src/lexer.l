%option noyywrap
%option nounput
%option noinput
%top{
    #include <stdarg.h>
    #include "common.h"
    #include "parser.h"
}
%{
    extern dump_type_t dump_type;

    inline void dump_tokens(const char* format, ...){
        va_list args;
        va_start(args, format);
        if (dump_type == TOKENS)
            vfprintf(yyout, format, args);
        va_end(args);
    }

    /* Your code here, if desired (lab3). */
%}

/* definitions section */

/* 锟斤拷锟斤拷锟斤拷锟斤拷 */
DECIMIAL ([1-9][0-9]*|0)
HEXADECIMAL "0x"([1-9a-eA-E][0-9a-eA-E]*|0)
OCTAL "0b"([1-7][0-7]*|0)
FLOAT [+-]?([1-9][0-9]*|0)?"."([0-9]*[1-9]|0)([eE][+-]?([1-9][0-9]*|0))?

ID [[:alpha:]_][[:alpha:][:digit:]_]*


EOL (\r\n|\n|\r)
WHITE [\t ]



/*  Your code here (lab3). */


/* 锟斤拷锟斤拷锟斤拷锟斤拷锟斤拷锟皆硷�?   */
ADD_type "+"?"+"
SUB_type "-"?"-"


/* 锟斤拷锟斤拷锟斤拷锟斤拷注锟酵和匡拷注锟斤拷 */

block_commentbegin "/*" 
block_commentelement .|\n
block_commentend "*/"
%x BLOCK_COMMENT

line_commentbegin "//" 
line_commentelement .
line_commentend \n
%x LINE_COMMENT




%%
    /* rules section */
"int" {
    dump_tokens("INT\t%s\n", yytext);//\t锟斤拷tab锟斤�?
    return INT;
}

"float" {
    dump_tokens("FLOAT\t%s\n", yytext);
    return FLOAT;
}


"void" {
    dump_tokens("VOID\t%s\n", yytext);
    return VOID;
}

"if" {
    dump_tokens("IF\t%s\n", yytext);
    return IF;
}

"else" {
    dump_tokens("ELSE\t%s\n", yytext);
    return ELSE;
}


"while" {
    dump_tokens("WHILE\t%s\n", yytext);
    return WHILE;
}




"return" {
    dump_tokens("RETURN\t%s\n", yytext);
    return RETURN;
}

"=" {
    dump_tokens("ASSIGN\t%s\n", yytext);
    return ASSIGN;
}

"<" {
    dump_tokens("LESS\t%s\n", yytext);
    return LESS;
}
">" {
    dump_tokens("GREATER\t%s\t%d\t%d\n", yytext);
    return GREATER;
}

"<=" {
    dump_tokens("LESSEQUAL\t%s\n", yytext);
    return LESSEQUAL;
}

">=" {
   dump_tokens("GREATEREQUAL\t%s\n", yytext);
    return GREATEREQUAL;
}
"==" {
   dump_tokens("EQUAL\t%s\n", yytext);
    return EQUAL;
}
"!=" {
   dump_tokens("NOTEQUAL\t%s\n", yytext);
    return NOTEQUAL;
}


{ADD_type} {
    int i = 0;
    while(yytext[i] != '\0')
    {
        i++;
    }
    if(i == 1)
    {
        dump_tokens("ADD\t%s\n", yytext);
        return ADD;
    }
    else if(i == 2)
    {
        dump_tokens("INCREMENT\t%s\n", yytext);
        return INCREMENT;
    }
}

{SUB_type} {
    int i = 0;
    while(yytext[i] != '\0')
    {
        i++;
    }
    if(i == 1)
    {
        dump_tokens("SUB\t%s\n", yytext);
        return SUB;
    }
    else if(i == 2)
    {
        dump_tokens("DECREMENT\t%s\n", yytext);
        return DECREMENT;
    }
}

"*" {
    dump_tokens("MUL\t%s\n", yytext);
    return MUL;
}

"/" {
    dump_tokens("DIV\t%s\n", yytext);
    return DIV;
}

"%" {
    dump_tokens("MOD\t%s\n", yytext);
    return MOD;
}
"&&" {
    dump_tokens("AND\t%s\n", yytext);
    return AND;
}

"||" {
    dump_tokens("OR\t%s \n", yytext);
    return OR;
}

"!" {
    dump_tokens("NOTEQUAL\t%s \n", yytext);
    return NOTEQUAL;
}



";" {
    dump_tokens("SEMICOLON\t%s\n", yytext);
    return SEMICOLON;
}

"," {
    dump_tokens("COMMA\t%s\n", yytext);
    return COMMA;
}


"(" {
    dump_tokens("LPAREN\t%s\n", yytext);
    return LPAREN;
}

")" {
    dump_tokens("RPAREN\t%s\n", yytext);
    return RPAREN;
}

"{" {
    dump_tokens("LBRACE\t%s\n", yytext);
    return LBRACE;
}

"}" {
    dump_tokens("RBRACE\t%s\n", yytext);
    return RBRACE;
}

"[" {
    dump_tokens("LBRACKET\t%s\n", yytext);
    return LBRACKET;
}

"]" {
    dump_tokens("RBRACKET\t%s\n", yytext);
    return RBRACKET;
}


{DECIMIAL} {
    int decimal;
    decimal = atoi(yytext);
    dump_tokens("DECIMAL\t%s\t%d\n", yytext, decimal);
    yylval.itype = decimal;
    return INTEGER;
}

{ID} {
    char *lexeme;
    dump_tokens("ID\t%s\n", yytext);
    lexeme = new char[strlen(yytext) + 1];
    strcpy(lexeme, yytext);
    yylval.strtype = lexeme;
    return ID;
}


{EOL} yylineno++;

{WHITE} {}


{block_commentbegin} {BEGIN BLOCK_COMMENT;}
<BLOCK_COMMENT>{block_commentelement} {}
<BLOCK_COMMENT>{block_commentend} {BEGIN INITIAL;}

{line_commentbegin} {BEGIN LINE_COMMENT;}
<LINE_COMMENT>{line_commentelement} {}
<LINE_COMMENT>{line_commentend} {BEGIN INITIAL;}




%%
/* user code section */

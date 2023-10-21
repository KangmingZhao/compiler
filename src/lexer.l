%option noyywrap
%option nounput
%option noinput
%top{
    #include <stdarg.h>
    #include "common.h"
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
DECIMIAL ([1-9][0-9]*|0)
HEXADECIMAL "0x"([1-9a-eA-E][0-9a-eA-E]*|0)
OCTAL "0b"([1-7][0-7]*|0)

FLOAT [+-]?([1-9][0-9]*|0)?"."([0-9]*[1-9]|0)([eE][+-]?([1-9][0-9]*|0))?

/*
    这里防止写多了人乱了记录一下。
    [+-]?可选开头正负号
    ([1-9][0-9]*|0)?"."([0-9]*[1-9]|0)是正常的部分。
    ([eE][+-]?([1-9][0-9]*|0))?是一个可选的幂，[eE]是计数法的分隔符，[+-]?可选的正负幂，([1-9][0-9]*|0)具体的幂次数。
*/


ID [[:alpha:]_][[:alpha:][:digit:]_]*
ARRAY [[:alpha:]_][[:alpha:][:digit:]_]*("["([1-9][0-9]*|0)"]")*

EOL (\r\n|\n|\r)
WHITE [\t ]
/*  Your code here (lab3). */

ADD_type "+"?"+"
MINUS_type "-"?"-"

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
    dump_tokens("INT\t%s\n", yytext);
}

"float" {
    dump_tokens("FLOAT\t%s\n", yytext);
}

"void" {
    dump_tokens("VOID\t%s\n", yytext);
}

"if" {
    dump_tokens("IF\t%s\n", yytext);
}

"else" {
    dump_tokens("ELSE\t%s\n", yytext);
}

"while" {
    dump_tokens("WHILE\t%s\n", yytext);
}

"for" {
    dump_tokens("FOR\t%s\n", yytext);
}

"return" {
    dump_tokens("RETURN\t%s\n", yytext);
}

"=" {
    dump_tokens("ASSIGN\t%s\n", yytext);
}


"<" {
    dump_tokens("LESS\t%s\n", yytext);
}

">" {
    dump_tokens("MORE\t%s\n", yytext);
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
    }
    else if(i == 2)
    {
        dump_tokens("INCREMENT\t%s\n", yytext);
    }
}

{MINUS_type} {
    int i = 0;
    while(yytext[i] != '\0')
    {
        i++;
    }
    if(i == 1)
    {
        dump_tokens("MINUS\t%s\n", yytext);
    }
    else if(i == 2)
    {
        dump_tokens("DECREMENT\t%s\n", yytext);
    }
}



"*" {
    dump_tokens("MUL\t%s\n", yytext);
}

"%" {
    dump_tokens("MOD\t%s\n", yytext);
}

"/" {
    dump_tokens("DIV\t%s\n", yytext);
}


"&&" {
    dump_tokens("AND\t%s\n", yytext);
}

"||" {
    dump_tokens("OR\t%s\n", yytext);
}

"!" {
    dump_tokens("NOT\t%s\n", yytext);
}

"==" {
    dump_tokens("EQUAL\t%s\n", yytext);
}

"!=" {
    dump_tokens("NOT_EQUAL\t%s\n", yytext);
}

"," {
    dump_tokens("COMMA\t%s\n", yytext);
}

";" {
    dump_tokens("SEMICOLON\t%s\n", yytext);
}

"(" {
    dump_tokens("LPAREN\t%s\n", yytext);
}

")" {
    dump_tokens("RPAREN\t%s\n", yytext);
}

"{" {
    dump_tokens("LBRACE\t%s\n", yytext);
}

"}" {
    dump_tokens("RBRACE\t%s\n", yytext);
}

{DECIMIAL} {
    int decimal;
    decimal = atoi(yytext);
    dump_tokens("DECIMAL\t%s\t%d\n", yytext, decimal);
}

{OCTAL} {
    int decmial = 0;
    int i = 2;//别把"0b"也算进去
    while(yytext[i] != '\0')
    {
        decmial *= 8;
        decmial += yytext[i] - '0';
        i++;
    }
    dump_tokens("OCTAL\t%s\t%d\n", yytext, decmial);
}

{HEXADECIMAL} {
    //dump_tokens("HEXADECIMAL\t%s\n", yytext);
    //int decimal;
    //decimal = std::stoi(yytext, 0, 16);
    //dump_tokens("HEXADECIMAL\t%s\t%d\n", yytext, decimal);
    int decmial = 0;
    int i = 2;//别把"0x"也算进去
    while(yytext[i] != '\0')
    {
        decmial *= 16;
        if(yytext[i] >= '0' && yytext[i] <= '9')
        {
            decmial += yytext[i] - '0';
        }
        else if(yytext[i] >= 'a' && yytext[i] <= 'z')
        {
            decmial += 10;
            decmial += yytext[i] - 'a';
        }
        else
        {
            decmial += 10;
            decmial += yytext[i] - 'A';
        }
        i++;
    }
    dump_tokens("HEXADECIMAL\t%s\t%d\n", yytext, decmial);
}


{FLOAT} {
    float f_num = 0;
    float int_part = 0;
    float float_part = 0;//鄙人英语不好，这个是小数部分。
    float div_reg = 10;
    float power = 1;
    int power_part = 0;
    bool pow_mul = 1;

    
    int now_part = 0; //进来先处理整数部分
    /*
    0:整数部分
    1：消暑部分
    2:幂部分
    */
    int i = 0;

    float minus = 1;

    while(yytext[i] != '\0')
    {
        if(i == 0 && (yytext[i] == '+' || yytext[i] == '-'))
        {
            if(yytext[i] == '-')
            {
                minus = -1;
            }
            else
            {
                minus = 1;
            }
            i++;
            continue;
        }
        if(yytext[i] >= '0' && yytext[i] <= '9')
        {
            switch(now_part)
            {
            case 0:
                int_part *= 10.0;
                int_part += (float)(yytext[i] - '0');
                
                break;
            case 1:
                float_part += (float)(yytext[i] - '0') / div_reg;
                div_reg *= 10.0;
                break;
            case 2:
                power_part *= 10;
                power_part += (yytext[i] - '0');
                break;
            }
        }
        else if(yytext[i] == '.')
        {
            now_part = 1;
        }
        else if(yytext[i] == 'e' || yytext[i] == 'E')
        {
            now_part = 2;
        }
        else if(yytext[i] == '+')
        {
            pow_mul = 1;
        }
        else if(yytext[i] == '-')
        {
            pow_mul = 0;
        }
        i++;
    }
    if(pow_mul)
        for(int i = 0; i < power_part; i++)
        {
            power *= 10.0;
        }
    else
        for(int i = 0; i < power_part; i++)
        {
            power /= 10.0;
        }
    f_num = int_part + float_part;
    f_num = f_num * minus * power;
    dump_tokens("FLOAT\t%s\t%f\n", yytext, f_num);
}


{ID} {
    dump_tokens("ID\t%s\n", yytext);
}

{ARRAY} {
    int i = 0;
    char* name = new char[100];
    int dimension_size[100];
    int index = 0;
    bool ID_over = 0;
    int temp = 0;
    while(yytext[i] != '\0')
    {
        if(yytext[i] == '[')
        {
            ID_over = 1;
        }
        if(!ID_over)
        {
            name[i] = yytext[i];
        }
        else
        {
            i++;
            while(yytext[i] >= '0' && yytext[i] <= '9')
            {
                temp *= 10;
                temp += yytext[i] - '0';
                i++;
            }
            dimension_size[index] = temp;
            index++;
            temp = 0;
        }
        i++;
    }
    dump_tokens("ARRAY_ID\t%s\n", name);
    for(int i = 0; i < index; i++)
    {
        dump_tokens("Dimension%d size\t%d\n", i,dimension_size[i]);
    }
}


{EOL} yylineno++;

{WHITE}



{block_commentbegin} {BEGIN BLOCK_COMMENT;}
<BLOCK_COMMENT>{block_commentelement} {}
<BLOCK_COMMENT>{block_commentend} {BEGIN INITIAL;}

{line_commentbegin} {BEGIN LINE_COMMENT;}
<LINE_COMMENT>{line_commentelement} {}
<LINE_COMMENT>{line_commentend} {BEGIN INITIAL;}


%%
/* user code section */

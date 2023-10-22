%option noyywrap
%option nounput
%option noinput
%top{
    #include <stdarg.h>
    #include "symbolTable.h"
    #include "common.h"
    using namespace std;

    int offset=0;//偏移量
    int scope=0;//作用域 呃没什么用 
    symbolTable* identifiers = new symbolTable();
    //这个就是编译时，整个项目的根符号表。也就是栈底

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
    /*  
    这里写一些注释。关于符号表，由于这里我们……emm，只是识别出token，并没有判断这个到底是不是一个block块，
    所以暂时只能简单的根据"{"和"}"符号判断进入/退出作用域。更精确的做法是等到lab4可以判断block之后，进入/退出block时才
    进行作用域的判断。
    */


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
    dump_tokens("INT\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset+=3;
}

"float" {
    dump_tokens("FLOAT\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset+=5;
}

"void" {
    dump_tokens("VOID\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset+=4;
}

"if" {
    dump_tokens("IF\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset+=2;
}

"else" {
    dump_tokens("ELSE\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset+=4;
}

"while" {
    dump_tokens("WHILE\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset+=5;
}

"for" {
    dump_tokens("FOR\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset+=3;
}

"return" {
    dump_tokens("RETURN\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset+=6;
}

"=" {
    dump_tokens("ASSIGN\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset++;
}


"<" {
    dump_tokens("LESS\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset++;
}

">" {
    dump_tokens("MORE\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset++;
}


{ADD_type} {
    int i = 0;
    while(yytext[i] != '\0')
    {
        i++;
    }
    if(i == 1)
    {
        dump_tokens("ADD\t%s\t%d\t%d\n", yytext,yylineno,offset);
        offset++;
    }
    else if(i == 2)
    {
        dump_tokens("INCREMENT\t%s\t%d\t%d\n", yytext,yylineno,offset);
        offset+=2;
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
        dump_tokens("MINUS\t%s\t%d\t%d\n", yytext,yylineno,offset);
        offset++;
    }
    else if(i == 2)
    {
        dump_tokens("DECREMENT\t%s\t%d\t%d\n", yytext,yylineno,offset);
        offset+=2;
    }
}



"*" {
    dump_tokens("MUL\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset++;
}

"%" {
    dump_tokens("MOD\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset++;
}

"/" {
    dump_tokens("DIV\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset++;
}


"&&" {
    dump_tokens("AND\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset+=2;
}

"||" {
    dump_tokens("OR\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset+=2;
}

"!" {
    dump_tokens("NOT\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset++;
}

"==" {
    dump_tokens("EQUAL\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset+=2;
}

"!=" {
    dump_tokens("NOT_EQUAL\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset+=2;
}

"," {
    dump_tokens("COMMA\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset++;
}

";" {
    dump_tokens("SEMICOLON\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset++;
}

"(" {
    dump_tokens("LPAREN\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset++;
}

")" {
    dump_tokens("RPAREN\t%s\t%d\t%d\n", yytext,yylineno,offset);
    offset++;
}

"{" {
    //遇到一个"{"时表示进入一个新的作用域
    identifiers = new symbolTable(identifiers);
    //scope++;// 作用域+1
    dump_tokens("LBRACE\t%s\n", yytext);
    offset++;
}

"}" {
    //遇到"}"时表示退出作用域
    symbolTable *now_identifiers = identifiers;
    //出栈
    identifiers = identifiers->get_prev();
    delete now_identifiers; //删除原栈顶
    //scope--;
    dump_tokens("RBRACE\t%s\n", yytext);
    offset++;
}

{DECIMIAL} {
    int decimal;
    decimal = atoi(yytext);
    dump_tokens("DECIMAL\t%s\t%d\t%d\t%d\n", yytext,yylineno,offset,decimal);
    offset+=strlen(yytext);
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
    dump_tokens("OCTAL\t%s\t%d \t%d\t%d\n", yytext,yylineno,offset, decmial);
    offset+=strlen(yytext);
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
    dump_tokens("HEXADECIMAL\t%s\t%d \t%d\t%d\n", yytext,yylineno,offset, decmial);
    offset+=strlen(yytext);
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
    dump_tokens("FLOAT\t%s\t%d\t%d\t%f\n", yytext,yylineno,offset, f_num);
    offset+=strlen(yytext);
}


{ID} {
    //遇到id我们先找找看符号表里有没有
    //printf("%s\n",yytext);
    symbolTableEntry* node = identifiers->find(yytext);

    if(node!=nullptr)
    {
 
    }
    else
    {
        symbolTableEntry* temp = new symbolTableEntry(yylineno,offset,INT,0,nullptr);
        identifiers->insert(yytext, temp);

    }
    dump_tokens("ID\t%s\t%d\t%d\tox%08x\n", yytext,yylineno,offset,&identifiers);
    offset+=strlen(yytext);
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

    //遇到id我们先找找看符号表里有没有
   
    symbolTableEntry* node = identifiers->find(name);
    //int value = 0;
    if(node)
    {
       // value = node->value;
        //dump_tokens("ARRAY_ID\t%s\t%d\t%d\t%d\n", name,yylineno,offset,value);
        dump_tokens("ARRAY_ID\t%s\t%d\t%d\tox%08x\n", name,yylineno,offset,&identifiers);
        for(int i = 0; i < index; i++)
        {
            dump_tokens("accessing_index\t%d\n", i,dimension_size[i]);
        }
    }
    else
    {
        symbolTableEntry* temp = new symbolTableEntry(yylineno,offset,INT,0,nullptr);
        identifiers->insert(yytext, temp);
        //value = temp->value;
        dump_tokens("ARRAY_ID\t%s\t%d\t%d\tox%08x\n",name,yylineno,offset,&identifiers);
        for(int i = 0; i < index; i++)
        {
            dump_tokens("Dimension%d size\t%d\n", i,dimension_size[i]);
        }
    }
    offset+=strlen(yytext);
}


{EOL} yylineno++;offset=0;

{WHITE} offset++;



{block_commentbegin} {BEGIN BLOCK_COMMENT;}
<BLOCK_COMMENT>{block_commentelement} {
    if(*yytext=='\n')
    {
        yylineno++;
        offset++;
    }
    else{
        offset=0;
    }

}
<BLOCK_COMMENT>{block_commentend} {BEGIN INITIAL;}

{line_commentbegin} {BEGIN LINE_COMMENT;}
<LINE_COMMENT>{line_commentelement} {}
<LINE_COMMENT>{line_commentend} {BEGIN INITIAL;}


%%
/* user code section */

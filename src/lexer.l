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

/* 閿熸枻鎷烽敓鏂ゆ嫹閿熸枻鎷烽敓鏂ゆ�? */

/* 璇嗗埆娴偣鏁扮殑鏃跺€欏彂鐜�?簡涓弗閲嶇殑闂锛屽鏋滆繛鐫�??ｈ�?鍙蜂竴璧�?��鍒负涓�?��猼oken鐨勮瘽浼�?��娉曞鐞嗗�?鍑忔硶銆傛�?浠ヨ繕鏄緱鏁板瓧鍜岀鍙峰垎�?��璇嗗�? */
DECIMIAL ([1-9][0-9]*|0)
HEXADECIMAL "0x"([1-9a-eA-E][0-9a-eA-E]*|0)
OCTAL "0b"([1-7][0-7]*|0)
FLOAT ([1-9][0-9]*|0)?"."([0-9]*[1-9]|0)([eE][+-]?([1-9][0-9]*|0))?

ID [[:alpha:]_][[:alpha:][:digit:]_]*


EOL (\r\n|\n|\r)
WHITE [\t ]



/*  Your code here (lab3). */


/* 閿熸枻鎷烽敓鏂ゆ嫹閿熸枻鎷烽敓鏂ゆ�?閿熸枻鎷烽敓鐨嗙》锟�??   */
ADD_type "+"?"+"
SUB_type "-"?"-"


/* 閿熸枻鎷烽敓鏂ゆ嫹閿熸枻鎷烽敓鏂ゆ�?娉ㄩ敓閰靛拰鍖℃嫹娉ㄩ敓鏂ゆ�? */

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
    dump_tokens("INT\t%s\n", yytext);//\t閿熸枻鎷穞ab閿熸枻锟�??
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
"const" {
    dump_tokens("CONST\t%s\n", yytext);
    return CONST;
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
    dump_tokens("NOT\t%s \n", yytext);
    return NOT;
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


{OCTAL} {
    int decmial = 0;
    int i = 2;//鍒�?"0b"涔熺畻杩涘幓
    while(yytext[i] != '\0')
    {
        decmial *= 8;
        decmial += yytext[i] - '0';
        i++;
    }
    dump_tokens("DECIMAL\t%s\t%d\n", yytext, decmial);
    yylval.itype = decmial;
    return INTEGER;
}

{HEXADECIMAL} {
    //dump_tokens("HEXADECIMAL\t%s\n", yytext);
    //int decimal;
    //decimal = std::stoi(yytext, 0, 16);
    //dump_tokens("HEXADECIMAL\t%s\t%d\n", yytext, decimal);
    int decmial = 0;
    int i = 2;//鍒�?"0x"涔熺畻杩涘幓
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
    dump_tokens("DECIMAL\t%s\t%d\n", yytext, decmial);
    yylval.itype = decmial;
    return INTEGER;
}

{FLOAT} {
    float f_num = 0;
    float int_part = 0;
    float float_part = 0;//�?��汉鑻�?��涓嶅ソ锛岃繖涓槸灏忔暟�?��垎銆�?
    float div_reg = 10;
    float power = 1;
    int power_part = 0;
    bool pow_mul = 1;

    
    int now_part = 0; //杩涙潵鍏堝鐞嗘暣鏁伴儴鍒�
    /*
    0:鏁存暟閮ㄥ垎
    1锛氭秷鏆戦儴鍒�
    2:骞傞儴鍒�?
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
    yylval.fltype = f_num;
    return FLOATPOINT;
}

"getint" {
        char *lexeme;
        lexeme=new char[strlen(yytext) + 1];
        strcpy(lexeme, yytext);
        yylval.strtype = lexeme;
        Type* funcType = new FunctionType(TypeSystem::intType, {});//杩斿洖绫诲瀷int锛屾棤鍙傛暟
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//鎻掑叆褰撳�?绗﹀彿琛ㄤ腑 浣滅敤鍩熶负鍏ㄥ�?
        globals->install(yytext, entry);
        return ID;
    
}
"getch" {

        char *lexeme = new char[strlen(yytext) + 1];
        strcpy(lexeme, yytext);
        yylval.strtype = lexeme;
        Type* funcType = new FunctionType(TypeSystem::intType, {});//杩斿洖绫诲瀷int锛屾棤鍙傛暟
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//浣滅敤鍩�?OBAL 浣滅敤鍩熶负鍏ㄥ�?
        globals->install(yytext, entry);
        return ID;
    
}

"putint" {

        char *lexeme = new char[strlen(yytext) + 1];
        strcpy(lexeme, yytext);
        yylval.strtype = lexeme;
	std::vector<Type*> vec; //褰㈠�?��诲瀷琛�?
        vec.push_back(TypeSystem::intType);
        FunctionType *funcType = new FunctionType(TypeSystem::voidType, vec);//杩斿洖绫诲瀷void锛屽�?��拌〃涓篿nt
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//浣滅敤鍩�?OBAL 浣滅敤鍩熶负鍏ㄥ�?
        globals->install(yytext, entry);
        return ID;
   
}
"putch" {

        char *lexeme = new char[strlen(yytext) + 1];
        strcpy(lexeme, yytext);
        yylval.strtype = lexeme;
	std::vector<Type*> vec; //褰㈠�?��诲瀷琛�?
        vec.push_back(TypeSystem::intType);
        Type* funcType = new FunctionType(TypeSystem::voidType, vec);//杩斿洖绫诲瀷void锛屽�?��拌〃涓篿nt 
        SymbolEntry* entry = new IdentifierSymbolEntry(funcType, yytext, 0);//浣滅敤鍩�?OBAL 浣滅敤鍩熶负鍏ㄥ�?
        globals->install(yytext, entry);
        return ID;
    
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

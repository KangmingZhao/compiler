#include <iostream>
#include <string.h>
#include <unistd.h>
#include "common.h"
#include "Ast.h"
#include "Unit.h"
#include "MachineCode.h"
#include "LinearScan.h"

extern FILE *yyin;
extern FILE *yyout;
int yyparse();

Ast ast;
Unit unit;
MachineUnit mUnit;
char outfile[256] = "a.out";
dump_type_t dump_type = ASM;

int main(int argc, char *argv[])
{
    int opt;
    while ((opt = getopt(argc, argv, "Siato:")) != -1)
    {
        switch (opt)
        {
        case 'o':
            strcpy(outfile, optarg);
            break;
        case 'a':
            dump_type = AST;
            break;
        case 't':
            dump_type = TOKENS;
            break;
        case 'i':
            dump_type = IR;
            break;
        case 'S':
            dump_type = ASM;
            break;
        default:
            fprintf(stderr, "Usage: %s [-o outfile] infile\n", argv[0]);
            exit(EXIT_FAILURE);
            break;
        }
    }
    if (optind >= argc)
    {
        fprintf(stderr, "no input file\n");
        exit(EXIT_FAILURE);
    }
    if (!(yyin = fopen(argv[optind], "r")))
    {
        fprintf(stderr, "%s: No such file or directory\nno input file\n", argv[optind]);
        exit(EXIT_FAILURE);
    }
    if (!(yyout = fopen(outfile, "w")))
    {
        fprintf(stderr, "%s: fail to open output file\n", outfile);
        exit(EXIT_FAILURE);
    }
       // getint 
    std::vector<Type*> vec;
    SymbolTable* st = identifiers;
    while(st->getPrev())
        st = st->getPrev();
    Type* funcType = new FunctionType(TypeSystem::intType, vec);
    SymbolEntry* se = new IdentifierSymbolEntry(funcType, "getint",0);
    st->install("getint", se);

    // getch
    funcType = new FunctionType(TypeSystem::intType, vec);
    st = identifiers;
    while(st->getPrev())
        st = st->getPrev();
    se = new IdentifierSymbolEntry(funcType,"getch", 0);
    st->install("getch", se);

    // putint
    vec.clear();
    vec.push_back(TypeSystem::intType);
    funcType = new FunctionType(TypeSystem::voidType, vec);
    st = identifiers;
    while(st->getPrev())
         st = st->getPrev();
    se = new IdentifierSymbolEntry(funcType, "putint", 0);
    st->install("putint", se);

    // putch
    vec.clear();
    vec.push_back(TypeSystem::intType);
    funcType = new FunctionType(TypeSystem::voidType, vec);
    st = identifiers;
    while(st->getPrev())
        st = st->getPrev();
    se = new IdentifierSymbolEntry(funcType, "putch", 0);
    st->install("putch", se);

    // putf
    vec.clear();
    funcType = new FunctionType(TypeSystem::voidType, vec);
    st = identifiers;
    while(st->getPrev())
        st = st->getPrev();
    se = new IdentifierSymbolEntry(funcType, "putf", 0);
    st->install("putf", se);




    yyparse();
    if (dump_type == AST)
    {
        ast.output();
    }



    //ast.output();

    ast.typeCheck();    
    //std::cout << "fuck\n";


    ast.mergeConstExp();


    //ast.output();
    ast.genCode(&unit);



    if(dump_type == IR)
        unit.output();

    unit.genMachineCode(&mUnit);


    LinearScan linearScan(&mUnit);
    linearScan.allocateRegisters();
    if(dump_type == ASM)
        mUnit.output();
    return 0;
}

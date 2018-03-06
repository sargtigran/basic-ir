
#include <fstream>
#include <iostream>
#include <sstream>

#include "aslisp.hxx"
#include "parser.hxx"
#include "scanner.hxx"
#include "typechecker.hxx"

bool fileExists(const std::string& filename)
{
    std::ifstream infi(filename);
    return infi.good();
}

//
int main( int argc, char* argv[] )
{
    std::cout << "Parsing ..." << std::endl;

    if( argc < 2 ) {
        std::cout << "" << std::endl;
        return 0;
    }

    if( !fileExists(argv[1]) ) {
        std::cout << argv[1] << " ֆայլը գոյություն չունի։";
        return 0;
    }

    basic::Parser parser(argv[1]);
    basic::Program* prog = parser.parse();

    if( nullptr != prog ) {
        bool errok = basic::TypeChecker().check(prog);

        if( errok ) {
            std::ofstream sout(std::string(argv[1]) + ".lisp");
            basic::Lisper(sout).asLisp(prog);
            sout.close();
        }
    }

    return 0;
}


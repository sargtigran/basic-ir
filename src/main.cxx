
#include <fstream>
#include <iostream>
#include <sstream>

#include <llvm/Support/FileSystem.h>

#include "aslisp.hxx"
#include "parser.hxx"
#include "scanner.hxx"
#include "typechecker.hxx"
#include "iremitter.hxx"
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

    basic::Parser parser("../cases/case10.bas");
    auto prog = parser.parse();

    /*
    if( argc < 2 ) {
        std::cout << "Մուտքային ֆայլը տրված չէ։" << std::endl;
        return 0;
    }

    if( !fileExists(argv[1]) ) {
        std::cout << argv[1] << " ֆայլը գոյություն չունի։";
        return 0;
    }

    std::cout << "Parsing ..." << std::endl;
    basic::Parser parser(argv[1]);
    auto prog = parser.parse();
    */

    if( nullptr != prog ) {
        std::cout << "Type checking ..." << std::endl;
        bool errok = basic::TypeChecker().check(prog);

        if( errok ) {
            std::cout << "Lisp ..." << std::endl;
            //std::ofstream sout(std::string(argv[1]) + ".lisp");
            //basic::Lisper(std::cout).asLisp(prog);
            basic::IrEmitter::emitIrCode(prog);
            //sout.close();

            std::error_code ec;
            llvm::raw_fd_ostream ef("emitted.ll", ec, llvm::sys::fs::F_RW);
            std::cout << "Compiling ..." << std::endl;
            basic::IrEmitter(ef).emitIrCode(prog);
        }
    }

    return 0;
}

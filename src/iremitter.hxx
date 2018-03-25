
#include "ast.hxx"

#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/raw_ostream.h>

#include <iostream>
#include <sstream>
#include <unordered_map>
#include <string>
#include <memory>

namespace basic {

///
class IrEmitter {
public:
    using String = std::string;
    using IrType = llvm::Type;
    using TypeVector = std::vector<IrType*>;

public:
    IrEmitter();
    ~IrEmitter() = default;

    bool emitIrCode( ProgramPtr prog );

private:
    void emitProgram( ProgramPtr prog );
    void emitSubroutine( SubroutinePtr subr );

    void emitStatement( StatementPtr st );
    void emitSequence( SequencePtr seq );
    void emitLet( LetPtr let );
    void emitInput( InputPtr inp );
    void emitPrint( PrintPtr pri );
    void emitIf( IfPtr sif );
    void emitFor( ForPtr sfor );
    void emitWhile( WhilePtr swhi );
    void emitCall( CallPtr cal );

    llvm::Value* emitExpression( ExpressionPtr expr );
    llvm::Value* emitApply( ApplyPtr apy );
    llvm::Value* emitBinary( BinaryPtr bin );
    llvm::Value* emitUnary( UnaryPtr una );
    llvm::Value* emitText( TextPtr txt );
    llvm::Constant* emitNumber( NumberPtr num );
    llvm::LoadInst* emitLoad( VariablePtr var );

    //! @brief BASIC-IR տիպից կառուցում է LLVM տիպ։
    llvm::Type* llvmType( Type type );

    //! @brief Ճշտում է հերթական BasicBlock-ի դիրքը։
    void placeBlock( llvm::Function* fun, llvm::BasicBlock* bl );

    void prepareLibrary();
    llvm::Constant* LF( const String& name );
    llvm::Constant* UF( const String& name );

    void declareSubroutines( ProgramPtr prog );
    void defineSubroutines( ProgramPtr prog );
    bool createsTempText( ExpressionPtr expr );

private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;

    std::unique_ptr<llvm::Module> module = nullptr;

    ////llvm::raw_fd_ostream& outstream;

    //! @brief Գրադարանային ֆունկցիաների ցուցակն է։
    //!
    //! Բանալին ֆունկցիայի անունն է, իսկ արժեքը ֆունկցիայի տիպն է՝
    //! որպես @c FunctionType ցուցիչ։ Այս ցուցակում պետք է գրանցվեն,
    //! բոլոր այն ֆունկցիաները, որոնք կոդի գեներատորն օգտագործելու է։
    std::unordered_map<String,llvm::FunctionType*> library;

    //! @brief Տեքստային հաստատունների ցուցակն է։
    //!
    //! Երբ պետք է գեներացվի հղում տեքստային հաստատունին, @c emitText
    //! մեթոդը նախ այդ հաստատունի հասցեն փնտրում է այս ցուցակում։ 
    //! Եթե տվյալ հաստատունն արդեն սահմանված է, ապա օգտագործվում
    //! է դրա հասցեն, հակառակ դեպքում՝ սահմանվում է նորը։
    std::unordered_map<String,llvm::Value*> globaltexts;

    //! @brief Ֆունկցիայի լոկալ անունների ցուցակն է։
    //!
    //! Սա օգտագործվում է բոլոր այն դեպքերում, երբ պետք է իդենտիֆիկատորը
    //! կապել @c Value ցուցիչի հետ։ Քանի որ վերլուծության վերջում արդեն 
    //! հայտնի են ենթածրագրում օգտագործված բոլոր անունները և դրանք 
    //! գրանցված են @c Subroutine օբյեկտի @c locals ցուցակում, ապա կոդի
    //! գեներացիայի ժամանակ հենց ամենասկզբում ստեղծվում է այս ցուցակը,
    //! իսկ այն դեպքերում, երբ պետք է հղվել անունին, համապատասխան
    //! ցուցիչն ընտրվում է այստեղից։
    std::unordered_map<String,llvm::Value*> varaddresses;
};
} // namespace basic

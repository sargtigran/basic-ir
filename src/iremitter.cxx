
#include "iremitter.hxx"
#include "ast.hxx"

#include <llvm/IR/GlobalValue.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Verifier.h>

#include <iostream>
#include <sstream>

/*
// աջակցող գրադարանը բեռնելու համար

std::unique_ptr<Module> llvm::parseAssemblyString(
    StringRef AsmString,
    SMDiagnostic& Error,
    LLVMContext& Context,
    SlotMapping* Slots = nullptr,
    bool UpgradeDebugInfo = true,
    StringRef DataLayoutString = "");

սtd::unique_ptr<Module> llvm::parseAssemblyFile(
    StringRef Filename,
    SMDiagnostic& Error,
    LLVMContext& Context,
    SlotMapping* Slots = nullptr,
    bool UpgradeDebugInfo = true,
    StringRef DataLayoutString = "");

// llvm::Linker դասի օգնությամբ կապակցել գրադարանի ու ծրագրի մոդուլները
*/


namespace basic {
///
bool IrEmitter::emitIrCode( Program* prog )
{
    emitProgram(prog);
}

///
void IrEmitter::emitProgram( Program* prog )
{
    module = new llvm::Module(prog->filename, context);

    for( Subroutine* si : prog->members )
        emitSubroutine(si);

    module->print(llvm::errs(), nullptr);
    //module->print(mOut, nullptr);
}

//
void IrEmitter::emitSubroutine( Subroutine* subr )
{
    // պարամետրերի տիպերի ցուցակի կառուցումը
    std::vector<llvm::Type*> ptypes;
    for( auto& pr : subr->parameters )
        ptypes.push_back(llvmType(typeOf(pr)));

    // վերադարձվող արժեքի տիպը
    llvm::Type* rtype = nullptr;
    if( subr->hasValue )
        rtype = llvmType(typeOf(subr->name));
    else
        rtype = builder.getVoidTy();

    // ֆունկցիայի տիպը
    auto procty = llvm::FunctionType::get(rtype, ptypes, false);
    // ֆունկցիա օբյեկտը
    auto fun = llvm::Function::Create(procty, llvm::GlobalValue::ExternalLinkage, subr->name, module);

    // եթե սա ներդրված ենթածրագիր է, ապա գեներացնում ենք միայն հայտարարությունը
    if( subr->isBuiltIn )
        return;

    // ֆունկցիայի առաջին պիտակը (ցույց է տալիս ֆունկցիայի սկիզբը)
    auto start = llvm::BasicBlock::Create(context, "start", fun);
    builder.SetInsertPoint(start);

    // ֆունկցիայի պարամետրերին տալ անուններ
    for( auto& arg : fun->args() ) {
        int ix = arg.getArgNo();
        arg.setName(subr->parameters[ix]);
    }

    // օբյեկտներ բոլոր լոկալ փոփոխականների, պարամետրերի 
    // և վերադարձվող արժեքի համար
    for( Variable* vi : subr->locals ) {
        auto vty = llvmType(vi->type);
        auto addr = builder.CreateAlloca(vty, nullptr, vi->name + "_addr");
        varaddresses[vi->name] = addr;
    }

    // պարամետրերի արժեքները վերագրել լոկալ օբյեկտներին
    for( auto& arg : fun->args() )
        builder.CreateStore(&arg, varaddresses[arg.getName()]);

    // գեներացնել ֆունկցիայի մարմինը
    emitSequence(dynamic_cast<Sequence*>(subr->body));

    // լրացնել, ուղղել
    if( rtype->isVoidTy() )
        builder.CreateRetVoid();
    else {
        auto rv = builder.CreateLoad(varaddresses[subr->name]);
        builder.CreateRet(rv);
    }
    llvm::verifyFunction(*fun);
}

void IrEmitter::emitStatement(Statement* st)
{
    switch (st->kind) {
        case NodeKind::Apply:
            break;
        case NodeKind::Sequence:
            emitSequence(static_cast<Sequence*>(st));
            break;
        case NodeKind::Input:
            emitInput(dynamic_cast<Input*>(st));
            break;
        case NodeKind::Print:
            emitPrint(dynamic_cast<Print*>(st));
            break;
        case NodeKind::Let:
            emitLet(static_cast<Let*>(st));
            break;
        case NodeKind::If:
            emitIf(static_cast<If*>(st));
            break;
        case NodeKind::While:
            //emitWhile(static_cast<While*>(stat), endBB);
            break;
        case NodeKind::For:
            emitFor(static_cast<For*>(st));
            break;
        case NodeKind::Call:
            break;
        default:
            break;
     }
 }
 
 ///
void IrEmitter::emitSequence(Sequence* seq)
{
    for (auto st : seq->items) {
        emitStatement(st);
    }
}

/////
//void IrEmitter::emitSequence( Sequence* seq )
//{
//    for( Statement* st : seq->items ) {
//        switch( st->kind ) {
//            case NodeKind::Let:
//                emitLet(dynamic_cast<Let*>(st));
//                break;
//            case NodeKind::Input:
//                emitInput(dynamic_cast<Input*>(st));
//                break;
//            case NodeKind::Print:
//                emitPrint(dynamic_cast<Print*>(st));
//                break;
//            case NodeKind::If:
//                emitIf(dynamic_cast<If*>(st));
//                break;
//            case NodeKind::While:
//                break;
//            case NodeKind::For:
//                break;
//            case NodeKind::Call:
//                break;
//            default:
//                break;
//        }
//    }
//}

///
void IrEmitter::emitLet( Let* let )
{
    // TODO: տողերի դեպքում՝ ուրիշ մոտեցում
    auto val = emitExpression(let->expr);
    auto addr = varaddresses[let->varptr->name];
    builder.CreateStore(val, addr);
}

//
void IrEmitter::emitInput( Input* inp )
{
    // կանչել գրադարանային ֆունկցիա
    // input_text() կամ input_number()
}

///
void IrEmitter::emitPrint( Print* pri )
{
    // կանչել գրադարանային ֆունկցիա
    // print_text() կամ print_number()
}

//TODO լրացնել ուղղել
void IrEmitter::emitIf(If* sif)
//void IrEmitter::emitIf(If* sif, llvm::BasicBlock* endBB)
{
    // ընթացիկ ֆունկցիայի դուրս բերում
    auto insertBB = builder.GetInsertBlock();
    auto fun = insertBB->getParent();

    // գեներացնել պայմանի օպերտորը 
    auto cnd = emitExpression(sif->condition);

    // 
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context, "then.bb", fun);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context, "merge.bb", fun);

    // then
    builder.SetInsertPoint(thenBB);
    emitStatement(sif->decision);

    // else
    llvm::BasicBlock* elseBB = mergeBB;
    if (sif->alternative) {
        elseBB = llvm::BasicBlock::Create(context, "else.bb", fun, mergeBB);
        builder.SetInsertPoint(elseBB);
        emitStatement(sif->alternative);
    }


    builder.SetInsertPoint(insertBB);
    auto br = builder.CreateCondBr(cnd, thenBB, elseBB);

    if (!thenBB->getTerminator()) {
        builder.SetInsertPoint(thenBB);
        builder.CreateBr(mergeBB);
    }

    if (!elseBB->getTerminator()) {
        builder.SetInsertPoint(elseBB);
        builder.CreateBr(mergeBB);
    }

    builder.SetInsertPoint(mergeBB);
    //mEmittedNodes.insert({ sif, br });
}

/*
void IrEmitter::emitWhile(While* whileSt, llvm::BasicBlock* endBB)
{
    llvm::BasicBlock* head = llvm::BasicBlock::Create(context, "bb", endBB->getParent(), endBB);
    llvm::BasicBlock* body = llvm::BasicBlock::Create(context, "bb", endBB->getParent(), endBB);

    builder.CreateBr(head);

    builder.SetInsertPoint(head);
    auto cnd = emitExpression(whileSt->condition);
    auto br = builder.CreateCondBr(cnd, body, endBB);

    builder.SetInsertPoint(body);
    emitStatement(whileSt->body, endBB);

    if (!body->getTerminator()) {
        builder.SetInsertPoint(body);
        builder.CreateBr(head);
    }
    builder.SetInsertPoint(endBB);
}
*/

//
void IrEmitter::emitFor( For* sfor )
{
    // TODO:
    // 1. գեներացնել սկզբնական արժեքի արտահայտությունը,
    // 2. գեներացնել վերջնական արժեքի արտահայտությունը,
    // 3. պարամետրին վերագրել սկզբնական արժեքը,
    // 4. եթե պարամետրի արժեքը >= (կամ <=, եթե քայլը բացասական է) վերջնականից,
    // 5. ապա դուրս գալ ցիկլից,
    // 6. գեներացնել մարմինը,
    // 7. պարամետրի արժեքին գումարել քայլի արժեքը,
    // 8. շարունակել 4-րդ կետից։

    /*
    llvm::BasicBlock* head = llvm::BasicBlock::Create(context, "bb", endBB->getParent(), endBB);
    llvm::BasicBlock* body = llvm::BasicBlock::Create(context, "bb", endBB->getParent(), endBB);
    llvm::BasicBlock* exit = llvm::BasicBlock::Create(context, "bb", endBB->getParent(), endBB);

    auto param_addr = getVariableAddress(forSt->parameter->name);
    auto begin = emitExpression(forSt->begin);
    builder.CreateStore(begin, param_addr);

    //Setting step 1 by default
    llvm::Value* step = builder.getInt32(1);

    //Looking if step is given
    if (forSt->step) {
        step = emitExpression(forSt->step);
    }

    auto end = emitExpression(forSt->end);
    builder.CreateBr(head);

    builder.SetInsertPoint(head);
    auto param = builder.CreateLoad(param_addr);
    auto cnd = builder.CreateFCmpOLE(param, end, "le");
    builder.CreateCondBr(cnd, body, endBB);

    //Handling the body
    builder.SetInsertPoint(body);
    emitStatement(forSt->body, exit);

    //Incrementing the index
    auto inc_param = builder.CreateFAdd(param, step);
    builder.CreateStore(inc_param, param_addr);

    if (!body->getTerminator()) {
        builder.SetInsertPoint(body);
        builder.CreateBr(head);
    }

    builder.SetInsertPoint(endBB);
    */
}


///
llvm::Value* IrEmitter::emitExpression( Expression* expr )
{
    llvm::Value* res = nullptr;

    switch( expr->kind ) {
        case NodeKind::Number:
            res = emitNumber(dynamic_cast<Number*>(expr));
            break;
        case NodeKind::Text:
            res = emitText(dynamic_cast<Text*>(expr));
            break;
        case NodeKind::Variable:
            res = emitLoad(dynamic_cast<Variable*>(expr));
            break;
        case NodeKind::Unary:
            break;
        case NodeKind::Binary:
            break;
        case NodeKind::Apply:
            break;
        default:
            break;
    }

    return res;
}

//
llvm::Value* IrEmitter::emitText( Text* txt )
{
    // եթե տրված արժեքով տող արդեն սահմանված է գլոբալ
    // տիրույթում, ապա վերադարձնել դրա հասցեն
    auto sri = globaltexts.find(txt->value);
    if( sri != globaltexts.end() )
        return sri->second;

    // ... հակառակ դեպքում՝ սահմանել նոր գլոբալ տող, դրա հասցեն
    // պահել գլոբալ տողերի ցուցակում և վերադարձնել որպես արժեք
    auto strp = builder.CreateGlobalStringPtr(txt->value, "g_str");
    globaltexts[txt->value] = strp;

    return strp;
}

//
llvm::Constant* IrEmitter::emitNumber( Number* num )
{
    return llvm::ConstantFP::get(builder.getDoubleTy(), num->value);
}

///
llvm::LoadInst* IrEmitter::emitLoad( Variable* var )
{
    llvm::Value* vaddr = varaddresses[var->name];
    return builder.CreateLoad(vaddr, var->name);
}

/*
llvm::Value* IrEmitter::emitBinary(Binary* bin)
{
    if (auto r = getEmittedNode(bin)) {
        return r;
    }
    llvm::Value* lhs = emitExpression(bin->subexpro);
    assert(lhs);
    llvm::Value* rhs = emitExpression(bin->subexpri);
    assert(rhs);
    llvm::Value* ret = nullptr;
    switch (bin->opcode) {
        case Operation::None:
            break;
        case Operation::Add:
            ret = builder.CreateFAdd(lhs, rhs, "add");
            break;
        case Operation::Sub:
            ret = builder.CreateFSub(lhs, rhs, "sub");
            break;
        case Operation::Mul:
            ret = builder.CreateFMul(lhs, rhs, "mul");
            break;
        case Operation::Div:
            ret = builder.CreateFDiv(lhs, rhs, "div");
            break;
        case Operation::Mod:
            ret = builder.CreateFRem(lhs, rhs, "rem");
            break;
        case Operation::Pow:
            assert("POW operator is not handled yet");
            break;
        case Operation::Eq:
            ret = builder.CreateFCmpOEQ(lhs, rhs, "eq");
            break;
        case Operation::Ne:
            ret = builder.CreateFCmpONE(lhs, rhs, "ne");
            break;
        case Operation::Gt:
            ret = builder.CreateFCmpOGT(lhs, rhs, "ne");
            break;
        case Operation::Ge:
            ret = builder.CreateFCmpOGE(lhs, rhs, "ge");
            break;
        case Operation::Lt:
            ret = builder.CreateFCmpOLT(lhs, rhs, "lt");
            break;
        case Operation::Le:
            ret = builder.CreateFCmpOLE(lhs, rhs, "le");
            break;
        case Operation::And:
            ret = builder.CreateAnd(lhs, rhs, "and");
            break;
        case Operation::Or:
            ret = builder.CreateOr(lhs, rhs, "or");
            break;
        case Operation::Conc:
            // TODO: [18:02:36] Armen Badalian: դեռ չեմ պատկերացնում, թե տողերի կոնկատենացիայի համար ինչ կոդ ես գեներացնելու
            //[18:03:16] Tigran Sargsyan: ես էլ չեմ պատկերացնում
            //[18:03:21] Tigran Sargsyan: :)
            //[18:03:33] Tigran Sargsyan: բայց դե միբան կբստրենք
            //[18:03:44] Armen Badalian: միգուցե տողերը սարքենք հին Պասկալի պես, երկարությունը ֆիքսենք 255 նիշ, ու բոլոր գործողությունները դրանով անենք
            //[18:04:16 | Edited 18:04:20] Armen Badalian: հին Պասկալում տողի առաջին բայթում գրվում էր տողի երկարությունը
            //[18:04:30] Armen Badalian: ու դա կարող էր լինել 255
            //[18:05:14] Tigran Sargsyan: տարբերակ ա, կարելի ա մտածել
            assert("CONC operator is not handled yet");
            break;
        default: {
            assert("Undefined binary operator");
            break;
        }
    }
    mEmittedNodes.insert({ bin, ret });
    return ret;
}

llvm::Value* IrEmitter::emitUnary(Unary* un)
{
    llvm::Value* val = emitExpression(un->subexpr);
    switch (un->opcode) {
        case Operation::Sub:
            return builder.CreateFNeg(val, "neg");
        case Operation::Not:
            return builder.CreateNot(val, "not");
        default: {
            assert("Invalid unary operation");
        }
    }
    return nullptr;
}
*/

llvm::Type* IrEmitter::llvmType( Type type )
{
    if( Type::Number == type )
        return builder.getDoubleTy();

    if( Type::Text == type )
        return builder.getInt8PtrTy();

    return builder.getVoidTy();
}
} // namespace llvm

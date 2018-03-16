
#include "iremitter.hxx"
#include "ast.hxx"

#include <llvm/IR/GlobalValue.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/FileSystem.h>

#include <iostream>
#include <sstream>
#include <list>

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
bool IrEmitter::emitIrCode( ProgramPtr prog )
{
    std::error_code ec;
    llvm::raw_fd_ostream ef("emitted.ll", ec, llvm::sys::fs::F_RW);
    IrEmitter emitter(ef);
    emitter.emitProgram(prog);
    return true;
}

///
void IrEmitter::emitProgram( ProgramPtr prog )
{
    module = std::make_unique<llvm::Module>(prog->filename, context);

    declareLibrary();

    for( auto& si : prog->members )
        emitSubroutine(si);

	// TODO: աշխատեցնել verify pass

    module->print(llvm::errs(), nullptr);
    //module->print(mOut, nullptr);
}

//
void IrEmitter::emitSubroutine( SubroutinePtr subr )
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
    auto fun = llvm::Function::Create(procty,
        llvm::GlobalValue::ExternalLinkage,
        subr->name, module.get());

    // եթե սա ներդրված ենթածրագիր է, ապա գեներացնում
    // ենք միայն հայտարարությունը
    if( subr->isBuiltIn )
        return;

    // ֆունկցիայի առաջին պիտակը (ցույց է տալիս ֆունկցիայի սկիզբը)
    auto start = llvm::BasicBlock::Create(context, "start", fun);
    builder.SetInsertPoint(start);

    // ֆունկցիայի պարամետրերին տալ սահմանված անունները
    for( auto& arg : fun->args() ) {
        int ix = arg.getArgNo();
        arg.setName(subr->parameters[ix]);
    }

    // մաքրել varaddresses ցուցակը
    varaddresses.clear();

    // տեքստային օբյեկտների հասցեները
    std::list<llvm::Value*> localtexts;
    
    // բոլոր լոկալ փոփոխականների, պարամետրերի 
    // և վերադարձվող արժեքի համար
    for( auto& vi : subr->locals ) {
        auto vty = llvmType(vi->type);
        auto addr = builder.CreateAlloca(vty, nullptr, vi->name + "_addr");
        varaddresses[vi->name] = addr;
        if( Type::Text == vi->type )
            localtexts.push_back(addr);
    }

    // պարամետրերի արժեքները վերագրել լոկալ օբյեկտներին
    for( auto& arg : fun->args() )
        if( arg.getType()->isPointerTy() ) {
            auto parval = builder.CreateCall(library["text_clone"], { &arg });
            builder.CreateStore(parval, varaddresses[arg.getName()]);
            localtexts.remove(varaddresses[arg.getName()]);
        }
        else
            builder.CreateStore(&arg, varaddresses[arg.getName()]);

    // տեքստային օբյեկտների համար գեներացնել սկզբնական արժեք
    // (սա արվում է վերագրման ժամանակ հին արժեքը ջնջելու և 
    // նորը վերագրելու սիմետրիկությունն ապահովելու համար)
    auto one = builder.getInt64(1);
    for( auto vp : localtexts ) {
        auto deva = builder.CreateCall(library["malloc"], { one });
        builder.CreateStore(deva, vp);
    }

    // գեներացնել ֆունկցիայի մարմինը
    emitSequence(std::dynamic_pointer_cast<Sequence>(subr->body));

    // ազատել տեքստային օբյեկտների զբաղեցրած հիշողությունը
    for( auto vi : subr->locals ) {
        if( Type::Number == vi->type )
            continue;
        if( vi->name == subr->name )
            continue;
        auto addr = varaddresses[vi->name];
        auto deva = builder.CreateCall(library["free"], { addr });
    }

    // վերադարձվող արժեք
    if( rtype->isVoidTy() )
        builder.CreateRetVoid();
    else {
        auto rv = builder.CreateLoad(varaddresses[subr->name]);
        builder.CreateRet(rv);
    }

    llvm::verifyFunction(*fun);
}

///
void IrEmitter::emitStatement( StatementPtr st )
{
    switch( st->kind ) {
        case NodeKind::Apply:
            break;
        case NodeKind::Sequence:
		  emitSequence(std::dynamic_pointer_cast<Sequence>(st));
            break;
        case NodeKind::Input:
		  emitInput(std::dynamic_pointer_cast<Input>(st));
            break;
        case NodeKind::Print:
		  emitPrint(std::dynamic_pointer_cast<Print>(st));
            break;
        case NodeKind::Let:
		  emitLet(std::dynamic_pointer_cast<Let>(st));
            break;
        case NodeKind::If:
		  emitIf(std::dynamic_pointer_cast<If>(st));
            break;
        case NodeKind::While:
            //emitWhile(static_cast<While*>(stat), endBB);
            break;
        case NodeKind::For:
		  emitFor(std::dynamic_pointer_cast<For>(st));
            break;
        case NodeKind::Call:
            break;
        default:
            break;
     }
 }
 
void IrEmitter::emitSequence( SequencePtr seq )
{
    for( auto st : seq->items )
        emitStatement(st);
}

///
void IrEmitter::emitLet( LetPtr let )
{
    auto val = emitExpression(let->expr);
    auto addr = varaddresses[let->varptr->name];
    if( Type::Text == let->varptr->type ) {
        // TODO: եթե վերագրման աջ կողմում ֆունկցիայի կանչ է կամ
        // տեքստերի կցման `&` գործողություն, ապա ոչ թե պատճենել
        // ժամանակավոր օբյեկտը, այլ միանգամից օգտագործել այն

        // TODO: տեքստի պատճենումը կատարել միայն տեքստային լիտերալներից
        // կամ այլ տեքստային փոփոխականներից
        auto e0 = builder.CreateCall(library["text_clone"], {val});
        builder.CreateCall(library["free"], addr);
        builder.CreateStore(e0, addr);
        if( val->getName().startswith("_temp_") )
            builder.CreateCall(library["free"], val);
    }
    else
        builder.CreateStore(val, addr);
}

///
void IrEmitter::emitInput( InputPtr inp )
{
    // կանչել գրադարանային ֆունկցիա
    // input_text() կամ input_number()
}

///
void IrEmitter::emitPrint( PrintPtr pri )
{
    // կանչել գրադարանային ֆունկցիա
    // print_text() կամ print_number()
}

//TODO լրացնել ուղղել
void IrEmitter::emitIf( IfPtr sif )
{
    // ընթացիկ ֆունկցիայի դուրս բերում
    auto insertBB = builder.GetInsertBlock();
    auto fun = insertBB->getParent();
    auto _mbb = llvm::BasicBlock::Create(context, "merge", fun);

#if 1
	StatementPtr sp = sif;
	while( auto _if = std::dynamic_pointer_cast<If>(sp) ) {
        auto cnd = emitExpression(_if->condition);

        auto _tbb = llvm::BasicBlock::Create(context, "then", fun, _mbb);
		//auto _ebb = llvm::BasicBlock::Create(context, "", fun);


        builder.CreateCondBr(cnd, _tbb, _mbb);
		builder.SetInsertPoint(_tbb);
		emitStatement(_if->decision);
		sp = _if->alternative;
	};

	if( nullptr != sp )
        emitStatement(sp);
#else
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
#endif
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
void IrEmitter::emitFor( ForPtr sfor )
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
llvm::Value* IrEmitter::emitExpression( ExpressionPtr expr )
{
    llvm::Value* res = nullptr;

    switch( expr->kind ) {
        case NodeKind::Number:
            res = emitNumber(std::dynamic_pointer_cast<Number>(expr));
            break;
        case NodeKind::Text:
            res = emitText(std::dynamic_pointer_cast<Text>(expr));
            break;
        case NodeKind::Variable:
            res = emitLoad(std::dynamic_pointer_cast<Variable>(expr));
            break;
        case NodeKind::Unary:
            break;
        case NodeKind::Binary:
            res = emitBinary(std::dynamic_pointer_cast<Binary>(expr));
            break;
        case NodeKind::Apply:
            break;
        default:
            break;
    }

    return res;
}

//
llvm::Value* IrEmitter::emitText( TextPtr txt )
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
llvm::Constant* IrEmitter::emitNumber( NumberPtr num )
{
    return llvm::ConstantFP::get(builder.getDoubleTy(), num->value);
}

///
llvm::LoadInst* IrEmitter::emitLoad( VariablePtr var )
{
    llvm::Value* vaddr = varaddresses[var->name];
    return builder.CreateLoad(vaddr, var->name);
}

/**/
llvm::Value* IrEmitter::emitBinary( BinaryPtr bin )
{
    llvm::Value* lhs = emitExpression(bin->subexpro);
    llvm::Value* rhs = emitExpression(bin->subexpri);

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
            ret = builder.CreateCall(library["text_concatenate"], {lhs, rhs}, "_temp_");
            break;
        default:
            break;
    }

    return ret;
}

/*
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

/**/
void IrEmitter::declareLibSubr( const std::string& name,
    llvm::ArrayRef<llvm::Type*> patys, llvm::Type* rty )
{
    auto functy = llvm::FunctionType::get(rty, patys, false);
    library[name] = llvm::Function::Create(functy, 
        llvm::GlobalValue::ExternalLinkage, name, module.get());
}

/**/
void IrEmitter::declareLibrary()
{
    auto _V = builder.getVoidTy();
    auto _N = builder.getDoubleTy();
    auto _T = builder.getInt8PtrTy();

    declareLibSubr("text_clone", {_T}, _T);
    declareLibSubr("text_input", {}, _T);
    declareLibSubr("text_print", {_T}, _V);
    declareLibSubr("text_concatenate", {_T, _T}, _T);

    declareLibSubr("number_input", {}, _N);
    declareLibSubr("number_print", {_N}, _V);

    declareLibSubr("malloc", { builder.getInt64Ty() }, _T);
    declareLibSubr("free", {_T}, _V);
}

/**/
llvm::Type* IrEmitter::llvmType( Type type )
{
    if( Type::Number == type )
        return builder.getDoubleTy();

    if( Type::Text == type )
        return builder.getInt8PtrTy();

    return builder.getVoidTy();
}
} // namespace llvm

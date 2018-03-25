
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
IrEmitter::IrEmitter()
    : context(), builder(context)
{
    // նախապատրաստել գրադարանակին (սպասարկող) ֆունկցիաները
    prepareLibrary();
}

///
bool IrEmitter::emitIrCode( ProgramPtr prog )
{
    try {
        emitProgram(prog);
        // TODO: ստացվող ֆայլի անունը կառուցել .bas ֆայլից
        // TODO: module-ը արտածել outstream-ի մեջ
        // օգտագործել PrintModulePass
        /* DEBUG */ module->print(llvm::errs(), nullptr);
        // module->print(outstream, nullptr);
    }
    catch(...) {
        return false;
    }
    
    return true;
}

///
void IrEmitter::emitProgram( ProgramPtr prog )
{
    // ստեղծել LLVM-ի Module օբյեկտ՝ դրա հասցեն պահելով
    // STL գրադարանի unique_ptr-ի մեջ։
    module = std::make_unique<llvm::Module>(prog->filename, context);

    // սեփական ֆունկցիաների հայտարարությունն ու սահմանումը
    // հարմար է առանձնացնել, որպեսզի Apply և Call գործողությունների
    // գեներացիայի ժամանակ արդեն գոյություն ունենան LLVM-ի
    // Function օբյեկտները
    
    // հայտարարել սեփական ֆունկցիաները
    declareSubroutines(prog);
    //  սահմանել սեփական ֆունկցիաները
    defineSubroutines(prog);

    // TODO: աշխատեցնել verify pass մոդուլի համար
}

//
void IrEmitter::emitSubroutine( SubroutinePtr subr )
{
    // մոդուլից վերցնել ֆունկցիայի հայտարարությունը դրան
    // մարմին ավելացնելու համար
    auto fun = module->getFunction(subr->name);

    // Քանի որ նախ գեներացվելու են ֆունկցիաների 
    // հայտարարությունները, ապա նույն այդ ցուցակով 
    // ամեն մի ֆունկցիայի համար գեներացվելու է մարմին,
    // բացառված է, որ fun ցուցիչը զրոյական լինի, սակայն,
    // կոդի ճիշտ կազմակերպվածության տեսակետից, ճիշտ կլինի,
    // որ այս և սրա նման դեպքերում աշխատանքը շարունակվի
    // ցուցիչի ոչ զրոյական լինելը ստուգելուց հետո
    if( nullptr == fun )
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
            auto parval = builder.CreateCall(LF("text_clone"), { &arg });
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
        auto deva = builder.CreateCall(LF("malloc"), { one });
        builder.CreateStore(deva, vp);
    }

    // գեներացնել ենթածրագրի մարմնի հրամանները
    emitStatement(subr->body);

    // ազատել տեքստային օբյեկտների զբաղեցրած հիշողությունը
    // Յուրաքանչյուր ֆունկցիայի ավարտին պետք է ազատել
    // տեքստային օբյեկտների զբաղեցրած հիշողությունը։ 
    // Բացառություն պիտի լինի միայն ֆունկցիայի անունով 
    // փոփոխականին կապված արժեքը, որը վերադարձվելու է
    // ֆունկցիային կանչողին
    for( auto vi : subr->locals ) {
        if( Type::Number == vi->type )
            continue;
        if( vi->name == subr->name )
            continue;
        auto addr = varaddresses[vi->name];
        auto deva = builder.CreateCall(LF("free"), { addr });
    }

    // վերադարձվող արժեք
    if( fun->getReturnType()->isVoidTy() )
        builder.CreateRetVoid();
    else {
        auto rv = builder.CreateLoad(varaddresses[subr->name]);
        builder.CreateRet(rv);
    }

    // ստուգել կառուցված ֆունկցիան
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
            emitWhile(std::dynamic_pointer_cast<While>(st));
            break;
        case NodeKind::For:
            emitFor(std::dynamic_pointer_cast<For>(st));
            break;
        case NodeKind::Call:
            emitCall(std::dynamic_pointer_cast<Call>(st));
            break;
        default:
            break;
    }
}

///
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
        builder.CreateCall(LF("free"), addr);
        if( !createsTempText(let->expr) )
            val = builder.CreateCall(LF("text_clone"), { val });
    }

    builder.CreateStore(val, addr);
}

///
void IrEmitter::emitInput( InputPtr inp )
{
    if( !inp->prompt.empty() ) {
        // TODO: գեներացնել հրավերքի արտածումը
    }
    
    // TODO: կարգի բերել
    if( Type::Text == inp->varptr->type ) {
        auto _inp = builder.CreateCall(LF("text_input"), {});
        builder.CreateStore(_inp, varaddresses[inp->varptr->name]);
    }
    else if( Type::Number == inp->varptr->type ) {
        auto _inp = builder.CreateCall(LF("number_input"), {});
        builder.CreateStore(_inp, varaddresses[inp->varptr->name]);
    }
}

///
void IrEmitter::emitPrint( PrintPtr pri )
{
    // կանչել գրադարանային ֆունկցիա
    // print_text() կամ print_number()
}

// TODO: լրացնել ուղղել
void IrEmitter::emitIf( IfPtr sif )
{
    // ընթացիկ ֆունկցիայի դուրս բերում
    auto _fun = builder.GetInsertBlock()->getParent();

    auto _eif = llvm::BasicBlock::Create(context, "");
    
    auto _begin = llvm::BasicBlock::Create(context, "", _fun);
    builder.CreateBr(_begin);

    builder.SetInsertPoint(_begin);
    
    StatementPtr sp = sif;
    while( auto _if = std::dynamic_pointer_cast<If>(sp) ) {
        // then֊բլոկ
        auto _tbb = llvm::BasicBlock::Create(context, "", _fun);
        // else-բլոկ
        auto _cbb = llvm::BasicBlock::Create(context, "", _fun);

        // գեներացնել պայմանը 
        auto cnd = emitExpression(_if->condition);
        
        // անցում ըստ պայմանի
        builder.CreateCondBr(cnd, _tbb, _cbb);

        // 
        placeBlock(_fun, _tbb);
        builder.SetInsertPoint(_tbb);
        emitStatement(_if->decision);
        builder.CreateBr(_eif);

        placeBlock(_fun, _cbb);
        builder.SetInsertPoint(_cbb);
        
        // հաջորդ բլոկի մշակում
        sp = _if->alternative;
    }
    
    // կա՞ արդյոք else-բլոկ
    if( nullptr != sp )
        emitStatement(sp);
    
    builder.CreateBr(_eif);

    _fun->getBasicBlockList().push_back(_eif);
    //placeBlock(_fun, _eif);
    builder.SetInsertPoint(_eif);
}

///
void IrEmitter::emitWhile( WhilePtr swhi )
{
    auto _fun = builder.GetInsertBlock()->getParent();
    
    auto _cond = llvm::BasicBlock::Create(context, "", _fun);
    auto _body = llvm::BasicBlock::Create(context, "", _fun);
    auto _end = llvm::BasicBlock::Create(context, "", _fun);

    builder.CreateBr(_cond);
    
    placeBlock(_fun, _cond);
    builder.SetInsertPoint(_cond);
    
    auto coex = emitExpression(swhi->condition);
    auto one = llvm::ConstantFP::get(builder.getDoubleTy(), 1.0);
    coex = builder.CreateFCmpOEQ(coex, one);
    builder.CreateCondBr(coex, _body, _end);

    placeBlock(_fun, _body);
    builder.SetInsertPoint(_body);
    emitStatement(swhi->body);
    builder.CreateBr(_cond);

    placeBlock(_fun, _end);
    builder.SetInsertPoint(_end);
}

///
void IrEmitter::emitFor( ForPtr sfor )
{
    auto _fun = builder.GetInsertBlock()->getParent();

    auto _cond = llvm::BasicBlock::Create(context, "", _fun);
    auto _body = llvm::BasicBlock::Create(context, "", _fun);
    auto _end = llvm::BasicBlock::Create(context, "", _fun);

    auto _param = varaddresses[sfor->parameter->name];
    // գեներացնել սկզբնական արժեքի արտահայտությունը
    auto _init = emitExpression(sfor->begin);
    // պարամետրին վերագրել սկզբնական արժեքը
    builder.CreateStore(_init, _param);
    // գեներացնել վերջնական արժեքի արտահայտությունը
    auto _finish = emitExpression(sfor->end);
    // քայլը հաստատուն է
    auto _step = llvm::ConstantFP::get(builder.getDoubleTy(), sfor->step->value);
    
    placeBlock(_fun, _cond);
    builder.SetInsertPoint(_cond);

    // եթե պարամետրի արժեքը >= (կամ <=, եթե քայլը բացասական է)
    // վերջնականից, ապա ավարտել ցիկլը
    if( sfor->step->value >= 0.0 ) {
        auto _pv = builder.CreateLoad(_param);
        auto coex = builder.CreateFCmpOLT(_pv, _finish);
        builder.CreateCondBr(coex, _body, _end);
    }
    else if( sfor->step->value <= 0.0 ) {
        auto _pv = builder.CreateLoad(_param);
        auto coex = builder.CreateFCmpOGT(_pv, _finish);
        builder.CreateCondBr(coex, _body, _end);
    }

    // գեներացնել մարմինը
    placeBlock(_fun, _body);
    builder.SetInsertPoint(_body);
    emitStatement(sfor->body);

    // պարամետրի արժեքին գումարել քայլի արժեքը
    auto parval = builder.CreateLoad(_param);
    auto nwpv = builder.CreateFAdd(parval, _step);
    builder.CreateStore(nwpv, _param);

    // կրկնել ցիկլը
    builder.CreateBr(_cond);

    placeBlock(_fun, _end);
    builder.SetInsertPoint(_end);
}

///
void IrEmitter::emitCall( CallPtr cal )
{
    emitApply(cal->subrcall);
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
            res = emitUnary(std::dynamic_pointer_cast<Unary>(expr));
            break;
        case NodeKind::Binary:
            res = emitBinary(std::dynamic_pointer_cast<Binary>(expr));
            break;
        case NodeKind::Apply:
            res = emitApply(std::dynamic_pointer_cast<Apply>(expr));
            break;
        default:
            break;
    }

    return res;
}

///
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

///
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

///
llvm::Value* IrEmitter::emitApply( ApplyPtr apy )
{
    // գեներացնել կանչի արգումենտները
    std::vector<llvm::Value*> argus, temps;
    for( auto& ai : apy->arguments ) {
        auto ap = emitExpression(ai);
        argus.push_back(ap);
        if( createsTempText(ai) )
            temps.push_back(ap);
    }

    // կանչել ֆունկցիան ու պահել արժեքը
    auto callee = module->getFunction(apy->procptr->name);
    auto calv = builder.CreateCall(callee, argus);

    // մաքրել կանչի ժամանակավոր արգումենտները
    for( auto ai : temps )
        if( ai->getType()->isPointerTy() )
            builder.CreateCall(LF("free"), { ai });

    // վերադարձնել կանչի արդյունքը
    return calv;
}

/**/
llvm::Value* IrEmitter::emitBinary( BinaryPtr bin )
{
    llvm::Value* lhs = emitExpression(bin->subexpro);
    llvm::Value* rhs = emitExpression(bin->subexpri);

    bool numopers = (Type::Number == bin->subexpro->type)
                 || (Type::Number == bin->subexpri->type);
    
    llvm::Value* ret = nullptr;
    switch( bin->opcode ) {
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
            if( numopers )
                ret = builder.CreateFCmpOEQ(lhs, rhs, "eq");
            else
                ret = builder.CreateCall(LF("text_eq"), {lhs, rhs});
            break;
        case Operation::Ne:
            if( numopers )
                ret = builder.CreateFCmpONE(lhs, rhs, "ne");
            else
                ret = builder.CreateCall(LF("text_ne"), {lhs, rhs});
            break;
        case Operation::Gt:
            if( numopers )
                ret = builder.CreateFCmpOGT(lhs, rhs, "gt");
            else
                ret = builder.CreateCall(LF("text_gt"), {lhs, rhs});
            break;
        case Operation::Ge:
            if( numopers )
                ret = builder.CreateFCmpOGE(lhs, rhs, "ge");
            else
                ret = builder.CreateCall(LF("text_ge"), {lhs, rhs});
            break;
        case Operation::Lt:
            if( numopers )
                ret = builder.CreateFCmpOLT(lhs, rhs, "lt");
            else
                ret = builder.CreateCall(LF("text_lt"), {lhs, rhs});
            break;
        case Operation::Le:
            if( numopers )
                ret = builder.CreateFCmpOLE(lhs, rhs, "le");
            else
                ret = builder.CreateCall(LF("text_le"), {lhs, rhs});
            break;
        case Operation::And:
            ret = builder.CreateAnd(lhs, rhs, "and");
            break;
        case Operation::Or:
            ret = builder.CreateOr(lhs, rhs, "or");
            break;
        case Operation::Conc:
            ret = builder.CreateCall(LF("text_conc"), { lhs, rhs });
            break;
        default:
            break;
    }

    return ret;
}

///
llvm::Value* IrEmitter::emitUnary( UnaryPtr un )
{
    auto val = emitExpression(un->subexpr);
    llvm::Value* res = nullptr;
    
    switch( un->opcode ) {
        case Operation::Sub:
            res = builder.CreateFNeg(val, "neg");
            break;
        case Operation::Not:
            res = builder.CreateNot(val, "not");
            break;
        default:
            break;
    }
    
    return res;
}

///
void IrEmitter::placeBlock( llvm::Function* fun, llvm::BasicBlock* bl )
{
    builder.ClearInsertionPoint();
    auto _ib = builder.GetInsertBlock();
    if( nullptr != _ib && nullptr != _ib->getParent() )
        fun->getBasicBlockList().insertAfter(_ib->getIterator(), bl);
    else
        fun->getBasicBlockList().push_back(bl);
    // TODO: builder.SerInsertPoint(bl);
}

///
void IrEmitter::prepareLibrary()
{
    auto _V = builder.getVoidTy();
    auto _B = builder.getInt1Ty();
    auto _N = builder.getDoubleTy();
    auto _T = builder.getInt8PtrTy();

    // տեքստային ֆունկցիաներ
    library["text_clone"] = llvm::FunctionType::get(_T, {_T}, false);
    library["text_input"] = llvm::FunctionType::get(_T, {}, false);
    library["text_print"] = llvm::FunctionType::get(_V, {_T}, false);
    library["text_conc"] = llvm::FunctionType::get(_T, {_T, _T}, false);
    library["text_mid"] = llvm::FunctionType::get(_T, {_T, _N, _N}, false);
    library["text_str"] = llvm::FunctionType::get(_T, {_N}, false);
    library["text_eq"] = llvm::FunctionType::get(_B, {_T, _T}, false);
    library["text_ne"] = llvm::FunctionType::get(_B, {_T, _T}, false);
    library["text_gt"] = llvm::FunctionType::get(_B, {_T, _T}, false);
    library["text_ge"] = llvm::FunctionType::get(_B, {_T, _T}, false);
    library["text_lt"] = llvm::FunctionType::get(_B, {_T, _T}, false);
    library["text_le"] = llvm::FunctionType::get(_B, {_T, _T}, false);

    // թվային ֆունկցիաներ
    library["number_input"] = llvm::FunctionType::get(_N, {}, false);
    library["number_print"] = llvm::FunctionType::get(_V, {_N}, false);

    // հիշողության ֆունկցիաներ
    library["malloc"] = llvm::FunctionType::get(_T, {builder.getInt64Ty()}, false);
    library["free"] = llvm::FunctionType::get(_V, {_T}, false);
}

///
llvm::Constant* IrEmitter::LF( const String& name )
{
    return module->getOrInsertFunction(name, library[name]);
}

///
llvm::Constant* IrEmitter::UF( const String& name )
{
    if( "MID$" == name )
        return LF("text_mid");

    if( "STR$" == name )
        return LF("text_str");

    return module->getFunction(name);
}

///
void IrEmitter::declareSubroutines( ProgramPtr prog )
{
    for( auto& subr : prog->members ) {
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

        // ստեղծել ֆունկցիայի հայտարարությունը
        auto functy = llvm::FunctionType::get(rtype, ptypes, false);
        auto linkage = llvm::GlobalValue::ExternalLinkage;
        llvm::Function::Create(functy, linkage, subr->name, module.get());
    }
}

///
void IrEmitter::defineSubroutines( ProgramPtr prog )
{
    for( auto& subr : prog->members )
        if( !subr->isBuiltIn )
            emitSubroutine(subr);
}

///
llvm::Type* IrEmitter::llvmType( Type type )
{
    if( Type::Number == type )
        return builder.getDoubleTy();

    if( Type::Text == type )
        return builder.getInt8PtrTy();

    return builder.getVoidTy();
}

///
bool IrEmitter::createsTempText( ExpressionPtr expr )
{
    // թվային արտահայտությունը ժամանակավոր օբյեկտ չի ստեղծում
    if( Type::Number == expr->type )
        return false;

    // տեքստային լիտերալներն ու փոփոխականներն էլ չեն ստեղծում
    if( NodeKind::Text == expr->kind || NodeKind::Variable == expr->kind )
        return false;

    return true;
}
} // namespace llvm

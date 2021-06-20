#include "BuildIR.h"

static llvm::Value * calArrInd(std::shared_ptr<NArrayIndex> ind, BuildContext & context, std::vector<uint64_t> asizes)
{
	auto totalexp = *(ind->expressions->rbegin());
	for(unsigned int i=asizes.size()-1; i>=1; i--)
	{
		auto tmp = make_shared<NBinaryOperator>(TMUL, make_shared<NInteger>(asizes[i]), ind->expressions->at(i-1));
		totalexp = make_shared<NBinaryOperator>(TPLUS, tmp, totalexp);
	}
	if (context.in_scanf) {
		auto res = totalexp->codeBuild(context);
		return context.builder.CreateLoad(res, false);
	}
	
	return totalexp->codeBuild(context);
}

// start
void BuildContext::buildIR (NBlock & root)
{
    std::cout << "In buildIR..." << std::endl;
    
    std::vector<llvm::Type *> argtyvec;
    llvm::FunctionType * mainfuncType = llvm::FunctionType::get(llvm::Type::getVoidTy(this->ircontext),
                                                                llvm::makeArrayRef(argtyvec), false);
    mainfunc = llvm::Function::Create(mainfuncType, GlobalValue::ExternalLinkage, "main");
    
    llvm::BasicBlock * bblock = llvm::BasicBlock::Create(this->ircontext, "entry", mainfunc, nullptr);
    this->builder.SetInsertPoint(bblock);
    push(bblock);
    llvm::Value * retval = root.codeBuild(*this);
    pop();

    return;                                                  
}
llvm::Value * NBlock::codeBuild(BuildContext & context)
{
    std::cout << "In NBlock::codeBuild..." << std::endl;
    
    llvm::Value * last = nullptr;
    for (auto it = this->statements->begin(); it != this->statements->end(); it++) 
    {
        last = (*it)->codeBuild(context);
    }
    return last;
}

// basic constant
llvm::Value * NInteger::codeBuild(BuildContext & context)
{
    std::cout << "In NInteger::codeBuild..." << std::endl;
    return llvm::ConstantInt::get(context.getVarType("int"), this->value, true);
}
llvm::Value * NDouble::codeBuild(BuildContext & context)
{
    std::cout << "In NDouble::codeBuild..." << std::endl;
    return llvm::ConstantFP::get(context.getVarType("double"), this->value);
}
llvm::Value * NLiteral::codeBuild(BuildContext & context)   // constant string or char
{
    std::cout << "In NLiteral::codeBuild..." << std::endl;
    return context.builder.CreateGlobalString(this->str, "string");
}

// about expression
llvm::Value * NIdentifier::codeBuild(BuildContext & context)
{
    std::cout << "In NIdentifier::codeBuild with id: " << this->name << std::endl;

    SymAttr * idattr = context.getSymAttr(this->name);
    llvm::Value * idval = idattr->local;
    if (!idattr || !idval)
    {   ErrorPrint("Error: Undeclared identifier: " + this->name); return nullptr;  }
    // string or array 
    if (idval->getType()->isPointerTy())
    {
        // auto aptr = context.builder.CreateLoad(idval, false);
        // array
        if (idval->getType()->getPointerElementType()->isArrayTy())
        {
            std::vector<llvm::Value *> indvec;
            indvec.push_back(llvm::ConstantInt::get(context.getVarType("int"), 0, false));
            auto ptr = context.builder.CreateInBoundsGEP(idval, indvec);
            return ptr;
        }
    }
    // array too, but in a form like int32*
    if (idattr->asizes.size() != 0)
    {
        return idval;
    }
    
    if (context.in_scanf && idattr->type != context.getVarType("string") )
    {
    	return idval;
    }
    
    // string or others
    return context.builder.CreateLoad(idval, false);   
}
llvm::Value * NExpressionStatement::codeBuild(BuildContext & context)
{
    std::cout << "In NExpressionStatement::codeBuild..." << std::endl;
    return this->expression->codeBuild(context);
}
llvm::Value * NAssignment::codeBuild(BuildContext & context)
{
    std::cout << "In NAssignment::codeBuild..." << std::endl;

    auto dstattr = context.getSymAttr(this->lhs->name);
    
    if (!dstattr)
    {   ErrorPrint("Error: Undeclared left-hand-side variable: " + this->lhs->name); return nullptr;  }
    llvm::Type * dsttype = dstattr->type;

    llvm::Value * srcval = this->rhs->codeBuild(context);
    llvm::Type * srctype = srcval->getType();
    // name equivalence
    if (srctype->getTypeID() != dsttype->getTypeID())
    {   ErrorPrint("Error: Assignment type not match."); return dstattr->local;   }
    
    context.builder.CreateStore(srcval, dstattr->local);
    return dstattr->local;
}
llvm::Value * NBinaryOperator::codeBuild(BuildContext & context)
{
    std::cout << "In NBinaryOperator::codeBuild..." << std::endl;

    llvm::Value * L = this->lhs->codeBuild(context);
    llvm::Value * R = this->rhs->codeBuild(context);
    bool isfloat = false;

    if (!(L->getType()->isIntegerTy()) || !(R->getType()->isIntegerTy()))
    {
        isfloat = true;
        if (L->getType()->isIntegerTy())
            L = context.builder.CreateSIToFP(L, llvm::Type::getDoubleTy(context.ircontext), "ITFLtmp");
        if (R->getType()->isIntegerTy())
            R = context.builder.CreateSIToFP(R, llvm::Type::getDoubleTy(context.ircontext), "ITFRtmp");
    } 
    if (!L || !R)
    {   ErrorPrint("Error: Wrong expr in binary op.");  return nullptr; }

    switch (this->op)
    {
        case TPLUS:
            return isfloat ? context.builder.CreateFAdd(L, R, "fpadd") : context.builder.CreateAdd(L, R, "siadd");
        case TMINUS:
            return isfloat ? context.builder.CreateFSub(L, R, "fpsub") : context.builder.CreateSub(L, R, "sisub");
        case TMUL:
            return isfloat ? context.builder.CreateFMul(L, R, "fpmul") : context.builder.CreateMul(L, R, "simul");
        case TDIV:
            return isfloat ? context.builder.CreateFDiv(L, R, "fpdiv") : context.builder.CreateSDiv(L, R, "sidiv");
        case TMOD:
            return isfloat ? context.builder.CreateFRem(L, R, "fpmod") : context.builder.CreateSRem(L, R, "simod");
        case TLSHIFT:
            if (isfloat)
            {   ErrorPrint("Error: Operator LEFT SHIFT with float."); return nullptr;   }
            else    return context.builder.CreateShl (L, R, "shl");
        case TRSHIFT:
            if (isfloat)
            {   ErrorPrint("Error: Operator RIGHT SHIFT with float."); return nullptr;   }
            else    return context.builder.CreateAShr(L, R, "ashr");
        case TAND:
            if (isfloat)
            {   ErrorPrint("Error: Operator AND with float."); return nullptr;   }
            else    return context.builder.CreateAnd (L, R, "and");
        case TOR:
            if (isfloat)
            {   ErrorPrint("Error: Operator OR with float."); return nullptr;   }
            else    return context.builder.CreateOr  (L, R, "or");
        case TXOR:
            if (isfloat)
            {   ErrorPrint("Error: Operator XOR with float."); return nullptr;   }
            else    return context.builder.CreateXor (L, R, "xor");
        // comparation operations
        case TCLT:
            return isfloat ? context.builder.CreateFCmpOLT(L, R, "fplt") : context.builder.CreateICmpSLT(L, R, "silt");
        case TCLE:
            return isfloat ? context.builder.CreateFCmpOLE(L, R, "fple") : context.builder.CreateICmpSLE(L, R, "sile");
        case TCGE:
            return isfloat ? context.builder.CreateFCmpOGE(L, R, "fpge") : context.builder.CreateICmpSGE(L, R, "sige");
        case TCGT:
            return isfloat ? context.builder.CreateFCmpOGT(L, R, "fpgt") : context.builder.CreateICmpSGT(L, R, "sigt");
        case TCEQ:
            return isfloat ? context.builder.CreateFCmpOEQ(L, R, "fpeq") : context.builder.CreateICmpEQ (L, R, "sieq");
        case TCNE:
            return isfloat ? context.builder.CreateFCmpONE(L, R, "fpne") : context.builder.CreateICmpNE (L, R, "sine");
        default:
        {
            ErrorPrint("Error: Unknown binary operator.");
            return nullptr;   
        }
    }
}

// about function
llvm::Value * NFunctionDeclaration::codeBuild(BuildContext & context)
{
    std::cout << "In NFunctionDeclaration::codeBuild..." << std::endl;
    // handle the arguments of the function
    std::vector<llvm::Type *> argtypes;
    for (auto & aarg: *this->args)
    {
        llvm::Type * aargtype = context.getVarType(aarg->type->name);
        if (aarg->type->is_array)
            argtypes.push_back(aargtype->getPointerTo());
        else argtypes.push_back(aargtype);
    }
    // get the type of return value
    llvm::Type * rettype = context.getVarType(this->type->name);
    // set up the function prototype
    llvm::FunctionType *fprot = llvm::FunctionType::get(rettype, llvm::makeArrayRef(argtypes), false);
    llvm::Function *function = llvm::Function::Create(fprot, llvm::GlobalValue::ExternalLinkage, this->id->name, context.themodule.get());

    if (!is_extern)
    {
        llvm::BasicBlock *bblock = llvm::BasicBlock::Create(context.ircontext, this->id->name + "()->entry", function, nullptr);         
        context.push(bblock);
        context.builder.SetInsertPoint(bblock);
        // set function args
        auto origarg = this->args->begin();
        for(auto &irarg: function->args())
        {
            // irarg.setName((*origarg)->id->name);
            llvm::Value * allocarg;
            
            if( (*origarg)->type->is_array ) {
            	 vector<uint64_t> tmp;
            	 
            	 // which assumes our function argument should be like int[2] rather than int[2+2].
            	 for (auto it=(*origarg)->type->arraysize->begin(); it!=(*origarg)->type->arraysize->end(); it++)
            	 {
            	     shared_ptr<NInteger> in = dynamic_pointer_cast<NInteger>(*it);
            	     tmp.push_back(in->value);
            	 }
                allocarg = context.builder.CreateAlloca(PointerType::get(context.getVarType((*origarg)->type->name), 0));
                context.builder.CreateStore(&irarg, allocarg, false);
                context.setSymAttr((*origarg)->id->name, &irarg, context.getVarType((*origarg)->type->name), tmp, true);
            } else {
                allocarg = (*origarg)->codeBuild(context);
                context.builder.CreateStore(&irarg, allocarg, false);
                context.setSymAttr((*origarg)->id->name, allocarg, context.getVarType((*origarg)->type->name), true);
            }
            origarg++;
        }
        // build the block part
        this->block->codeBuild(context);
        /*
        // build return value
        if( context.getCurRvalue() ) {
            context.builder.CreateRet(context.getCurRvalue());
        } else {
            ErrorPrint("Error: Function return value not founded.");
            return nullptr;
        }
        */
        if (this->type->name == "void")
        	context.builder.CreateRetVoid();
        context.pop();
        context.builder.SetInsertPoint(context.getCurBlock());
    }        
    return function; 
}

llvm::Value * NMethodCall::codeBuild(BuildContext & context)
{
    std::cout << "In NMethodCall::codeBuild..." << std::endl;

    llvm::Function * funccalled = context.themodule->getFunction(this->id->name);
    if( !funccalled )   ErrorPrint("Error: No function declaration of "+this->id->name+"().");

    if( funccalled->arg_size() != this->args->size() && this->id->name != "scanf" && this->id->name != "printf") {
        ErrorPrint("Error: Function arguments' size not match.");
    }

    std::vector<llvm::Value *> params;
    for (auto it = this->args->begin(); it != this->args->end(); it++)
    {
    	if (this->id->name == "scanf" && it != this->args->begin())
    		context.in_scanf = true;
    	
        auto tval = (*it)->codeBuild(context);
        if (tval->getType()->isArrayTy())
           tval = context.builder.CreateBitCast(tval, tval->getType()->getArrayElementType()->getPointerTo());
        else if (tval->getType()->isPointerTy() && tval->getType()->getPointerElementType()->isArrayTy()) {
           tval = context.builder.CreateBitCast(tval, tval->getType()->getPointerElementType()->getArrayElementType()->getPointerTo());
        }
        params.push_back(tval);
        
        if (this->id->name == "scanf" && it != this->args->begin())
    		context.in_scanf = false;
    }

    return context.builder.CreateCall(funccalled, params, "call_" + this->id->name + "()");
}

llvm::Value * NReturnStatement::codeBuild(BuildContext & context)
{
    std::cout << "In NReturnStatement::codeBuild..." << std::endl;

	if (this-> expression) 
	{
		llvm::Value * retval = this->expression->codeBuild(context);
		context.setCurRvalue(retval);
		context.builder.CreateRet(context.getCurRvalue());
		return retval;
    }
    else
    	return context.builder.CreateRetVoid(); 
}

// about variable
llvm::Value * NVariableDeclaration::codeBuild(BuildContext & context)
{
    std::cout << "In NVariableDeclaration::codeBuild..." << std::endl;

    llvm::Type* type = context.getVarType(this->type->name);
    // calculate elements number of the array
    llvm::Value * alloca = nullptr;
    std::vector<uint64_t> asizevec;
    if( this->type->is_array )
    {
        uint64_t elenum = 1;
        for(auto it=this->type->arraysize->begin(); it!=this->type->arraysize->end(); it++){
            NInteger* tint = dynamic_cast<NInteger*>(it->get());
            elenum *= tint->value;
            asizevec.push_back(tint->value);
        }
        // set array size vector in symbol table
        llvm::Value * elenumval = NInteger(elenum).codeBuild(context);
        auto atype = llvm::ArrayType::get(context.getVarType(this->type->name), elenum);
        alloca = context.builder.CreateAlloca(atype, nullptr, "array_"+this->id->name);
        // std::cout << alloca->hasName() << std::endl;
    }
    else
        alloca = context.builder.CreateAlloca(type, nullptr, "variable_"+this->id->name);

    context.setSymAttr(this->id->name, alloca, type, false);
    if(asizevec.size() != 0)
    	context.setArraySize(this->id->name, asizevec);
    // when it is not array
    if( this->assignmentExpr && !this->type->is_array ){
        NAssignment assign(this->id, this->assignmentExpr->at(0));
        assign.codeBuild(context);
    }

    return alloca;
}

// about array
llvm::Value * NArrayIndex::codeBuild(BuildContext & context)
{
    std::cout << "In NArrayIndex::codeBuild..." << std::endl;

    SymAttr * arrattr = context.getSymAttr(this->arrayname->name);
    if (!arrattr)
    {
        ErrorPrint("Error: Undeclared array -> " + this->arrayname->name);
        return nullptr;
    }
    llvm::Value * arrval = arrattr->local;
    llvm::Type * arrtype = arrattr->type;
    std::vector<uint64_t> arrsizevec = arrattr->asizes;
    
    auto index = calArrInd(make_shared<NArrayIndex>(*this), context, arrsizevec);
    std::vector<llvm::Value *> arrvec;
    if (arrval->hasName())
        arrvec.push_back(llvm::ConstantInt::get(llvm::Type::getInt64Ty(context.ircontext), 0));
    arrvec.push_back(index);

    auto elementptr = context.builder.CreateInBoundsGEP(arrval, llvm::makeArrayRef(arrvec), "elementPtr");
    if (context.in_scanf)
    	return elementptr;
    return context.builder.CreateAlignedLoad(elementptr, llvm::MaybeAlign(4));
}

llvm::Value * NArrayAssign::codeBuild(BuildContext & context)
{
    std::cout << "In NArrayAssign::codeBuild..." << std::endl;

    SymAttr * arrattr = context.getSymAttr(this->index->arrayname->name);
    if (!arrattr)
    {
        ErrorPrint("Error: Undeclared array -> " + this->index->arrayname->name);
        return nullptr;
    }
    llvm::Value * arrval = arrattr->local;
    llvm::Type * arrtype = arrattr->type;
    std::vector<uint64_t> arrsizevec = arrattr->asizes;

    auto assignval = this->assign->codeBuild(context);
    if (arrtype->getTypeID() != assignval->getType()->getTypeID())
    {
        ErrorPrint("Error: Array assign with wrong type.");
        return nullptr;
    }

/*
    for (auto it=this->index->expressions->begin(); it != this->index->expressions->end(); it++)
    {
        shared_ptr<NInteger> in = dynamic_pointer_cast<NInteger>(*it);
        std::cout << in->value << std::endl;
    }
    
    for (auto it=arrsizevec.begin(); it!=arrsizevec.end(); it++)
    {
        std::cout << "Hello" << std::endl;
        std::cout << *it << std::endl;
    }
*/
    
    Value* index = calArrInd(this->index, context, arrsizevec);
    std::vector<llvm::Value *> arrvec;
    
    if (arrval->hasName())
    	arrvec.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context.ircontext), 0));
    arrvec.push_back(index);

    auto elementptr = context.builder.CreateInBoundsGEP(arrval, arrvec, "elementPtr");
    return context.builder.CreateAlignedStore(assignval, elementptr, llvm::MaybeAlign(4));
}

// about struct
llvm::Value * NStructDeclaration::codeBuild(BuildContext & context)
{
    std::cout << "In NStructDeclaration::codeBuild..." << std::endl;

    std::vector<Type *> memtypevec;

    auto stype = llvm::StructType::create(context.ircontext, this->id->name);
    context.structstore->addStruType(this->id->name, stype);

    for(auto& mem: *this->member){
        context.structstore->addStruMem(this->id->name, mem->id->name, mem->type->name);
        memtypevec.push_back(context.getVarType(mem->type->name));
    }

    stype->setBody(memtypevec);
    return nullptr;
}
llvm::Value * NStructMember::codeBuild(BuildContext & context)
{
    std::cout << "In NStructMember::codeBuild..." << std::endl;

    llvm::Value * varptr = context.getSymAttr(this->tag->name)->local;
    auto sptr = context.builder.CreateLoad(varptr, false, "structPtr");
    sptr->setAlignment(llvm::Align(4));

    if( !sptr->getType()->isStructTy() )
    {
        ErrorPrint("Error: Not a struct -> " + this->tag->name);
        return nullptr;
    }

	std::string sname = sptr->getType()->getStructName().str();
    uint64_t memind = context.structstore->getStruMemSeq(sname, this->member->name);
    std::vector<Value*> indices;
    indices.push_back(ConstantInt::get(context.getVarType("int"), 0, false));
    indices.push_back(ConstantInt::get(context.getVarType("int"), memind, false));
    auto ptr = context.builder.CreateInBoundsGEP(varptr, indices, "memberPtr");
    return context.builder.CreateLoad(ptr, false);
}
llvm::Value * NStructAssign::codeBuild(BuildContext & context)
{
    std::cout << "In NStructAssign::codeBuild..." << std::endl;

    llvm::Value * varptr = context.getSymAttr(this->struct_mem->tag->name)->local;
    auto sptr = context.builder.CreateLoad(varptr, false, "structPtr");
    sptr->setAlignment(llvm::Align(4));

    if( !sptr->getType()->isStructTy() )
    {
        ErrorPrint("Error: Not a struct -> " + this->struct_mem->tag->name);
        return nullptr;
    }

	std::string sname = sptr->getType()->getStructName().str();
    uint64_t memind = context.structstore->getStruMemSeq(sname, this->struct_mem->member->name);
    std::vector<Value*> indices;
    auto assignval = this->assign->codeBuild(context);
    indices.push_back(ConstantInt::get(context.getVarType("int"), 0, false));
    indices.push_back(ConstantInt::get(context.getVarType("int"), memind, false));
    auto ptr = context.builder.CreateInBoundsGEP(varptr, indices, "memberPtr");
    return context.builder.CreateStore(assignval, ptr);
}

// about control
llvm::Value * NIfStatement::codeBuild(BuildContext & context)
{
    std::cout << "In NIfStatement::codeBuild..." << std::endl;

    llvm::Value * condval = this->condition->codeBuild(context);
    if (!condval)   return nullptr;
    // turn the condition-value into bool
    if( condval->getType()->isIntegerTy() ) {
        condval = context.builder.CreateIntCast(condval, llvm::Type::getInt1Ty(context.ircontext), true);
        condval = context.builder.CreateICmpNE(condval, llvm::ConstantInt::get(llvm::Type::getInt1Ty(context.ircontext), 0, true));
    }else if( condval->getType()->isDoubleTy() ) {
        condval = context.builder.CreateFCmpONE(condval, llvm::ConstantFP::get(context.ircontext, llvm::APFloat(0.0)));
    }
	
    llvm::Function *function = context.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenbb = llvm::BasicBlock::Create(context.ircontext, "thenblock", function);
    llvm::BasicBlock *elsebb = llvm::BasicBlock::Create(context.ircontext, "elseblock");
    llvm::BasicBlock *mergebb = llvm::BasicBlock::Create(context.ircontext, "mergeblock");
    
    if( this->FalseBlock ){
        context.builder.CreateCondBr(condval, thenbb, elsebb);
    } else{
        context.builder.CreateCondBr(condval, thenbb, mergebb);
    }

    context.builder.SetInsertPoint(thenbb);

    context.push(thenbb);
    this->TrueBlock->codeBuild(context);
    context.pop();

    thenbb = context.builder.GetInsertBlock();

    if( thenbb->getTerminator() == nullptr ){       
        context.builder.CreateBr(mergebb);
    }

    if( this->FalseBlock ){
        function->getBasicBlockList().push_back(elsebb);    
        context.builder.SetInsertPoint(elsebb);            

        context.push(thenbb);
        this->FalseBlock->codeBuild(context);
        context.pop();

        context.builder.CreateBr(mergebb);
    }

    function->getBasicBlockList().push_back(mergebb);        
    context.builder.SetInsertPoint(mergebb);        

    return nullptr;
}
llvm::Value * NWhileStatement::codeBuild(BuildContext & context)
{
    std::cout << "In NWhileStatement::codeBuild..." << std::endl;

    llvm::Function *function = context.builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *loop = llvm::BasicBlock::Create(context.ircontext, "whileLoop", function);
    llvm::BasicBlock *after = llvm::BasicBlock::Create(context.ircontext, "afterWhile");

    llvm::Value * condval = this->testcase->codeBuild(context);
    if (!condval)   return nullptr;
    // turn the condition-value into bool
    if( condval->getType()->isIntegerTy() ) {
        condval = context.builder.CreateIntCast(condval, llvm::Type::getInt1Ty(context.ircontext), true);
        condval = context.builder.CreateICmpNE(condval, llvm::ConstantInt::get(llvm::Type::getInt1Ty(context.ircontext), 0, true));
    }else if( condval->getType()->isDoubleTy() ) {
        condval = context.builder.CreateFCmpONE(condval, llvm::ConstantFP::get(context.ircontext, llvm::APFloat(0.0)));
    }
    
    context.builder.CreateCondBr(condval, loop, after);
    context.builder.SetInsertPoint(loop);
    context.push(loop);
    this->block->codeBuild(context);
  	context.pop();
  
    condval = this->testcase->codeBuild(context);
    if (!condval)   return nullptr;
    // turn the condition-value into bool
    if( condval->getType()->isIntegerTy() ) {
        condval = context.builder.CreateIntCast(condval, llvm::Type::getInt1Ty(context.ircontext), true);
        condval = context.builder.CreateICmpNE(condval, llvm::ConstantInt::get(llvm::Type::getInt1Ty(context.ircontext), 0, true));
    }else if( condval->getType()->isDoubleTy() ) {
        condval = context.builder.CreateFCmpONE(condval, llvm::ConstantFP::get(context.ircontext, llvm::APFloat(0.0)));
    }
    context.builder.CreateCondBr(condval, loop, after);
    
    function->getBasicBlockList().push_back(after);
    context.builder.SetInsertPoint(after);

    return nullptr;
}


#ifndef __BUILDIR_H__
#define __BUILDIR_H__

#include <iostream>
#include <exception>
#include <stack>
#include <vector>
#include <memory>
#include <string>
#include <map>
#include <regex>

#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Bitcode/BitcodeWriter.h>

#include "ASTNode.h"
#include "grammar.hpp"

typedef std::pair<std::string, std::string> StruTypeIDPair;

static void ErrorPrint(std::string errstr)
{
    std::cerr << errstr << std::endl;
}

// for struct handle
class StructHandle {
public:
    //   struct identifier     vector of struct mem
    std::map<std::string, std::vector<StruTypeIDPair>> smem;   // struct members
    std::map<std::string, llvm::StructType *> stype;           // struct types

    void addStruMem (std::string sname, std::string memname, std::string memtype)
    {
        if (stype.find(sname) == stype.end()) 
        { ErrorPrint("Error: no such struct."); }
        smem[sname].push_back(std::make_pair(memname, memtype));
    }
    void addStruType (std::string sname, llvm::StructType * lstype)
    {
        stype[sname] = lstype;
        smem[sname] = std::vector<StruTypeIDPair>();
    }
    llvm::StructType * getStruType (std::string sname)
    {
        return stype[sname];
    }
    int getStruMemSeq(std::string sname, std::string memname)
    {
        if (stype.find(sname) == stype.end()) 
        { ErrorPrint("Error: no such struct."); return 0; }

        for(auto it=smem[sname].begin(); it!=smem[sname].end(); it++) {
            if( it->first == memname ){
                return std::distance(smem[sname].begin(), it);
            }
        }
        // else if not found
        ErrorPrint("Error: no such struct.");
        return 0;
    }
    bool isStruct (std::string sname) { return (stype.find(sname) != stype.end()); }
};

// a class of < symbol attributes >
class SymAttr {
public:
    llvm::Value * local;                    // a pointer to the local var
    llvm::Type * type;                      // the type of the var
    std::vector<uint64_t>  asizes;          // array's dimension
    bool funcarg;                           // if it is function argument

    SymAttr(llvm::Value * local, llvm::Type * type, std::vector<uint64_t> asizes, bool flag = false):
            local(local), type(type), asizes(asizes), funcarg(flag)  {}
            
    SymAttr(llvm::Value * local, llvm::Type * type, bool flag = false):
            local(local), type(type), funcarg(flag)  {}
};

class IRBlock {
public:
    llvm::BasicBlock * irblock;
    std::map<std::string, SymAttr *> attributes;
    llvm::Value * rvalue;                    // return value

    IRBlock(llvm::BasicBlock * irblock, llvm::Value * rvalue = nullptr): 
            irblock(irblock), rvalue(rvalue) {}
};

// set up the context of codes
// to be a global entity
class BuildContext {
private:
    std::vector<IRBlock *> blockStack;
    llvm::Function * mainfunc;                // main function
public:
    llvm::LLVMContext ircontext;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> themodule;
    StructHandle * structstore;
    bool in_scanf;
    
    // initialize
    BuildContext(): builder(ircontext) { themodule = unique_ptr<llvm::Module>(new llvm::Module("main", this->ircontext)); 
    structstore = new StructHandle();
    in_scanf = false;}
    // build ir
    void buildIR (NBlock & root);
    // get info
    SymAttr * getSymAttr (const std::string& name) const
    {
        for (auto it = blockStack.rbegin(); it != blockStack.rend(); it++)
        {
            // if the name can be found
            if ((*it)->attributes.find(name) != (*it)->attributes.end())
                return (*it)->attributes[name];
        }
        // if not found
        return nullptr;
    }
    llvm::Value * getCurRvalue () 
    {   return blockStack.back()->rvalue;   }
    llvm::BasicBlock * getCurBlock () 
    {   return blockStack.back()->irblock;           }
    // set info
    void setSymAttr (const std::string& name, llvm::Value* tlocal, llvm::Type* ttype, bool flag)
    {
        blockStack.back()->attributes[name] = new SymAttr(tlocal, ttype, flag);
    }
    void setSymAttr (const std::string& name, llvm::Value* tlocal, llvm::Type* ttype, std::vector<uint64_t>& tsizes, bool flag)
    {
        blockStack.back()->attributes[name] = new SymAttr(tlocal, ttype, tsizes, flag);
    }
    
    void setCurRvalue (llvm::Value * trvalue) 
    {   blockStack.back()->rvalue = trvalue;    }
    void setArraySize(std::string name, std::vector<uint64_t> value)
    {   blockStack.back()->attributes[name]->asizes = value;	}
    // push & pop
    void push (llvm::BasicBlock * tblock) 
    {   blockStack.push_back(new IRBlock(tblock));  }
    void pop()
    {
        IRBlock * top = blockStack.back();
        blockStack.pop_back();
        delete top;
    }
    
    // type system
    llvm::Type * getVarType(const std::string& type) 
    {
        if      (type == "int")     { return llvm::Type::getInt32Ty(ircontext); } 
        else if (type == "float")   { return llvm::Type::getFloatTy(ircontext); } 
        else if (type == "double")  { return llvm::Type::getDoubleTy(ircontext);} 
        else if (type == "char")    { return llvm::Type::getInt8Ty(ircontext);  } 
        else if (type == "bool")    { return llvm::Type::getInt1Ty(ircontext);  } 
        else if (type == "string")  { return llvm::PointerType::getInt8PtrTy(ircontext); } 
        else if (type == "void")    { return llvm::Type::getVoidTy(ircontext);  }

        else if( structstore->isStruct(type) ) { return structstore->getStruType(type); }

        return nullptr;
    }
    
};



#endif

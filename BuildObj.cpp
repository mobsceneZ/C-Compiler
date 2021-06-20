#include "BuildObj.h"

using namespace llvm;

void buildObj(BuildContext & context, const string& filename)
{
    // Initialize the target registry etc.
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    auto targetTriple = sys::getDefaultTargetTriple();
    context.themodule->setTargetTriple(targetTriple);

    std::string error;
    auto Target = TargetRegistry::lookupTarget(targetTriple, error);

    if( !Target ){
        errs() << error;
        return;
    }

    auto CPU = "generic";
    auto features = "";

    TargetOptions opt;
    auto RM = Optional<Reloc::Model>();
    auto theTargetMachine = Target->createTargetMachine(targetTriple, CPU, features, opt, RM);

    context.themodule->setDataLayout(theTargetMachine->createDataLayout());
    context.themodule->setTargetTriple(targetTriple);

    std::error_code EC;
    raw_fd_ostream dest(filename.c_str(), EC, sys::fs::F_None);

    legacy::PassManager pass;
    auto fileType = CGFT_ObjectFile;

    if( theTargetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType) ) {
        errs() << "theTargetMachine can't emit a file of this type";
        return;
    }

    pass.run(*context.themodule.get());
    dest.flush();

    outs() << "Object code wrote to " << filename.c_str() << "\n";

    return;
}

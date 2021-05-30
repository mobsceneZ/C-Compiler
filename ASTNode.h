#ifndef _ASTNODE_H
#define _ASTNODE_H

#include <string>
#include <vector>
#include <memory>
#include <iostream>
#include <llvm/IR/Value.h>

using namespace std;
using namespace llvm;

class CodeGenContext;
class NStatement;
class NExpression;
class NVariableDeclaration;

typedef vector<shared_ptr<NStatement>> StatementList;
typedef vector<shared_ptr<NExpression>> ExpressionList;
typedef vector<shared_ptr<NVariableDeclaration>> VariableList;

struct 

class Node {
public:
    Node() {}
    virtual ~Node() {}
    virtual string TypeName() {} = 0;
    virtual Value* codeGen(CodeGenContext& context) {return NULL;}
}

class NExpression : public Node {
public:
    NExpression() {}
    string TypeName() {return "NExpression"; }
}

class NStatement : public Node {
public:
    NStatement() {}
    string TypeName() {return "NStatement"; }
}



#endif
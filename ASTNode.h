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

class Node {
public:
    Node() {}
    virtual ~Node() {}
    virtual string TypeName() = 0;
    virtual Value* codeGen(CodeGenContext& context) {return NULL;}
};

class NExpression : public Node {
public:
    NExpression() {}
    string TypeName() {return "NExpression"; }
};

class NStatement : public Node {
public:
    NStatement() {}
    string TypeName() {return "NStatement"; }
};

// For integer constants, like 2.
class NInteger : public NExpression {
public:
    long long value;
    NInteger() {}
    NInteger(long long value):value(value) {}
    string TypeName() {return "NInteger"; }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

// For double constants, like 3.14159.
class NDouble : public NExpression {
public:
    double value;
    NDouble() {}
    NDouble(double value): value(value) {}
    string TypeName() {return "NDouble"; }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

// Two Possible Uses:
// 1. Serve As Identifier's TypeName (is_type = true)
//  (1) if is_array = true, vector arraysize denotes the size of each dimension.
// 2. Identifier itself (is_type = false)
class NIdentifier : public NExpression {
public:
    string name;
    bool is_type = false;
    bool is_array = false;
    shared_ptr<ExpressionList> arraysize = make_shared<ExpressionList>();

    NIdentifier() {}
    NIdentifier(string & name):name(name) {}
    void SetType() {this->is_type = true; }
    void SetArray() {this->is_array = true; }
    void AddDimension(shared_ptr<NExpression> p) {this->arraysize->push_back(p); }

    string TypeName() {return "NIdentifier"; }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

// expr -> identifier LPAREN Args RPAREN
class NMethodCall : public NExpression {
public:
    shared_ptr<NIdentifier> id;
    shared_ptr<ExpressionList> args;

    NMethodCall() {}
    NMethodCall(shared_ptr<NIdentifier> id, shared_ptr<ExpressionList> args):id(id), args(args) {}

    string TypeName() {return "NMethodCall"; }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

// expr -> expr comparison expr
// expr -> expr arithmetic expr
class NBinaryOperator : public NExpression {
public:
    int op;
    shared_ptr<NExpression> lhs, rhs;

    NBinaryOperator() {}
    NBinaryOperator(int op, shared_ptr<NExpression> lhs, shared_ptr<NExpression> rhs):op(op), lhs(lhs), rhs(rhs) {}

    string TypeName() {return "NBinaryOperator"; }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

// expr -> identifier TEQUAL expr
class NAssignment : public NExpression {
public:
    shared_ptr<NIdentifier> lhs; 
    shared_ptr<NExpression> rhs;

    NAssignment() {}
    NAssignment(shared_ptr<NIdentifier> lhs, shared_ptr<NExpression> rhs): lhs(lhs), rhs(rhs) {}

    
};

#endif
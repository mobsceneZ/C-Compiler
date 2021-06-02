#ifndef _ASTNODE_H
#define _ASTNODE_H

#include <string>
#include <vector>
#include <memory>
#include <iostream>
#include "llvm/IR/Value.h"

using namespace std;
using namespace llvm;

// class CodeGenContext;
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
    // virtual Value* codeGen(CodeGenContext& context) {return NULL;}
};

class NExpression : public Node {
public:
    NExpression() {}
    ~NExpression() {}
    string TypeName() {return "NExpression"; }
};

class NStatement : public Node {
public:
    NStatement() {}
    ~NStatement() {}
    string TypeName() {return "NStatement"; }
};

// For integer constants, like 2.
class NInteger : public NExpression {
public:
    long long value;
    NInteger() {}
    ~NInteger() {}
    NInteger(long long value):value(value) {}
    string TypeName() {return "NInteger"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// For double constants, like 3.14159.
class NDouble : public NExpression {
public:
    double value;
    NDouble() {}
    ~NDouble() {}
    NDouble(double value): value(value) {}
    string TypeName() {return "NDouble"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
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
    ~NIdentifier() {}
    NIdentifier(string & name):name(name) {}
    void SetType() {this->is_type = true; }
    void SetArray() {this->is_array = true; }
    void AddDimension(shared_ptr<NExpression> p) {this->arraysize->push_back(p); }

    string TypeName() {return "NIdentifier"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// expr -> identifier LPAREN Args RPAREN
class NMethodCall : public NExpression {
public:
    shared_ptr<NIdentifier> id;
    shared_ptr<ExpressionList> args;

    NMethodCall() {}
    ~NMethodCall() {}
    NMethodCall(shared_ptr<NIdentifier> id, shared_ptr<ExpressionList> args):id(id), args(args) {}

    string TypeName() {return "NMethodCall"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// expr -> expr comparison expr
// expr -> expr arithmetic expr
class NBinaryOperator : public NExpression {
public:
    int op;
    shared_ptr<NExpression> lhs, rhs;

    NBinaryOperator() {}
    ~NBinaryOperator() {}
    NBinaryOperator(int op, shared_ptr<NExpression> lhs, shared_ptr<NExpression> rhs):op(op), lhs(lhs), rhs(rhs) {}

    string TypeName() {return "NBinaryOperator"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// expr -> identifier TEQUAL expr
class NAssignment : public NExpression {
public:
    shared_ptr<NIdentifier> lhs; 
    shared_ptr<NExpression> rhs;

    NAssignment() {}
    ~NAssignment() {}
    NAssignment(shared_ptr<NIdentifier> lhs, shared_ptr<NExpression> rhs): lhs(lhs), rhs(rhs) {}

    string TypeName() {return "NAssignment"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);  
};

// NBlock presents collection of statements.
class NBlock : public NStatement {
public:
    shared_ptr<StatementList> statements = make_shared<StatementList>();

    NBlock() {}
    ~NBlock() {}
    void AddStatement(shared_ptr<NStatement> p) {this->statements->push_back(p);}

    string TypeName() {return "NBlock"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// stmt -> expr SEMI
class NExpressionStatement : public NStatement {
public:
    shared_ptr<NExpression> expression;

    NExpressionStatement() {}
    ~NExpressionStatement() {}
    NExpressionStatement(shared_ptr<NExpression> expression): expression(expression) {}

    string TypeName() {return "NExpressionStatement";}
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// type id optional[assignment]
class NVariableDeclaration : public NStatement {
public:
    shared_ptr<NIdentifier> type;
    shared_ptr<NIdentifier> id;
    shared_ptr<ExpressionList> assignmentExpr = nullptr;

    NVariableDeclaration() {}
    ~NVariableDeclaration() {}
    NVariableDeclaration(shared_ptr<NIdentifier> type, shared_ptr<NIdentifier> id, shared_ptr<ExpressionList> expr = NULL)
    {
        this->type = type;
        this->id = id;
        this->assignmentExpr = expr;
        assert(type->is_type);
        assert(!type->is_array || (type->is_array && type->arraysize != nullptr));
    }
    NVariableDeclaration(shared_ptr<NIdentifier> type, shared_ptr<NIdentifier> id, shared_ptr<NExpression> expr)
    {
        this->type = type;
        this->id = id;
        if (expr != nullptr)
        {
            this->assignmentExpr = make_shared<ExpressionList>();
            this->assignmentExpr->push_back(expr);
        }
        assert(type->is_type);
        assert(!type->is_array || (type->is_array && type->arraysize != nullptr));
    }

    string TypeName() {return "NVariableDeclaration";}
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// (1) extern type id args
// (2) type id args CompSt
class NFunctionDeclaration : public NStatement {
public:
    shared_ptr<NIdentifier> type;
    shared_ptr<NIdentifier> id;
    shared_ptr<VariableList> args = nullptr;
    shared_ptr<NBlock> block;
    bool is_extern = false;

    NFunctionDeclaration() {}
    ~NFunctionDeclaration() {}
    NFunctionDeclaration(shared_ptr<NIdentifier> type, shared_ptr<NIdentifier> id, shared_ptr<VariableList> args, shared_ptr<NBlock> block, bool is_extern = false)
    {
        this->type = type;
        this->id = id;
        this->args = args;
        this->block = block;
        this->is_extern = is_extern;

        assert(type->is_type);
    }

    string TypeName() {return "NFunctionDeclaration"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// TSTRUCT id TLBRACE members TRBRACE
class NStructDeclaration : public NStatement {
public:
    shared_ptr<NIdentifier> id;
    shared_ptr<VariableList> member = nullptr;

    NStructDeclaration() {}
    ~NStructDeclaration() {}
    NStructDeclaration(shared_ptr<NIdentifier> id, shared_ptr<VariableList> member):id(id), member(member) {}

    string TypeName() {return "NStructDeclaration"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// Stmt -> TRETURN expr SEMI
class NReturnStatement : public NStatement {
public:
    shared_ptr<NExpression> expression;

    NReturnStatement() {}
    ~NReturnStatement() {}
    NReturnStatement(shared_ptr<NExpression> expression): expression(expression) {}

    string TypeName() {return "NreturnStatement"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// if_stmt -> TIF TLPAREN expr TRPAREN CompSt
// if_stmt -> TIF TLPAREN expr TRPAREN CompSt TELSE CompSt
// if_stmt -> TIF TLPAREN expr TRPAREN CompSt TELSE if_stmt
class NIfStatement : public NStatement {
public:
    shared_ptr<NExpression> condition;
    shared_ptr<NBlock> TrueBlock;
    shared_ptr<NBlock> FalseBlock;

    NIfStatement() {}
    ~NIfStatement() {}
    NIfStatement(shared_ptr<NExpression> condition, shared_ptr<NBlock> TrueBlock, shared_ptr<NBlock> FalseBlock = nullptr)
    {
        this->condition = condition;
        this->TrueBlock = TrueBlock;
        this->FalseBlock = FalseBlock;
    }

    string TypeName() {return "NIfStatement"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// while_stmt -> TWHILE TLPAREN expr TRPAREN CompSt
class NWhileStatement : public NStatement {
public:
    shared_ptr<NExpression> testcase;
    shared_ptr<NBlock> block;

    NWhileStatement() {}
    ~NWhileStatement() {}
    NWhileStatement(shared_ptr<NBlock> block, shared_ptr<NExpression> testcase = nullptr): testcase(testcase), block(block) 
    {
        if(testcase == nullptr)
            this->testcase = make_shared<NInteger>(1);
    }

    string TypeName() {return "NWhileStatement"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// expr -> STRUCT_TAG TDOT identifier
class NStructMember : public NExpression {
public: 
    shared_ptr<NIdentifier> tag;
    shared_ptr<NIdentifier> member;

    NStructMember() {}
    ~NStructMember() {}
    NStructMember(shared_ptr<NIdentifier> tag, shared_ptr<NIdentifier> member): tag(tag), member(member) {}

    string TypeName() {return "NStructMember"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};


// array_element -> identifier TLBRACKET expr TRBRACKET
// array_element -> array_element TLBRACKET expr TRBRACKET
class NArrayIndex : public NExpression {
public:
    shared_ptr<NIdentifier> arrayname;
    shared_ptr<ExpressionList> expressions = make_shared<ExpressionList>();

    NArrayIndex() {}
    ~NArrayIndex() {}
    NArrayIndex(shared_ptr<NIdentifier> arrayname, shared_ptr<NExpression> expression)
    {
        this->arrayname = arrayname;
        this->expressions->push_back(expression);
    }
    NArrayIndex(shared_ptr<NIdentifier> arrayname, shared_ptr<ExpressionList> expressions)
    {
        this->arrayname = arrayname;
        this->expressions = expressions;
    }

    string TypeName() {return "NArrayIndex"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// assignment -> array_element TEQUAL expr
class NArrayAssign : public NExpression {
public:
    shared_ptr<NArrayIndex> index;
    shared_ptr<NExpression> assign;

    NArrayAssign() {}
    ~NArrayAssign() {}
    NArrayAssign(shared_ptr<NArrayIndex> index, shared_ptr<NExpression> assign): index(index), assign(assign) {}

    string TypeName() {return "NArrayAssign"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// assignment -> STRUCT_TAG TDOT identifier TEQUAL expr
class NStructAssign : public NExpression {
public:
    shared_ptr<NStructMember> struct_mem;
    shared_ptr<NExpression> assign;

    NStructAssign() {}
    ~NStructAssign() {}
    NStructAssign(shared_ptr<NStructMember> struct_mem, shared_ptr<NExpression> assign): struct_mem(struct_mem), assign(assign) {}

    string TypeName() {return "NStructAssign"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

// expr -> TLITERAL
class NLiteral : public NExpression {
public:
    string str;

    NLiteral() {}
    ~NLiteral() {}
    NLiteral(const string str)
    {
        // when string literal is matched by yylex(), it's surrounded by quotes.
        this->str = str.substr(1, str.length()-2);
    }

    string TypeName() {return "NLiteral"; }
    // virtual llvm::Value* codeGen(CodeGenContext& context);
};

#endif
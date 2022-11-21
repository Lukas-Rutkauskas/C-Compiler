#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <llvm/IR/Constant.h>
#include <map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

using namespace llvm;
using namespace llvm::sys;

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::vector<std::map<std::string, AllocaInst*>> NamedValues;
static std::map<std::string, GlobalVariable*> GlobalValues;


FILE *pFile;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns one of these for known things.
enum TOKEN_TYPE {

  IDENT = -1,        // [a-zA-Z_][a-zA-Z_0-9]*
  ASSIGN = int('='), // '='

  // delimiters
  LBRA = int('{'),  // left brace
  RBRA = int('}'),  // right brace
  LPAR = int('('),  // left parenthesis
  RPAR = int(')'),  // right parenthesis
  SC = int(';'),    // semicolon
  COMMA = int(','), // comma

  // types
  INT_TOK = -2,   // "int"
  VOID_TOK = -3,  // "void"
  FLOAT_TOK = -4, // "float"
  BOOL_TOK = -5,  // "bool"

  // keywords
  EXTERN = -6,  // "extern"
  IF = -7,      // "if"
  ELSE = -8,    // "else"
  WHILE = -9,   // "while"
  RETURN = -10, // "return"
  // TRUE   = -12,     // "true"
  // FALSE   = -13,     // "false"

  // literals
  INT_LIT = -14,   // [0-9]+
  FLOAT_LIT = -15, // [0-9]+.[0-9]+
  BOOL_LIT = -16,  // "true" or "false" key words

  // logical operators
  AND = -17, // "&&"
  OR = -18,  // "||"

  // operators
  PLUS = int('+'),    // addition or unary plus
  MINUS = int('-'),   // substraction or unary negative
  ASTERIX = int('*'), // multiplication
  DIV = int('/'),     // division
  MOD = int('%'),     // modular
  NOT = int('!'),     // unary negation

  // comparison operators
  EQ = -19,      // equal
  NE = -20,      // not equal
  LE = -21,      // less than or equal to
  LT = int('<'), // less than
  GE = -23,      // greater than or equal to
  GT = int('>'), // greater than

  // special tokens
  EOF_TOK = 0, // signal end of file

  // invalid
  INVALID = -100 // signal invalid token
};

// TOKEN struct is used to keep track of information about a token
struct TOKEN {
  int type = -100;
  std::string lexeme;
  int lineNo;
  int columnNo;
};

static std::string IdentifierStr; // Filled in if IDENT
static int IntVal;                // Filled in if INT_LIT
static bool BoolVal;              // Filled in if BOOL_LIT
static float FloatVal;            // Filled in if FLOAT_LIT
static std::string StringVal;     // Filled in if String Literal
static int lineNo, columnNo;

static TOKEN returnTok(std::string lexVal, int tok_type) {
  TOKEN return_tok;
  return_tok.lexeme = lexVal;
  return_tok.type = tok_type;
  return_tok.lineNo = lineNo;
  return_tok.columnNo = columnNo - lexVal.length() - 1;
  return return_tok;
}

// Read file line by line -- or look for \n and if found add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
static TOKEN gettok() {

  static int LastChar = ' ';
  static int NextChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar)) {
    if (LastChar == '\n' || LastChar == '\r') {
      lineNo++;
      columnNo = 1;
    }
    LastChar = getc(pFile);
    columnNo++;
  }

  if (isalpha(LastChar) ||
      (LastChar == '_')) { // identifier: [a-zA-Z_][a-zA-Z_0-9]*
    IdentifierStr = LastChar;
    columnNo++;

    while (isalnum((LastChar = getc(pFile))) || (LastChar == '_')) {
      IdentifierStr += LastChar;
      columnNo++;
    }

    if (IdentifierStr == "int")
      return returnTok("int", INT_TOK);
    if (IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if (IdentifierStr == "float")
      return returnTok("float", FLOAT_TOK);
    if (IdentifierStr == "void")
      return returnTok("void", VOID_TOK);
    if (IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if (IdentifierStr == "extern")
      return returnTok("extern", EXTERN);
    if (IdentifierStr == "if")
      return returnTok("if", IF);
    if (IdentifierStr == "else")
      return returnTok("else", ELSE);
    if (IdentifierStr == "while")
      return returnTok("while", WHILE);
    if (IdentifierStr == "return")
      return returnTok("return", RETURN);
    if (IdentifierStr == "true") {
      BoolVal = true;
      return returnTok("true", BOOL_LIT);
    }
    if (IdentifierStr == "false") {
      BoolVal = false;
      return returnTok("false", BOOL_LIT);
    }

    return returnTok(IdentifierStr.c_str(), IDENT);
  }

  if (LastChar == '=') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // EQ: ==
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("==", EQ);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("=", ASSIGN);
    }
  }

  if (LastChar == '{') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("{", LBRA);
  }
  if (LastChar == '}') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("}", RBRA);
  }
  if (LastChar == '(') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("(", LPAR);
  }
  if (LastChar == ')') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(")", RPAR);
  }
  if (LastChar == ';') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(";", SC);
  }
  if (LastChar == ',') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(",", COMMA);
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9]+.
    std::string NumStr;

    if (LastChar == '.') { // Floatingpoint Number: .[0-9]+
      do {
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      FloatVal = strtof(NumStr.c_str(), nullptr);
      return returnTok(NumStr, FLOAT_LIT);
    } else {
      do { // Start of Number: [0-9]+
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      if (LastChar == '.') { // Floatingpoint Number: [0-9]+.[0-9]+)
        do {
          NumStr += LastChar;
          LastChar = getc(pFile);
          columnNo++;
        } while (isdigit(LastChar));

        FloatVal = strtof(NumStr.c_str(), nullptr);
        return returnTok(NumStr, FLOAT_LIT);
      } else { // Integer : [0-9]+
        IntVal = strtod(NumStr.c_str(), nullptr);
        return returnTok(NumStr, INT_LIT);
      }
    }
  }

  if (LastChar == '&') {
    NextChar = getc(pFile);
    if (NextChar == '&') { // AND: &&
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("&&", AND);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("&", int('&'));
    }
  }

  if (LastChar == '|') {
    NextChar = getc(pFile);
    if (NextChar == '|') { // OR: ||
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("||", OR);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("|", int('|'));
    }
  }

  if (LastChar == '!') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // NE: !=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("!=", NE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("!", NOT);
      ;
    }
  }

  if (LastChar == '<') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // LE: <=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("<=", LE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("<", LT);
    }
  }

  if (LastChar == '>') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // GE: >=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok(">=", GE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok(">", GT);
    }
  }

  if (LastChar == '/') { // could be division or could be the start of a comment
    LastChar = getc(pFile);
    columnNo++;
    if (LastChar == '/') { // definitely a comment
      do {
        LastChar = getc(pFile);
        columnNo++;
      } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

      if (LastChar != EOF)
        return gettok();
    } else
      return returnTok("/", DIV);
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF) {
    columnNo++;
    return returnTok("0", EOF_TOK);
  }

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  std::string s(1, ThisChar);
  LastChar = getc(pFile);
  columnNo++;
  return returnTok(s, int(ThisChar));
}

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static TOKEN CurTok;
static std::deque<TOKEN> tok_buffer;

static TOKEN getNextToken() {

  if (tok_buffer.size() == 0)
    tok_buffer.push_back(gettok());

  TOKEN temp = tok_buffer.front();
  tok_buffer.pop_front();

  return CurTok = temp;
}

static void putBackToken(TOKEN tok) { tok_buffer.push_front(tok); }


static AllocaInst* CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName, int type) {

  IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());

  switch(type){
    case INT_TOK:
      return TmpB.CreateAlloca(Type::getInt32Ty(TheContext), 0, VarName.c_str());
    case BOOL_TOK:
      return TmpB.CreateAlloca(Type::getInt1Ty(TheContext), 0, VarName.c_str());
    case FLOAT_TOK:
      return TmpB.CreateAlloca(Type::getFloatTy(TheContext), 0, VarName.c_str());
  }

}

static int CheckScope(std::string name) {
  int scope = NamedValues.size()-1;
    while(NamedValues[scope].find(name) == NamedValues[scope].end() && scope>0){
       scope--;
    }
    if(NamedValues[scope].find(name) == NamedValues[scope].end()){
      if(GlobalValues.find(name) != GlobalValues.end()) return -1;
    }
    return scope;
    // for(int i = NamedValues.size()-1; i>=0;i--){  
    //   if(NamedValues[i].count(name) > 0) return i;
    // }
}


//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// AST_node - Base class for all AST nodes.
class AST_node {
public:
  virtual ~AST_node() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string(int t) const {};
};

class ParamAST : public AST_node {
  
public:
  TOKEN Type, Name;
  ParamAST(TOKEN type, TOKEN name) : Type(type), Name(name) {}

  Value *codegen() override {
    return nullptr;
  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    return tabs + "--| PARAM: " + Name.lexeme + "\n  " + tabs + "--| TYPE: " + Type.lexeme;
  }
};

class ParamListAST : public AST_node {
public:
  std::vector<std::unique_ptr<ParamAST>> Params;
  ParamListAST(std::vector<std::unique_ptr<ParamAST>> params) : Params(std::move(params)) {}
  Value *codegen() override {
    return nullptr;
  }
  std::string to_string(int t) const override {
    std::string res = "";
    for( auto& n : Params){
        res+="\n" + n->to_string(t);
    }
    return res;
  }
};

class ExternAST : public AST_node {
  TOKEN Type, ID;
  std::unique_ptr<ParamListAST> Params;

public:
  ExternAST(TOKEN type, TOKEN id, std::unique_ptr<ParamListAST> params) 
  : Type(type), ID(id), Params(std::move(params)) {}

  Value *codegen() override {
    std::vector<llvm::Type *> Ints(Params->Params.size(), Type::getInt32Ty(TheContext));
    FunctionType *FT = FunctionType::get(Type::getInt32Ty(TheContext), Ints, false);

    Function *F = Function::Create(FT, Function::ExternalLinkage, ID.lexeme, TheModule.get());
    unsigned Idx = 0;
    for (auto &Arg : F->args())
      Arg.setName(Params->Params[Idx++]->Name.lexeme);
    return F;
  };
  
  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    return tabs + "--| EXTERN: " + ID.lexeme + Params->to_string(t+1);
  }
};

class ExternListAST : public AST_node {
  std::vector<std::unique_ptr<ExternAST>> Externs;

public:
  ExternListAST(std::vector<std::unique_ptr<ExternAST>> externs) : Externs(std::move(externs)) {}

  Value *codegen() override {
    for( auto& n : Externs){
      n->codegen();
    }
    return nullptr;
  }

  std::string to_string(int t) const override {
    std::string res = "";
    for( auto& n : Externs){
        res+="\n" + n->to_string(t+1);
    }
    return res;
  }
};

class DeclListAST : public AST_node {
  std::vector<std::unique_ptr<AST_node>> Decls;

public:
  DeclListAST(std::vector<std::unique_ptr<AST_node>> decls) : Decls(std::move(decls)) {}

  Value *codegen() override {
    for( auto& n : Decls){
        n->codegen();
    }
    return nullptr;
  }

  std::string to_string(int t) const override {
    std::string res = "\n";
    for( auto& n : Decls){
        res+=n->to_string(t+1) + "\n";
    }
    return res;
  }
};

class ProgramAST : public AST_node {
  std::unique_ptr<ExternListAST> Externs;
  std::unique_ptr<DeclListAST> Decls;

public:
  ProgramAST(std::unique_ptr<ExternListAST> externs, std::unique_ptr<DeclListAST> decls) 
  : Externs(std::move(externs)), Decls(std::move(decls)) {}

  Value *codegen() override {
    if (Externs) Externs->codegen();
    Decls->codegen();
    return nullptr;
  };

  std::string to_string(int t) const override {
    std::string res = "--| PROGRAM";
    if(Externs != nullptr) res+= Externs->to_string(t+1);
    res+= Decls->to_string(t+1);
    return res;
  };
};

class IDENTnode : public AST_node {

public:
  TOKEN Tok;
  IDENTnode(TOKEN tok) : Tok(tok) {}

  Value *codegen() override {
    std::string name = Tok.lexeme;
    int scope = CheckScope(name);
    if(scope == -1){
      auto g = GlobalValues[name];
      return Builder.CreateLoad(g->getValueType(),g,name);
    }
    if(NamedValues[scope].find(name) != NamedValues[scope].end()){
      auto alloca = NamedValues[scope][name];
      
      return Builder.CreateLoad(alloca->getAllocatedType(),alloca,name);
    }
  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    return tabs + "--| IDENT: " + Tok.lexeme;
  }
};

class AssignmentAST : public AST_node {
  std::unique_ptr<IDENTnode> Name;
  std::unique_ptr<AST_node> Expr;

public:
  AssignmentAST(std::unique_ptr<IDENTnode> name, std::unique_ptr<AST_node> expr) 
  : Name(std::move(name)), Expr(std::move(expr)) {}

  Value *codegen() override {

    auto val = Expr->codegen();

    int scope = CheckScope(Name->Tok.lexeme);

    if(scope == -1){
      auto g = GlobalValues[Name->Tok.lexeme];
      Builder.CreateStore(val, g);
      return nullptr;
    }

    auto alloca = NamedValues[scope][Name->Tok.lexeme];

    Builder.CreateStore(val, alloca);

    NamedValues[scope][Name->Tok.lexeme] = alloca;

    return nullptr;

  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    return tabs + "--| VARIABLE ASSIGNMENT: " + Name->to_string(0) + "\n" + Expr->to_string(t+1);
  }
};

class BinaryExprAST : public AST_node {
  TOKEN Op;
  std::unique_ptr<AST_node> LHS, RHS;

public:
  BinaryExprAST(TOKEN op, std::unique_ptr<AST_node> lhs, 
  std::unique_ptr<AST_node> rhs) 
  : Op(op), LHS(std::move(lhs)), RHS(std::move(rhs)) {}

  Value *codegen() override {
    Value * left = LHS->codegen(); 
    Value * right = RHS->codegen(); 
    switch(Op.type){
      case PLUS:{
        if(left->getType()->isFloatTy() && right->getType()->isFloatTy()){
          Value* add = Builder.CreateFAdd(left, right, "addtmp");
          return add;
        }
        else if(left->getType()->isFloatTy()) {
          auto rightF = Builder.CreateSIToFP(right, Type::getFloatTy(TheContext), "inttofp");
          Value* add = Builder.CreateFAdd(left, rightF, "addtmp");
          return add;
        }
        else if(right->getType()->isFloatTy()) {
          auto leftF = Builder.CreateSIToFP(left, Type::getFloatTy(TheContext), "inttofp");
          Value* add = Builder.CreateFAdd(leftF, right, "addtmp");
          return add;
        }
        else {
          Value* add = Builder.CreateAdd(left, right, "addtmp");
          return add;
        }
      }

      case MINUS:{
        if(left->getType()->isFloatTy() && right->getType()->isFloatTy()){
          Value* sub = Builder.CreateFSub(left, right, "subtmp");
          return sub;
        }
        else if(left->getType()->isFloatTy()) {
          auto rightF = Builder.CreateSIToFP(right, Type::getFloatTy(TheContext), "inttofp");
          Value* sub = Builder.CreateFSub(left, rightF, "subtmp");
          return sub;
        }
        else if(right->getType()->isFloatTy()) {
          auto leftF = Builder.CreateSIToFP(left, Type::getFloatTy(TheContext), "inttofp");
          Value* sub = Builder.CreateFSub(leftF, right, "subtmp");
          return sub;
        }
        else {
          Value* sub = Builder.CreateSub(left, right, "subtmp");
          return sub;
        }
      }

      case ASTERIX:{
        if(left->getType()->isFloatTy() && right->getType()->isFloatTy()){
          Value* mul = Builder.CreateFMul(left, right, "multmp");
          return mul;
        }
        else if(left->getType()->isFloatTy()) {
          auto rightF = Builder.CreateSIToFP(right, Type::getFloatTy(TheContext),"inttofp");
          Value* mul = Builder.CreateFMul(left, rightF, "multmp");
          return mul;
        }
        else if(right->getType()->isFloatTy()){
          auto leftF = Builder.CreateSIToFP(left, Type::getFloatTy(TheContext), "inttofp");
          Value* mul = Builder.CreateFMul(leftF, right, "multmp");
          return mul;
        }
        else {
          Value* mul = Builder.CreateMul(left, right, "multmp");
          return mul;
        }
      }

      case DIV:{
        if(left->getType()->isFloatTy() && right->getType()->isFloatTy()){
          Value* div = Builder.CreateFDiv(left, right, "divtmp");
          return div;
        }
        else if(left->getType()->isFloatTy()) {
          auto rightF = Builder.CreateSIToFP(right, Type::getFloatTy(TheContext), "inttofp");
          Value* div = Builder.CreateFDiv(left, rightF, "divtmp");
          return div;
        }
        else if(right->getType()->isFloatTy()){
          auto leftF = Builder.CreateSIToFP(left, Type::getFloatTy(TheContext), "inttofp");
          Value* div = Builder.CreateFDiv(leftF, right, "divtmp");
          return div;
        }
        else {
          Value* div = Builder.CreateSDiv(left, right, "divtmp");
          return div;
        }
      }

      case MOD:{
        if(left->getType()->isFloatTy() && right->getType()->isFloatTy()){
          Value* rem = Builder.CreateFRem(left, right, "remtmp");
          return rem;
        }
        else if(left->getType()->isFloatTy()) {
          auto rightF = Builder.CreateSIToFP(right, Type::getFloatTy(TheContext), "inttofp");
          Value* rem = Builder.CreateFRem(left, rightF, "remtmp");
          return rem;
        }
        else if(right->getType()->isFloatTy()){
          auto leftF = Builder.CreateSIToFP(left, Type::getFloatTy(TheContext), "inttofp");
          Value* rem = Builder.CreateFRem(leftF, right, "remtmp");
          return rem;
        }
        else {
          Value* rem = Builder.CreateSRem(left, right, "remtmp");
          return rem;
        }
      }

      case EQ:{
        if(left->getType()->isFloatTy() && right->getType()->isFloatTy()){
          Value* eq = Builder.CreateFCmpOEQ(left, right, "eqtmp");
          return eq;
        }
        else if(left->getType()->isFloatTy()){
          auto rightF = Builder.CreateSIToFP(right, Type::getFloatTy(TheContext), "inttofp");
          Value* eq = Builder.CreateFCmpOEQ(left, rightF, "eqtmp");
          return eq;
        }
        else if(right->getType()->isFloatTy()){
          auto leftF = Builder.CreateSIToFP(left, Type::getFloatTy(TheContext), "inttofp");
          Value* eq = Builder.CreateFCmpOEQ(leftF, right, "eqtmp");
          return eq;
        }
        else {
          Value* eq = Builder.CreateICmpEQ(left, right, "eqtmp");
          return eq;
        }
      }
      case NE:{
        if(left->getType()->isFloatTy() && right->getType()->isFloatTy()){
          Value* ne = Builder.CreateFCmpONE(left, right, "netmp");
          return ne;
        }
        else if(left->getType()->isFloatTy()){
          auto rightF = Builder.CreateSIToFP(right, Type::getFloatTy(TheContext), "inttofp");
          Value* ne = Builder.CreateFCmpONE(left, rightF, "netmp");
          return ne;
        }
        else if(right->getType()->isFloatTy()){
          auto leftF = Builder.CreateSIToFP(left, Type::getFloatTy(TheContext), "inttofp");
          Value* ne = Builder.CreateFCmpONE(leftF, right, "netmp");
          return ne;
        }
        else {
          Value* ne = Builder.CreateICmpNE(left, right, "netmp");
          return ne;
        }
      }
      case LE:{
        if(left->getType()->isFloatTy() && right->getType()->isFloatTy()){
          Value* le = Builder.CreateFCmpOLE(left, right, "letmp");
          return le;
        }
        else if(left->getType()->isFloatTy()){
          auto rightF = Builder.CreateSIToFP(right, Type::getFloatTy(TheContext), "inttofp");
          Value* le = Builder.CreateFCmpOLE(left, rightF, "letmp");
          return le;
        }
        else if(right->getType()->isFloatTy()){
          auto leftF = Builder.CreateSIToFP(left, Type::getFloatTy(TheContext), "inttofp");
          Value* le = Builder.CreateFCmpOLE(leftF, right, "letmp");
          return le;
        }
        else {
          Value* le = Builder.CreateICmpSLE(left, right, "letmp");
          return le;
        }
      }
      case LT:{
        if(left->getType()->isFloatTy() && right->getType()->isFloatTy()){
          Value* lt = Builder.CreateFCmpOLT(left, right, "lttmp");
          return lt;
        }
        else if(left->getType()->isFloatTy()){
          auto rightF = Builder.CreateSIToFP(right, Type::getFloatTy(TheContext), "inttofp");
          Value* lt = Builder.CreateFCmpOLT(left, rightF, "lttmp");
          return lt;
        }
        else if(right->getType()->isFloatTy()){
          auto leftF = Builder.CreateSIToFP(left, Type::getFloatTy(TheContext), "inttofp");
          Value* lt = Builder.CreateFCmpOLT(leftF, right, "lttmp");
          return lt;
        }
        else {
          Value* lt = Builder.CreateICmpSLT(left, right, "lttmp");
          return lt;
        }
      }
      case GE:{
        if(left->getType()->isFloatTy() && right->getType()->isFloatTy()){
          Value* ge = Builder.CreateFCmpOGE(left, right, "getmp");
          return ge;
        }
        else if(left->getType()->isFloatTy()){
          auto rightF = Builder.CreateSIToFP(right, Type::getFloatTy(TheContext), "inttofp");
          Value* ge = Builder.CreateFCmpOGE(left, rightF, "getmp");
          return ge;
        }
        else if(right->getType()->isFloatTy()){
          auto leftF = Builder.CreateSIToFP(left, Type::getFloatTy(TheContext), "inttofp");
          Value* ge = Builder.CreateFCmpOGE(leftF, right, "getmp");
          return ge;
        }
        else {
          Value* ge = Builder.CreateICmpSGE(left, right, "getmp");
          return ge;
        }
      }
      case GT:{
        if(left->getType()->isFloatTy() && right->getType()->isFloatTy()){
          Value* gt = Builder.CreateFCmpOGT(left, right, "gttmp");
          return gt;
        }
        else if(left->getType()->isFloatTy()){
          auto rightF = Builder.CreateSIToFP(right, Type::getFloatTy(TheContext), "inttofp");
          Value* gt = Builder.CreateFCmpOGT(left, rightF, "gttmp");
          return gt;
        }
        else if(right->getType()->isFloatTy()){
          auto leftF = Builder.CreateSIToFP(left, Type::getFloatTy(TheContext), "inttofp");
          Value* gt = Builder.CreateFCmpOGT(leftF, right, "gttmp");
          return gt;
        }
        else {
          Value* gt = Builder.CreateICmpSGT(left, right, "gttmp");
          return gt;
        }
      }
      case AND:{
        Value* leftB;
        Value* rightB;
        if(left->getType()->isFloatTy()) leftB = Builder.CreateFPToUI(left, Type::getInt1Ty(TheContext));
        if(left->getType()->isIntegerTy(32)) leftB = Builder.CreateICmpNE(left, ConstantInt::get(TheContext, APInt(1,0,false)), "inttobool");
        if(right->getType()->isFloatTy()) rightB = Builder.CreateFPToUI(right, Type::getInt1Ty(TheContext));
        if(right->getType()->isIntegerTy(32)) rightB = Builder.CreateICmpNE(right, ConstantInt::get(TheContext, APInt(1,0,false)), "inttobool");
        Value* an = Builder.CreateAnd(leftB, rightB, "andtmp");
      }
      case OR:{
        Value* leftB;
        Value* rightB;
        if(left->getType()->isFloatTy()) leftB = Builder.CreateFPToUI(left, Type::getInt1Ty(TheContext));
        if(left->getType()->isIntegerTy(32)) leftB = Builder.CreateICmpNE(left, ConstantInt::get(TheContext, APInt(1,0,false)), "inttobool");
        if(right->getType()->isFloatTy()) rightB = Builder.CreateFPToUI(right, Type::getInt1Ty(TheContext));
        if(right->getType()->isIntegerTy(32)) rightB = Builder.CreateICmpNE(right, ConstantInt::get(TheContext, APInt(1,0,false)), "inttobool");
        Value* an = Builder.CreateOr(leftB, rightB, "ortmp");
      }
    }
    return nullptr;
  }
    
  
  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    return tabs + "--| BINOP: " + Op.lexeme + "\n" + LHS->to_string(t+1) + "\n" + RHS->to_string(t+1);
  }
};

class BlockAST : public AST_node {
  std::unique_ptr<AST_node> Decls, Stmts;

public:
  BlockAST(std::unique_ptr<AST_node> decls, std::unique_ptr<AST_node> stmts) 
  : Decls(std::move(decls)), Stmts(std::move(stmts)) {}

  Value *codegen() override {
    std::map<std::string, AllocaInst*> newScope;
    NamedValues.push_back(newScope);
    if(Decls != nullptr) Decls->codegen();
    Stmts->codegen();
    NamedValues.pop_back();
    return nullptr;
  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    std::string res = "";
    res+= tabs + "--| BLOCK:";
    if(Decls != nullptr) res+= Decls->to_string(t+1);
    res+= Stmts->to_string(t+1);
    return res;
  }
};

class WhileAST : public AST_node {
  std::unique_ptr<AST_node> Con, Exe;

public:
  WhileAST(std::unique_ptr<AST_node> con, std::unique_ptr<AST_node> exe) 
  : Con(std::move(con)), Exe(std::move(exe)) {}

  Value *codegen() override {
    Function* TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock* before_ = BasicBlock::Create(TheContext, "before", TheFunction);
    BasicBlock*   loop_ = BasicBlock::Create(TheContext, "loop");
    BasicBlock*    end_ = BasicBlock::Create(TheContext, "end");

    Builder.CreateBr(before_);
    Builder.SetInsertPoint(before_);

    Value* cond = Con->codegen();
    Value* comp = Builder.CreateICmpNE(
      cond, ConstantInt::get(TheContext, APInt(1,0,false)), "whilecond");

    Builder.CreateCondBr(comp, loop_, end_);

    TheFunction->getBasicBlockList().push_back(loop_);
    Builder.SetInsertPoint(loop_);
    Exe->codegen();
    Builder.CreateBr(before_);

    Builder.CreateBr(end_);
    TheFunction->getBasicBlockList().push_back(end_);
    Builder.SetInsertPoint(end_);

    return nullptr;
  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    std::string res = "";
    res+= tabs + "--| WHILE:\n" + Con->to_string(t+1) + "\n" + Exe->to_string(t+1);
    return res;
  }
};

class IfAST : public AST_node {
  std::unique_ptr<AST_node> Con, Exe, Else_st;

public:
  IfAST(std::unique_ptr<AST_node> con, std::unique_ptr<AST_node> exe, std::unique_ptr<AST_node> else_st) 
  : Con(std::move(con)), Exe(std::move(exe)), Else_st(std::move(else_st)) {}

  Value *codegen() override {
    Function* TheFunction = Builder.GetInsertBlock()->getParent();
    if(Else_st == nullptr) {
      BasicBlock* true_ = BasicBlock::Create(TheContext, "then", TheFunction);
      BasicBlock*  end_ = BasicBlock::Create(TheContext, "end");
      Value* cond = Con->codegen();
      Value* comp = Builder.CreateICmpNE(
        cond, ConstantInt::get(TheContext, APInt(1,0,false)), "ifcond");
      Builder.CreateCondBr(comp, true_, end_);

      Builder.SetInsertPoint(true_);
      Exe->codegen();
      
      TheFunction->getBasicBlockList().push_back(end_);

      Builder.CreateBr(end_);
      Builder.SetInsertPoint(end_);
    }
    else {
      BasicBlock*  true_ = BasicBlock::Create(TheContext, "then", TheFunction);
      BasicBlock* false_ = BasicBlock::Create(TheContext, "else then");
      BasicBlock*  end_ = BasicBlock::Create(TheContext, "end");
      Value* cond = Con->codegen();
      Value* comp = Builder.CreateICmpNE(
        cond, ConstantInt::get(TheContext, APInt(1,0,false)), "ifcond");
      Builder.CreateCondBr(comp, true_, false_);

      Builder.SetInsertPoint(true_);
      Exe->codegen();
      TheFunction->getBasicBlockList().push_back(false_);
      Builder.CreateBr(end_);

      Builder.SetInsertPoint(false_);
      Else_st->codegen();
      TheFunction->getBasicBlockList().push_back(end_);
      Builder.CreateBr(end_);
      Builder.SetInsertPoint(end_);

    }

    return nullptr;
  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    std::string res = "";
    res+= tabs + "--| IF:\n" + Con->to_string(t+1) + "\n" + Exe->to_string(t+1);
    if(Else_st != nullptr) res+= Else_st->to_string(t+1);
    return res;
  }
};

class ElseAST : public AST_node {
  std::unique_ptr<AST_node> Body;

public:
  ElseAST(std::unique_ptr<AST_node> body) : Body(std::move(body)) {}

  Value *codegen() override {
    return Body->codegen();
  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    return tabs + "--| ELSE:\n" + Body->to_string(t);
  }
};

class ReturnAST : public AST_node {
  std::unique_ptr<AST_node> Body;

public:
  ReturnAST(std::unique_ptr<AST_node> body) : Body(std::move(body)) {}

  Value *codegen() override {
    if(!Body) {
      Builder.CreateRetVoid();
      return nullptr;
    }
    auto result = Body->codegen();
    return Builder.CreateRet(result);
  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    if(Body != nullptr) return tabs + "--| RETURN:\n" + Body->to_string(t+1);
    return "";
  }
};

class ArgListAST : public AST_node {
public:
  std::vector<std::unique_ptr<AST_node>> Args;
  ArgListAST(std::vector<std::unique_ptr<AST_node>> args) : Args(std::move(args)) {}

  Value *codegen() override {
    for( auto& n : Args){
        n->codegen();
    }
    return nullptr;
  }

  std::string to_string(int t) const override {
    std::string res = "";
    for( auto& n : Args){
        res+=n->to_string(t+1) + " ";
    }
    return res;
  }
};

class FunctionCallAST : public AST_node {
  std::unique_ptr<IDENTnode> Name;
  std::unique_ptr<ArgListAST> Arguments;

public:
  FunctionCallAST(std::unique_ptr<IDENTnode> name, std::unique_ptr<ArgListAST> arguments) 
  : Name(std::move(name)), Arguments(std::move(arguments)) {}

  Value *codegen() override {
    Function *CalleeF = TheModule->getFunction(Name->Tok.lexeme);

    std::vector<Value *> ArgsV;
    for (unsigned i = 0, e = Arguments->Args.size(); i != e; ++i) {
      ArgsV.push_back(Arguments->Args[i]->codegen());
      if (!ArgsV.back())
      return nullptr;
    }

  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");

  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    return tabs + "--| FUNCTION CALL: " + Name->to_string(0) + "\n" + Arguments->to_string(t+1);
  }
};

class StmtListAST : public AST_node {
  std::vector<std::unique_ptr<AST_node>> Stmts;

public:
  StmtListAST(std::vector<std::unique_ptr<AST_node>> stmts) : Stmts(std::move(stmts)) {}

  Value *codegen() override {
    for( auto& n : Stmts){
        n->codegen();
    }
    return nullptr;
  }

  std::string to_string(int t) const override {
    std::string res = "\n";
    for( auto& n : Stmts){
        res+=n->to_string(t+1) + "\n";
    }
    return res;
  }
};

class LocalDeclListAST : public AST_node {
  std::vector<std::unique_ptr<AST_node>> Decls;

public:
  LocalDeclListAST(std::vector<std::unique_ptr<AST_node>> decls) : Decls(std::move(decls)) {}

  Value *codegen() override {
    for( auto& n : Decls){
        n->codegen();
    }
    return nullptr;
  }

  std::string to_string(int t) const override {
    std::string res = "\n";
    for( auto& n : Decls){
        res+=n->to_string(t+1) + "\n";
    }
    return res;
  }
};

class VariableDeclarationAST : public AST_node {
  TOKEN Type, Name;

public:
  VariableDeclarationAST(TOKEN type, TOKEN name) : Type(type), Name(name) {}

  Value *codegen() override {

    if(NamedValues.size()==0){
      if(Type.type == INT_TOK) {
        GlobalVariable* g = new GlobalVariable(*(TheModule.get()), Type::getInt32Ty(TheContext), false, GlobalValue::CommonLinkage,Constant::getNullValue(Type::getInt32Ty(TheContext)));
        GlobalValues[Name.lexeme] = g;
      }
      if(Type.type == FLOAT_TOK){ 
        GlobalVariable* g = new GlobalVariable(*(TheModule.get()), Type::getFloatTy(TheContext), false, GlobalValue::CommonLinkage,Constant::getNullValue(Type::getFloatTy(TheContext)));
        GlobalValues[Name.lexeme] = g;
      }
      if(Type.type == BOOL_TOK) {
        GlobalVariable* g = new GlobalVariable(*(TheModule.get()), Type::getInt1Ty(TheContext), false, GlobalValue::CommonLinkage,Constant::getNullValue(Type::getInt1Ty(TheContext)));
        GlobalValues[Name.lexeme] = g;
      }
    }
    else {
      auto parentF = Builder.GetInsertBlock()->getParent();

      auto varAlloca = CreateEntryBlockAlloca(parentF, Name.lexeme, Type.type);
      //Builder.CreateStore(Constant::getNullValue(Type::getInt32Ty(TheContext)),varAlloca);
      NamedValues[NamedValues.size()-1][Name.lexeme] = varAlloca;
    }
    return nullptr;
  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    return tabs + "--| VARIABLE DECLARATION: " + Name.lexeme + "\n  " + tabs + "--| TYPE: " + Type.lexeme;
  }
};

class FunctionDeclarationAST : public AST_node {
  TOKEN Type, ID;
  std::unique_ptr<AST_node> Block;

public:
  std::unique_ptr<ParamListAST> Params;
  FunctionDeclarationAST(TOKEN type, TOKEN id, std::unique_ptr<ParamListAST> params, std::unique_ptr<AST_node> block) 
  : Type(type), ID(id), Params(std::move(params)), Block(std::move(block)) {}
  
  Value *codegen() override {
    std::map<std::string, AllocaInst*> newScope;
    NamedValues.push_back(newScope);
    // std::vector<llvm::Type *> Ints(Params->Params.size(), Type::getInt32Ty(TheContext));

    std::vector<llvm::Type *> Types;
    if(Params != nullptr){
      for(int i=0;i<Params->Params.size();i++){
        auto typeP = Params->Params[i]->Type.type;
        switch(typeP){
          case INT_TOK:{
            Types.push_back(Type::getInt32Ty(TheContext));
            break;
          }
          case FLOAT_TOK:{
            Types.push_back(Type::getFloatTy(TheContext));
            break;
          }
          case BOOL_TOK:{
            Types.push_back(Type::getInt1Ty(TheContext));
            break;
          }
        }
      }
    }

    FunctionType *FT;

    if(Type.type == INT_TOK)        FT = FunctionType::get(Type::getInt32Ty(TheContext), Types, false);
    else if(Type.type == FLOAT_TOK) FT = FunctionType::get(Type::getFloatTy(TheContext), Types, false);
    else if(Type.type == BOOL_TOK)  FT = FunctionType::get(Type::getInt1Ty(TheContext), Types, false);
    else if(Type.type == VOID_TOK)  FT = FunctionType::get(Type::getVoidTy(TheContext), Types, false);
    else {
      fprintf(stderr, "Invalid type\n");
      exit(0);
    }


    Function *TheFunction = Function::Create(FT, Function::ExternalLinkage, ID.lexeme, TheModule.get());
    unsigned Idx = 0;
    for (auto &Arg : TheFunction->args()){
      Arg.setName(Params->Params[Idx++]->Name.lexeme);
      }

    BasicBlock *BBEntry = BasicBlock::Create(TheContext, "entry", TheFunction);
    Builder.SetInsertPoint(BBEntry);


    Idx = 0;
    for (auto &Arg : TheFunction->args()) {

      AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Params->Params[Idx]->Name.lexeme, Params->Params[Idx]->Type.type);
      Idx++;
      Builder.CreateStore(&Arg, Alloca);
      NamedValues[0][std::string(Arg.getName())] = Alloca;
    }

    // if (Value *RetVal = Block->codegen()) {
    //   Builder.CreateRet(RetVal);
    // }

    Block->codegen();

    if(Type.type == INT_TOK)        Builder.CreateRet(Constant::getNullValue(Type::getInt32Ty(TheContext))); // generate default return
    else if(Type.type == FLOAT_TOK) Builder.CreateRet(Constant::getNullValue(Type::getFloatTy(TheContext))); // generate default return
    else if(Type.type == BOOL_TOK)  Builder.CreateRet(Constant::getNullValue(Type::getInt1Ty(TheContext))); // generate default return
    

    verifyFunction(*TheFunction);

    NamedValues.pop_back();
    return TheFunction;
  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    std::string res = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    res+= tabs + "--| FUNCTION DECLARATION: " + ID.lexeme + "\n  " + tabs + "--| TYPE: " + Type.lexeme;
    if(Params != nullptr) res+= Params->to_string(t+1);
    res+= "\n" + Block->to_string(t+1);
    return res;
  }
};

class NegatedValAST : public AST_node {
  TOKEN Negation;
  std::unique_ptr<AST_node> Val;

public:
  NegatedValAST(TOKEN negation, std::unique_ptr<AST_node> val) : Negation(negation), Val(std::move(val)) {}

  Value *codegen() override {
    Value * val = Val->codegen();
    switch(Negation.type){
      case NOT:{
        if(val->getType()->isIntegerTy(1)){
          auto newval = Builder.CreateNot(val);
          return newval;
        }
      }
      case MINUS:{
        if(val->getType()->isIntegerTy(32)){
          auto newval = Builder.CreateNeg(val);
          return newval;
        }
        if(val->getType()->isFloatTy()){
          auto newval = Builder.CreateFNeg(val);
          return newval;
        }
        return nullptr;
      }
    }
  }
  
  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    return tabs + Negation.lexeme + Val->to_string(0);
  }
};

class INT_LITnode : public AST_node {
  TOKEN Tok;

public:
  INT_LITnode(TOKEN tok) : Tok(tok) {}

  Value *codegen() override {
    Value* number = ConstantInt::get(TheContext, APInt(32,std::stoi(Tok.lexeme),true));
    return number;
  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    return tabs + "--| INT: " + Tok.lexeme;
  }
};

class FLOAT_LITnode : public AST_node {
  TOKEN Tok;

public:
  FLOAT_LITnode(TOKEN tok) : Tok(tok) {}

  Value *codegen() override {
    Value* number = ConstantFP::get(TheContext, APFloat(std::stof(Tok.lexeme)));
    return number;
  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    return tabs + "--| FLOAT: " + Tok.lexeme;
  }
};

class BOOL_LITnode : public AST_node {
  TOKEN Tok;

public:
  BOOL_LITnode(TOKEN tok) : Tok(tok) {}

  Value *codegen() override {
    int trueorfalse;
    if(Tok.lexeme == "true") trueorfalse=1;
    else trueorfalse=0;
    Value* number = ConstantInt::get(TheContext, APInt(1,trueorfalse,true));
    return number;
  }

  std::string to_string(int t) const override {
    std::string tabs = "";
    for(int i=0;i<t;i++){
      tabs+="  ";
    }
    return tabs + "--| BOOL: " + Tok.lexeme;
  }
};


/* add other AST nodes as nessasary */

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//
/* Add function calls for each production */
// program ::= extern_list decl_list
static std::unique_ptr<ProgramAST> ParseProgram();
static std::unique_ptr<ExternListAST> ParseExternList();  
static std::unique_ptr<ExternAST> ParseExtern();
static std::unique_ptr<DeclListAST> ParseDeclList();
static std::unique_ptr<AST_node> ParseDecl();
static std::unique_ptr<AST_node> ParseFunDecl();
static std::unique_ptr<ParamListAST> ParseParams();
static std::unique_ptr<ParamListAST> ParseParamList();
static std::unique_ptr<ParamAST> ParseParam();
static std::unique_ptr<AST_node> ParseLocalDecls();
static std::unique_ptr<AST_node> ParseLocalDecl();
static std::unique_ptr<AST_node> ParseStmtList();
static std::unique_ptr<AST_node> ParseStmt();
static std::unique_ptr<AST_node> ParseExprStmt();
static std::unique_ptr<AST_node> ParseWhile();
static std::unique_ptr<AST_node> ParseIf();
static std::unique_ptr<AST_node> ParseElse();
static std::unique_ptr<AST_node> ParseReturn();
static std::unique_ptr<AST_node> ParseBlock();
static std::unique_ptr<AST_node> ParseExpr();
static std::unique_ptr<AST_node> ParseRVAL();
static std::unique_ptr<AST_node> ParseRVAL_Prime(std::unique_ptr<AST_node> LHS);
static std::unique_ptr<AST_node> ParseR1VAL();
static std::unique_ptr<AST_node> ParseR1VAL_Prime(std::unique_ptr<AST_node> LHS);
static std::unique_ptr<AST_node> ParseR2VAL();
static std::unique_ptr<AST_node> ParseR2VAL_Prime(std::unique_ptr<AST_node> LHS);
static std::unique_ptr<AST_node> ParseR3VAL();
static std::unique_ptr<AST_node> ParseR3VAL_Prime(std::unique_ptr<AST_node> LHS);
static std::unique_ptr<AST_node> ParseR4VAL();
static std::unique_ptr<AST_node> ParseR4VAL_Prime(std::unique_ptr<AST_node> LHS);
static std::unique_ptr<AST_node> ParseR5VAL();
static std::unique_ptr<AST_node> ParseR5VAL_Prime(std::unique_ptr<AST_node> LHS);
static std::unique_ptr<AST_node> ParseR6VAL();
static std::unique_ptr<ArgListAST> ParseArgs();
static std::unique_ptr<ArgListAST> ParseArgList();
static std::unique_ptr<AST_node> ParseINT_LIT();
static std::unique_ptr<AST_node> ParseFLOAT_LIT();
static std::unique_ptr<AST_node> ParseBOOL_LIT();
static std::unique_ptr<AST_node> ParseIDENT();

static std::unique_ptr<ProgramAST> ParseProgram() {
  if(CurTok.type == EXTERN){
    auto externs = ParseExternList();
    auto decls = ParseDeclList();
    return std::make_unique<ProgramAST>(std::move(externs), std::move(decls));
  }
  else if(CurTok.type == VOID_TOK || CurTok.type == INT_TOK || 
    CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) {
    auto Result = ParseDeclList();
    return std::make_unique<ProgramAST>(nullptr, std::move(Result));
  }
  else {
    fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected tokens: %s, %s, %s, %s, %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo ,"extern", "int", "float", "bool", "void", CurTok.lexeme.c_str());
    exit(0);
  }
}

static std::unique_ptr<ExternListAST> ParseExternList() {
  std::vector<std::unique_ptr<ExternAST>> ExternList;
  while (CurTok.type != VOID_TOK && CurTok.type != INT_TOK && 
    CurTok.type != FLOAT_TOK && CurTok.type != BOOL_TOK) {
    auto Result = ParseExtern();
    ExternList.push_back(std::move(Result));
  }
  return std::make_unique<ExternListAST>(std::move(ExternList));
}

static std::unique_ptr<ExternAST> ParseExtern() {
  if(CurTok.type == EXTERN){
    getNextToken();
    if(CurTok.type == VOID_TOK || CurTok.type == INT_TOK || 
    CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) {
      TOKEN type = CurTok;
      getNextToken();
      if(CurTok.type == IDENT) {
        TOKEN ID = CurTok;
        getNextToken();
        if(CurTok.type == LPAR){
          getNextToken();
          auto params = ParseParams();
          if(CurTok.type == RPAR){
            getNextToken();
            if(CurTok.type == SC){
              getNextToken();
              return std::make_unique<ExternAST>(type, ID, std::move(params));
            }
            else {
              fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected token: %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo, ";", CurTok.lexeme.c_str());
              exit(0);
            }
          }
          else {
        fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected token: %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo, ")", CurTok.lexeme.c_str());
        exit(0);
      }
        }
        else {
        fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected token: %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo, "(", CurTok.lexeme.c_str());
        exit(0);
      }
      }
      else {
        fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected token: %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo, "IDENT", CurTok.lexeme.c_str());
        exit(0);
      }
    }
    else {
      fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected tokens: %s, %s, %s, %s\nActually received: %s\n", CurTok.lineNo, CurTok.columnNo , "int", "float", "bool", "void", CurTok.lexeme.c_str());
      exit(0);
    }
  }
  return nullptr;
}

static std::unique_ptr<DeclListAST> ParseDeclList() {
  std::vector<std::unique_ptr<AST_node>> DeclList;
  auto Result = ParseDecl();
  DeclList.push_back(std::move(Result));
  while (CurTok.type != EOF_TOK) {
    Result = ParseDecl();
    DeclList.push_back(std::move(Result));
  }
  return std::make_unique<DeclListAST>(std::move(DeclList));
}

static std::unique_ptr<AST_node> ParseDecl() {
  if(CurTok.type == VOID_TOK) return ParseFunDecl();
  if(CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK) {
    TOKEN type = CurTok;
    getNextToken();
    if(CurTok.type == IDENT) {
      TOKEN ID = CurTok;
      getNextToken();
      if(CurTok.type == SC){
        putBackToken(CurTok);
        putBackToken(ID);
        CurTok = type;
        return ParseLocalDecl();
      }
      else {
        putBackToken(CurTok);
        putBackToken(ID);
        CurTok = type;
        return ParseFunDecl();
      }
    }
  }
  return nullptr;
}

static std::unique_ptr<AST_node> ParseFunDecl() {
  TOKEN type = CurTok;
  getNextToken();
  if(CurTok.type == IDENT) {
    TOKEN ID = CurTok;
    getNextToken();
    if (CurTok.type == LPAR) {
      getNextToken();
      auto params = ParseParams();
      if(CurTok.type == RPAR) {
        getNextToken();
        auto block = ParseBlock();
        return std::make_unique<FunctionDeclarationAST>(type, ID, std::move(params), std::move(block));
      }
    }
  }
  return nullptr;
}

static std::unique_ptr<ParamListAST> ParseParams() {
  switch(CurTok.type){
    case INT_TOK:
    case FLOAT_TOK:
    case BOOL_TOK:
      return ParseParamList();
      break;
    case VOID_TOK:
    getNextToken();
      return nullptr;
    case RPAR:
      return nullptr;
  }
  return nullptr;
}

static std::unique_ptr<ParamListAST> ParseParamList() {
  std::vector<std::unique_ptr<ParamAST>> ParamList;
  auto Result = ParseParam();
  ParamList.push_back(std::move(Result));
  while(CurTok.type == COMMA) {
    getNextToken();
    Result = ParseParam();
    ParamList.push_back(std::move(Result));
  }
  if(CurTok.type == RPAR) return std::make_unique<ParamListAST>(std::move(ParamList));
  return nullptr;
}

static std::unique_ptr<ParamAST> ParseParam() {
  if(CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK){
    TOKEN type = CurTok;
    getNextToken();
    if(CurTok.type == IDENT){
      TOKEN name = CurTok;
      getNextToken();
      return std::make_unique<ParamAST>(type, name);
    }
  }
  else if (CurTok.type == VOID_TOK){
    getNextToken();
    return nullptr;
  }
  return nullptr;
}

static std::unique_ptr<AST_node> ParseLocalDecls() {
  std::vector<std::unique_ptr<AST_node>> DeclList;
  if(CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK){
    auto Result = ParseLocalDecl();
    DeclList.push_back(std::move(Result));
    while(CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK){
      Result = ParseLocalDecl();
      DeclList.push_back(std::move(Result));
    }
    return std::make_unique<LocalDeclListAST>(std::move(DeclList));
    }
  else return nullptr;
}

static std::unique_ptr<AST_node> ParseLocalDecl() {
  TOKEN type = CurTok;
  getNextToken();
  if(CurTok.type == IDENT){
    TOKEN ID = CurTok;
    getNextToken();
    if(CurTok.type == SC){
      getNextToken();
      return std::make_unique<VariableDeclarationAST>(type, ID);
    }
  }
  return nullptr;
}

static std::unique_ptr<AST_node> ParseStmtList() {
  std::vector<std::unique_ptr<AST_node>> StmtList;
  auto Result = ParseStmt();
  StmtList.push_back(std::move(Result));
  while(CurTok.type != RBRA){
    Result = ParseStmt();
    StmtList.push_back(std::move(Result));
  }
  return std::make_unique<StmtListAST>(std::move(StmtList));
}

static std::unique_ptr<AST_node> ParseStmt() {
  
  std::unique_ptr<AST_node> Result;
  switch(CurTok.type){
    case IDENT:
    case LPAR:
    case SC:
    case MINUS:
    case NOT:
    case INT_LIT:
    case FLOAT_LIT:
    case BOOL_LIT:
      Result = ParseExprStmt();
      break;
    case LBRA:
      Result = ParseBlock();
      break;
    case WHILE: {
      Result = ParseWhile();
      break;
    }
    case IF:
      Result = ParseIf();
      break;
    case RETURN:
      Result = ParseReturn();
      break;
  }
  return std::move(Result);
}

static std::unique_ptr<AST_node> ParseExprStmt() {
  if(CurTok.type == SC) {
    getNextToken();
    return nullptr;}
  else {
    auto Result = ParseExpr();
    if(CurTok.type == SC) {
      getNextToken();
      return std::move(Result);
      }
  }
  return nullptr;
}

static std::unique_ptr<AST_node> ParseWhile() {
  getNextToken();
  if(CurTok.type == LPAR){
    getNextToken();
    auto con = ParseExpr();
    if(CurTok.type == RPAR){
      getNextToken();
      auto exe = ParseStmt();
      return std::make_unique<WhileAST>(std::move(con), std::move(exe));
    }
  }
  return nullptr;
}

static std::unique_ptr<AST_node> ParseIf() {
  getNextToken();
  if(CurTok.type == LPAR){
    getNextToken();
    auto con = ParseExpr();
    if(CurTok.type == RPAR){
      getNextToken();
      auto exe = ParseBlock();
      auto else_st = ParseElse();
      return std::make_unique<IfAST>(std::move(con), std::move(exe), std::move(else_st));
    }
    else {
        fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected token: %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo, ")", CurTok.lexeme.c_str());
        exit(0);
      }
  }
  else {
        fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected token: %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo, "(", CurTok.lexeme.c_str());
        exit(0);
      }
  return nullptr;
}

static std::unique_ptr<AST_node> ParseElse() {
  switch(CurTok.type){
    case IDENT:
    case LPAR:
    case SC:
    case LBRA:
    case RBRA:
    case WHILE:
    case IF:
    case RETURN:
    case MINUS:
    case NOT:
    case INT_LIT:
    case FLOAT_LIT:
    case BOOL_LIT:
      return nullptr;
    case ELSE:
      getNextToken();
      auto Result = ParseBlock();
      return std::make_unique<ElseAST>(std::move(Result));
  }
  return nullptr;
}

static std::unique_ptr<AST_node> ParseReturn() {
  getNextToken();
  if(CurTok.type == SC) {
    getNextToken();
    return std::make_unique<ReturnAST>(nullptr);
    }
  else {
    auto Result = ParseExpr();
    if(CurTok.type == SC){
      getNextToken();
      return std::make_unique<ReturnAST>(std::move(Result));
    }
  }
  return nullptr;
}

static std::unique_ptr<AST_node> ParseBlock() {
  
  if(CurTok.type == LBRA) getNextToken();
  else {
    fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected token: %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo ,"{", CurTok.lexeme.c_str());
    exit(0);
  }
  auto decls = ParseLocalDecls();
  auto stmts = ParseStmtList();
  if(CurTok.type == RBRA) {
    getNextToken();
    return std::make_unique<BlockAST>(std::move(decls), std::move(stmts));
  }
  return nullptr;
}

static std::unique_ptr<AST_node> ParseExpr() {
  if(CurTok.type == IDENT){
    TOKEN ID = CurTok;
    getNextToken();
    switch(CurTok.type) {
      case ASSIGN:
        getNextToken();
        auto LHS = std::make_unique<IDENTnode>(ID);
        auto RHS = ParseExpr();
        return std::make_unique<AssignmentAST>(std::move(LHS), std::move(RHS));
    }
    putBackToken(CurTok);
    CurTok = ID;

    return ParseRVAL();
  }
  else {
    return ParseRVAL();
  }
  return nullptr;
}

static std::unique_ptr<AST_node> ParseRVAL() {
  auto LHS = ParseR1VAL();
  auto Result = ParseRVAL_Prime(std::move(LHS));
  return std::move(Result);
}

static std::unique_ptr<AST_node> ParseRVAL_Prime(std::unique_ptr<AST_node> LHS) {
  TOKEN Op;
  switch(CurTok.type) {
    case RPAR:
    case SC:
    case COMMA:
      // epsilon
      return LHS;
    case OR:
      Op = CurTok;
      getNextToken();

      auto Prime_RHS = ParseR1VAL();
      auto Prime_LHS = ParseRVAL_Prime(std::move(LHS));
      return std::make_unique<BinaryExprAST>(
        Op, std::move(Prime_LHS), std::move(Prime_RHS));
  }
  fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected token: %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo, "Operation token", CurTok.lexeme.c_str());
  exit(0);
  return nullptr;
}

static std::unique_ptr<AST_node> ParseR1VAL() {
  auto LHS = ParseR2VAL();
  auto Result = ParseR1VAL_Prime(std::move(LHS));
  return std::move(Result);
}

static std::unique_ptr<AST_node> ParseR1VAL_Prime(std::unique_ptr<AST_node> LHS) {
  TOKEN Op;
  switch(CurTok.type) {
    case RPAR:
    case SC:
    case COMMA:
    case OR:
      // epsilon
      return LHS;
    case AND:
      Op = CurTok;
      getNextToken();

      auto Prime_RHS = ParseR2VAL();
      auto Prime_LHS = ParseR1VAL_Prime(std::move(LHS));
      return std::make_unique<BinaryExprAST>(
        Op, std::move(Prime_LHS), std::move(Prime_RHS));
  }
  fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected token: %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo, "Operation token", CurTok.lexeme.c_str());
  exit(0);
  return nullptr;
}

static std::unique_ptr<AST_node> ParseR2VAL() {
  auto LHS = ParseR3VAL();
  auto Result = ParseR2VAL_Prime(std::move(LHS));
  return std::move(Result);
}

static std::unique_ptr<AST_node> ParseR2VAL_Prime(std::unique_ptr<AST_node> LHS) {
  TOKEN Op;
  switch(CurTok.type) {
    case RPAR:
    case SC:
    case COMMA:
    case OR:
    case AND:
      // epsilon
      return LHS;
    case EQ:
    case NE:
      Op = CurTok;
      getNextToken();

      auto Prime_RHS = ParseR3VAL();
      auto Prime_LHS = ParseR2VAL_Prime(std::move(LHS));
      return std::make_unique<BinaryExprAST>(
        Op, std::move(Prime_LHS), std::move(Prime_RHS));
  }
  fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected token: %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo, "Operation token", CurTok.lexeme.c_str());
  exit(0);
  return nullptr;
}

static std::unique_ptr<AST_node> ParseR3VAL() {
  auto LHS = ParseR4VAL();
  auto Result = ParseR3VAL_Prime(std::move(LHS));
  return std::move(Result);
}

static std::unique_ptr<AST_node> ParseR3VAL_Prime(std::unique_ptr<AST_node> LHS) {
  TOKEN Op;
  switch(CurTok.type) {
    case RPAR:
    case SC:
    case COMMA:
    case OR:
    case AND:
    case EQ:
    case NE:
      // epsilon
      return LHS;
    case LE:
    case LT:
    case GE:
    case GT:
      Op = CurTok;
      getNextToken();

      auto Prime_RHS = ParseR4VAL();
      auto Prime_LHS = ParseR3VAL_Prime(std::move(LHS));
      return std::make_unique<BinaryExprAST>(
        Op, std::move(Prime_LHS), std::move(Prime_RHS));
  }
  fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected token: %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo, "Operation token", CurTok.lexeme.c_str());
  exit(0);
  return nullptr;
}

static std::unique_ptr<AST_node> ParseR4VAL() {
  auto LHS = ParseR5VAL();
  auto Result = ParseR4VAL_Prime(std::move(LHS));
  return std::move(Result);
}

static std::unique_ptr<AST_node> ParseR4VAL_Prime(std::unique_ptr<AST_node> LHS) {
  TOKEN Op;
  switch(CurTok.type) {
    case RPAR:
    case SC:
    case COMMA:
    case OR:
    case AND:
    case EQ:
    case NE:
    case LE:
    case LT:
    case GE:
    case GT:
      // epsilon
      return LHS;
    case PLUS:
    case MINUS:
      Op = CurTok;
      getNextToken();

      auto Prime_RHS = ParseR5VAL();
      auto Prime_LHS = ParseR4VAL_Prime(std::move(LHS));
      return std::make_unique<BinaryExprAST>(
        Op, std::move(Prime_LHS), std::move(Prime_RHS));
  }
  fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected token: %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo, "Operation token", CurTok.lexeme.c_str());
  exit(0);
  return nullptr;
}

static std::unique_ptr<AST_node> ParseR5VAL() {
  auto LHS = ParseR6VAL();
  auto Result = ParseR5VAL_Prime(std::move(LHS));
  return std::move(Result);
}

static std::unique_ptr<AST_node> ParseR5VAL_Prime(std::unique_ptr<AST_node> LHS) {
  TOKEN Op;
  switch(CurTok.type) {
    case RPAR:
    case SC:
    case COMMA:
    case OR:
    case AND:
    case EQ:
    case NE:
    case LE:
    case LT:
    case GE:
    case GT:
    case PLUS:
    case MINUS:
      // epsilon
      return LHS;
    case ASTERIX:
    case DIV:
    case MOD:
      Op = CurTok;
      getNextToken();

      auto Prime_RHS = ParseR6VAL();
      auto Prime_LHS = ParseR5VAL_Prime(std::move(LHS));
      return std::make_unique<BinaryExprAST>(
        Op, std::move(Prime_LHS), std::move(Prime_RHS));
  }
  fprintf(stderr,"SYNTAX ERROR on line %d, column %d\nExpected token: %s\nActually received %s\n", CurTok.lineNo, CurTok.columnNo, "Operation token", CurTok.lexeme.c_str());
  exit(0);
  return nullptr;
}

static std::unique_ptr<AST_node> ParseR6VAL() {
  std::unique_ptr<AST_node> Result;
  switch(CurTok.type) {
    case MINUS:
    case NOT: 
      {TOKEN temp = CurTok;
      getNextToken();
      Result = std::make_unique<NegatedValAST>(temp, ParseR6VAL());
      break;}
    case INT_LIT:
      Result = ParseINT_LIT();
      break;
    case FLOAT_LIT:
      Result = ParseFLOAT_LIT();
      break;
    case BOOL_LIT:
      Result = ParseBOOL_LIT();
      break;
    case IDENT: {
      TOKEN ID = CurTok;
      auto FuncName = std::make_unique<IDENTnode>(ID);
      getNextToken();
      if(CurTok.type == LPAR) {
        getNextToken();
        auto FuncArgs = ParseArgs();
        auto FuncCall = std::make_unique<FunctionCallAST>(std::move(FuncName), std::move(FuncArgs));
        if(CurTok.type == RPAR) {
          getNextToken();
          return std::move(FuncCall);
          }
      }
      else {
        Result = std::move(FuncName);
        break;
      }
    }
    case LPAR:
      getNextToken();
      auto expression = ParseExpr();
      if(CurTok.type == RPAR) {
        getNextToken();
        return std::move(expression);
      }
  }

  return std::move(Result);
}

static std::unique_ptr<ArgListAST> ParseArgs() {
  switch(CurTok.type){
    case RPAR:
    case SC:
    case COMMA:
    // epsilon
    return nullptr;
  }
  return ParseArgList();
}

static std::unique_ptr<ArgListAST> ParseArgList() {
  std::vector<std::unique_ptr<AST_node>> ArgList;
  auto Result = ParseExpr();
  ArgList.push_back(std::move(Result));
  while(CurTok.type == COMMA) {
    getNextToken();
    Result = ParseExpr();
    ArgList.push_back(std::move(Result));
  }
  if(CurTok.type == RPAR) return std::make_unique<ArgListAST>(std::move(ArgList));
  return nullptr;
}

static std::unique_ptr<AST_node> ParseINT_LIT() {
  auto Result = std::make_unique<INT_LITnode>(CurTok);
  getNextToken();
  return std::move(Result);
  }

static std::unique_ptr<AST_node> ParseFLOAT_LIT() {
  auto Result = std::make_unique<FLOAT_LITnode>(CurTok);
  getNextToken();
  return std::move(Result);
  }

static std::unique_ptr<AST_node> ParseBOOL_LIT() {
  auto Result = std::make_unique<BOOL_LITnode>(CurTok);
  getNextToken();
  return std::move(Result);
  }

// r6val ::= IDENT
static std::unique_ptr<AST_node> ParseIDENT() {
  auto Result = std::make_unique<IDENTnode>(CurTok);
  getNextToken();
  return std::move(Result);
  }

static void parser() {
  // add body
  auto Result = ParseProgram();
  fprintf(stderr, "%s", Result->to_string(0).c_str());
  Result->codegen();
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const AST_node &ast) {
  os << ast.to_string(0);
  return os;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  if (argc == 2) {
    pFile = fopen(argv[1], "r");
    if (pFile == NULL)
      perror("Error opening file");
  } else {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // initialize line number and column numbers to zero
  lineNo = 1;
  columnNo = 1;

  // get the first token
  getNextToken();
  // while (CurTok.type != EOF_TOK) {
  //   fprintf(stderr, "Token: %s with type %d\n", CurTok.lexeme.c_str(),
  //           CurTok.type);
  //   getNextToken();
  // }
  // fprintf(stderr, "Lexer Finished\n");

  // Make the module, which holds all the code.
  TheModule = std::make_unique<Module>("mini-c", TheContext);

  // Run the parser now.
  parser();
  fprintf(stderr, "\nParsing Finished\n");



  //********************* Start printing final IR **************************
  // Print out all of the generated code into a file called output.ll
  auto Filename = "output.ll";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }
  //TheModule->print(errs(), nullptr); // print IR to terminal
  TheModule->print(dest, nullptr);
  //********************* End printing final IR ****************************

  fclose(pFile); // close the file that contains the code that was parsed
  return 0;
}

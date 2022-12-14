#include "cc.h"
// 生成AST  语法分析
// 抽象语法树

// 局部和全局变量或是typedef的域, enum常量的域
typedef struct VarScope VarScope;
struct VarScope {
  VarScope *Next; // 下一变量域
  char *Name;     // 变量域名称
  Obj *Var;       // 对应的变量  
  Type *Typedef;  // 别名
  Type *EnumTy;   // 枚举的类型
  int EnumVal;    // 枚举的值
};

// 结构体标签，联合体标签，枚举标签的域
typedef struct TagScope TagScope;
struct TagScope {
  TagScope *Next; // 下一标签域
  char *Name;     // 域名称
  Type *Ty;       // 域类型
};

// 表示一个块域
typedef struct Scope Scope;
struct Scope {
  Scope *Next;    // 指向上一级的域

  // C有两个域：变量（或类型别名）域，结构体（或联合体，枚举）标签域
  VarScope *Vars; // 指向当前域内的变量
  TagScope *Tags; // 指向当前域内的结构体标签
};

// 变量属性
typedef struct {
  bool IsTypedef; // 是否为类型别名
  bool IsStatic;  // 是否为文件域内
} VarAttr;

// 所有的域的链表
static Scope *Scp = &(Scope){};

// 在解析时，全部的变量实例都被累加到这个列表里。
Obj *Locals;  // 局部变量
Obj *Globals; // 全局变量

// 指向当前正在解析的函数
static Obj *CurrentFn;

// 语法
// program = (typedef | functionDefinition* | global-variable)*
// functionDefinition = declarator ("{" compoundStmt | ";" )
// global-variable = declarator?("," declarator)* ";"
// declspec =  ("void" | "_Bool" | "char" | "short" | "int" |"long" 
//            | "typedef" | | "static"
//            | "struct" structDecl | "union" unionDecl
//            | "enum" enumSpecifier)+
// enumSpecifier = ident? "{" enumList? "}"
//                 | ident ("{" enumList? "}")?
// enumList = ident ("=" num)? ("," ident ("=" num)?)*
// declarator = "*"* ( "(" declarator ")" | ident ) typeSuffix
// typeSuffix = "(" funcParams | "[" num "]" typeSuffix | ε
// funcParams = (param ("," param)*)? ")"
// param = declspec declarator
// compoundStmt = (typedef | declaration | stmt)* "}"
// declaration =
//         declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
// stmt = "return" expr ";"
//        | "if" "(" expr ")" stmt ("else" stmt)?
//        | "for" "(" exprStmt expr? ";" expr? ")" stmt
//        | "while" "(" expr ")" stmt
//        | "{" compoundStmt
//        | exprStmt
// exprStmt = expr? ";"
// expr = assign ("," expr)?
// assign = logOr (assignOp assign)?
// logOr = logAnd ("||" logAnd)*
// logAnd = bitOr ("&&" bitOr)*
// bitOr = bitXor ("|" bitXor)*
// bitXor = bitAnd ("^" bitAnd)*
// bitAnd = equality ("&" equality)*
// assignOp = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
// mul = cast ("*" cast | "/" cast | "%" cast)*
// cast = ("(" typeName ")" cast) | unary
// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
//       | ("++" | "--") unary
//       | postfix
// structDecl = structUnionDecl
// unionDecl = structUnionDecl
// structUnionDecl = ident? ("{" structMembers)?
// structMembers = (declspec declarator (","  declarator)* ";")* "}"
// postfix = primary ("[" expr "]" | "." ident | "->" ident | "++" | "--")*
// primary = "(" "{" stmt+ "}" ")"
//         | "(" expr ")"
//         | "sizeof" "(" typeName ")"
//         | "sizeof" unary
//         | ident funcArgs?
//         | str
//         | num
// typeName = declspec abstractDeclarator
// abstractDeclarator = "*"* ("(" abstractDeclarator ")")? typeSuffix

// funcall = ident "(" (assign ("," assign)*)? ")"
static bool isTypename(Token *Tok);
static Type *declspec(Token **Rest, Token *Tok, VarAttr *Attr);
static Type *enumSpecifier(Token **Rest, Token *Tok);
static Type *declarator(Token **Rest, Token *Tok, Type *Ty);
static Node *compoundStmt(Token **Rest, Token *Tok);
static Type *typeSuffix(Token **Rest, Token *Tok, Type *Ty);
static Type *funcParams(Token **Rest, Token *Tok, Type *Ty);
static Node *declaration(Token **Rest, Token *Tok, Type *BaseTy);
static Node *stmt(Token **Rest, Token *Tok);
static Node *exprStmt(Token **Rest, Token *Tok);
static Node *expr(Token **Rest, Token *Tok);
static Node *assign(Token **Rest, Token *Tok);
static Node *logOr(Token **Rest, Token *Tok);
static Node *logAnd(Token **Rest, Token *Tok);
static Node *bitOr(Token **Rest, Token *Tok);
static Node *bitXor(Token **Rest, Token *Tok);
static Node *bitAnd(Token **Rest, Token *Tok);
static Node *equality(Token **Rest, Token *Tok);
static Node *relational(Token **Rest, Token *Tok);
static Node *add(Token **Rest, Token *Tok);
static Node *newAdd(Node *LHS, Node *RHS, Token *Tok);
static Node *newSub(Node *LHS, Node *RHS, Token *Tok);
static Node *mul(Token **Rest, Token *Tok);
static Node *cast(Token **Rest, Token *Tok);
static Type *structDecl(Token **Rest, Token *Tok);
static Type *unionDecl(Token **Rest, Token *Tok);
static Node *unary(Token **Rest, Token *Tok);
static Node *postfix(Token **Rest, Token *Tok);
static Node *primary(Token **Rest, Token *Tok);
static Node *funCall(Token **Rest, Token *Tok);
static Token *parseTypedef(Token *Tok, Type *BaseTy);

// 进入域
static void enterScope(void) {
  Scope *S = calloc(1, sizeof(Scope));
  // 后来的在链表头部
  // 类似于栈的结构，栈顶对应最近的域
  S->Next = Scp;
  Scp = S;
}

// 结束当前域
static void leaveScope(void) { Scp = Scp->Next; }

// 通过名称，查找一个变量  
// 一个树状结构
static VarScope *findVar(Token *Tok) {
  // 此处越先匹配的域，越深层
  for (Scope *S = Scp; S; S = S->Next)  // next指向上一级的域
    // 遍历域内的所有变量
    for (VarScope *S2 = S->Vars; S2; S2 = S2->Next)  // next指向下一变量域
      if (equal(Tok, S2->Name))
        return S2;
  return NULL;
}

// 通过Token查找标签
static Type *findTag(Token *Tok) {
  for (Scope *S = Scp; S; S = S->Next)
    for (TagScope *S2 = S->Tags; S2; S2 = S2->Next)
      if (equal(Tok, S2->Name))
        return S2->Ty;
  return NULL;
}

// 将变量存入当前的域中
static VarScope *pushScope(char *Name) {
  VarScope *S = calloc(1, sizeof(VarScope));
  S->Name = Name;
  // 后来的在链表头部
  S->Next = Scp->Vars;
  Scp->Vars = S;
  return S;
}

static void pushTagScope(Token *Tok, Type *Ty) {
  TagScope *S = calloc(1, sizeof(TagScope));
  S->Name = strndup(Tok->Loc, Tok->Len);
  S->Ty = Ty;
  S->Next = Scp->Tags;
  Scp->Tags = S;
}

// 新建一个节点
static Node *newNode(NodeKind Kind, Token *Tok) {
  Node *Nd = calloc(1, sizeof(Node));
  Nd->Kind = Kind;
  Nd->Tok = Tok;
  return Nd;
}

// 新建一个单叉树
static Node *newUnary(NodeKind Kind, Node *Expr, Token *Tok) {
  Node *Nd = newNode(Kind, Tok);
  Nd->LHS = Expr;
  return Nd;
}

// 新建一个二叉树节点
static Node *newBinary(NodeKind Kind, Node *LHS, Node *RHS, Token *Tok) {
  Node *Nd = newNode(Kind, Tok);
  Nd->LHS = LHS;
  Nd->RHS = RHS;
  return Nd;
}

// 新建一个数字节点
static Node *newNum(int64_t Val, Token *Tok) {
  Node *Nd = newNode(ND_NUM, Tok);
  Nd->Val = Val;
  return Nd;
}

// 新建一个长整型节点
static Node *newLong(int64_t Val, Token *Tok) {
  Node *Nd = newNode(ND_NUM, Tok);
  Nd->Val = Val;
  Nd->Ty = TyLong;
  return Nd;
}

// 新变量
static Node *newVarNode(Obj *Var, Token *Tok) {
  Node *Nd = newNode(ND_VAR, Tok);
  Nd->Var = Var;
  return Nd;
}

// 新转换节点
Node *newCast(Node *Expr, Type *Ty) {
  addType(Expr);

  Node *Nd = calloc(1, sizeof(Node));
  Nd->Kind = ND_CAST;
  Nd->Tok = Expr->Tok;
  Nd->LHS = Expr;
  Nd->Ty = copyType(Ty);
  return Nd;
}

// 新建变量
static Obj *newVar(char *Name, Type *Ty) {
  Obj *Var = calloc(1, sizeof(Obj));
  Var->Name = Name;
  Var->Ty = Ty;
  pushScope(Name)->Var = Var;
  return Var;
}

// 在链表中新增一个局部变量
static Obj *newLVar(char *Name, Type *Ty) {
  Obj *Var = newVar(Name, Ty);
  Var->IsLocal = true;
  // 将变量插入头部
  Var->Next = Locals;
  Locals = Var;
  return Var;
}

// 在链表中新增一个全局变量
static Obj *newGVar(char *Name, Type *Ty) {
  Obj *Var = newVar(Name, Ty);
  Var->Next = Globals;
  Globals = Var;
  return Var;
}

// 新增唯一名称
static char *newUniqueName(void) {
  static int Id = 0;
  return format(".L..%d", Id++);
}

// 新增匿名全局变量
static Obj *newAnonGVar(Type *Ty) { return newGVar(newUniqueName(), Ty); }

// 新增字符串字面量
static Obj *newStringLiteral(char *Str, Type *Ty) {
  Obj *Var = newAnonGVar(Ty);
  Var->InitData = Str;
  return Var;
}

// 查找类型别名
static Type *findTypedef(Token *Tok) {
  // 类型别名是个标识符
  if (Tok->Kind == TK_IDENT) {
    // 查找是否存在于变量域内
    VarScope *S = findVar(Tok);
    if (S)
      return S->Typedef;
  }
  return NULL;
}

// 获取标识符
static char *getIdent(Token *Tok) {
  if (Tok->Kind != TK_IDENT)
    errorTok(Tok, "expected an identifier");
  return strndup(Tok->Loc, Tok->Len);
}

// 获取数字
static long getNumber(Token *Tok) {
  if (Tok->Kind != TK_NUM)
    errorTok(Tok, "expected a number");
  return Tok->Val;
}

// 判断是否为类型名
static bool isTypename(Token *Tok) {
  static char *Kw[] = {
      "void", "_Bool", "char", "short", "int", "long", "struct", "union", "typedef", "enum", "static",
  };

  for (int I = 0; I < sizeof(Kw) / sizeof(*Kw); ++I) {
    if (equal(Tok, Kw[I]))
      return true;
  }
  return findTypedef(Tok);
}

// abstractDeclarator = "*"* ("(" abstractDeclarator ")")? typeSuffix
static Type *abstractDeclarator(Token **Rest, Token *Tok, Type *Ty) {
  // "*"*
  while (consume(&Tok, Tok, "*"))
    Ty = pointerTo(Ty);

  // ("(" abstractDeclarator ")")?
  if (equal(Tok, "(")) {
    Token *Start = Tok;
    Type Dummy = {};
    // 使Tok前进到")"后面的位置
    abstractDeclarator(&Tok, Start->Next, &Dummy);
    Tok = skip(Tok, ")");
    // 获取到括号后面的类型后缀，Ty为解析完的类型，Rest指向分号
    Ty = typeSuffix(Rest, Tok, Ty);
    // 解析Ty整体作为Base去构造，返回Type的值
    return abstractDeclarator(&Tok, Start->Next, Ty);
  }

  // typeSuffix
  return typeSuffix(Rest, Tok, Ty);
}

// typeName = declspec abstractDeclarator
// 获取类型的相关信息
static Type *typename(Token **Rest, Token *Tok) {
  // declspec
  Type *Ty = declspec(&Tok, Tok, NULL);
  // abstractDeclarator
  return abstractDeclarator(Rest, Tok, Ty);
}

// (declarator specifier)
// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
//             | "typedef" | "static"
//             | "struct" structDecl | "union" unionDecl
//             | "enum" enumSpecifier)+
static Type *declspec(Token **Rest, Token *Tok, VarAttr *Attr) {
  // 类型的组合，被表示为例如：LONG+LONG=1<<9
  // 可知long int和int long是等价的。
  enum {
    VOID  = 1 << 0,
    BOOL  = 1 << 2,
    CHAR  = 1 << 4,
    SHORT = 1 << 6,
    INT   = 1 << 8,
    LONG  = 1 << 10,
    OTHER = 1 << 12,
  };

  Type *Ty = TyInt;
  int Counter = 0; // 记录类型相加的数值

  // 遍历所有类型名的Tok
  while (isTypename(Tok)) {
    // 处理typedef关键字
    if (equal(Tok, "typedef") || equal(Tok, "static")) {
      if (!Attr)
        errorTok(Tok, "storage class specifier is not allowed in this context");
      if (equal(Tok, "typedef"))
        Attr->IsTypedef = true;
      else
        Attr->IsStatic = true;
      // typedef不应与static一起使用
      if (Attr->IsTypedef && Attr->IsStatic)
        errorTok(Tok, "typedef and static may not be used together");
      Tok = Tok->Next;
      continue;
    }

    // 处理用户定义的类型
    // typedef struct a b;
    Type *Ty2 = findTypedef(Tok);   // struct a
    if (equal(Tok, "struct") || equal(Tok, "union") || equal(Tok, "enum") || Ty2) {
      if(Counter)  // 这几种不能嵌套
        break;
      if (equal(Tok, "struct")){
        Ty = structDecl(&Tok, Tok->Next);
      }else if (equal(Tok, "union")) {
        Ty = unionDecl(&Tok, Tok->Next);
      }else if (equal(Tok, "enum")) {
        Ty = enumSpecifier(&Tok, Tok->Next);
      }else{
        // 将类型设为类型别名指向的类型
        Ty = Ty2;   // struct a 
        Tok = Tok->Next;
      }
      Counter += OTHER;
      continue;
    }

    // 对于出现的类型名加入Counter
    // 每一步的Counter都需要有合法值
    if (equal(Tok, "void"))
      Counter += VOID;
    else if (equal(Tok, "_Bool"))
      Counter += BOOL;
    else if (equal(Tok, "char"))
      Counter += CHAR;
    else if (equal(Tok, "short"))
      Counter += SHORT;
    else if (equal(Tok, "int"))
      Counter += INT;
    else if (equal(Tok, "long"))
      Counter += LONG;
    else
      unreachable();

    // 根据Counter值映射到对应的Type
    switch (Counter) {
    case VOID:
      Ty = TyVoid;
      break;
    case BOOL:
      Ty = TyBool;
      break;
    case CHAR:
      Ty = TyChar;
      break;
    case SHORT:
    case SHORT + INT:
      Ty = TyShort;
      break;
    case INT:
      Ty = TyInt;
      break;
    case LONG:
    case LONG + INT:
    case LONG + LONG:
    case LONG + LONG + INT:
      Ty = TyLong;
      break;
    default:
      errorTok(Tok, "invalid type");
    }

    Tok = Tok->Next;
  } // while (isTypename(Tok))

  *Rest = Tok;
  return Ty;

}

// 获取枚举类型信息
// enumSpecifier = ident? "{" enumList? "}"
//               | ident ("{" enumList? "}")?
// enumList      = ident ("=" num)? ("," ident ("=" num)?)*
static Type *enumSpecifier(Token **Rest, Token *Tok) {
  Type *Ty = enumType();

  // 读取标签
  // ident?
  Token *Tag = NULL;
  if (Tok->Kind == TK_IDENT) {
    Tag = Tok;
    Tok = Tok->Next;
  }

  // 处理没有{}的情况
  if (Tag && !equal(Tok, "{")) {
    Type *Ty = findTag(Tag);
    if (!Ty)
      errorTok(Tag, "unknown enum type");
    if (Ty->Kind != TY_ENUM)
      errorTok(Tag, "not an enum tag");
    *Rest = Tok;
    return Ty;
  }

  // "{" enumList? "}"
  Tok = skip(Tok, "{");

  // enumList
  // 读取枚举列表
  int I = 0;   // 第几个枚举常量
  int Val = 0; // 枚举常量的值
  while (!equal(Tok, "}")) {
    if (I++ > 0)
      Tok = skip(Tok, ",");

    char *Name = getIdent(Tok);
    Tok = Tok->Next;

    // 判断是否存在赋值
    if (equal(Tok, "=")) {
      Val = getNumber(Tok->Next);
      Tok = Tok->Next->Next;
    }

    // 存入枚举常量
    VarScope *S = pushScope(Name);
    S->EnumTy = Ty;
    S->EnumVal = Val++;
  }

  *Rest = Tok->Next;

  if (Tag)
    pushTagScope(Tag, Ty);
  return Ty;
}

// declarator = "*"* ( "(" declarator ")" | ident ) typeSuffix
static Type *declarator(Token **Rest, Token *Tok, Type *Ty) {
  // "*"*
  // 构建所有的（多重）指针
  while (consume(&Tok, Tok, "*"))
    Ty = pointerTo(Ty);
  
  // "(" declarator ")"
  if (equal(Tok, "(")) {
    // 记录"("的位置
    Token *Start = Tok;
    Type Dummy = {};
    // 使Tok前进到")"后面的位置
    declarator(&Tok, Start->Next, &Dummy);
    Tok = skip(Tok, ")");
    // 获取到括号后面的类型后缀，Ty为解析完的类型，Rest指向分号
    Ty = typeSuffix(Rest, Tok, Ty);
    // 解析Ty整体作为Base去构造，返回Type的值
    return declarator(&Tok, Start->Next, Ty);
  }

  if (Tok->Kind != TK_IDENT)
    errorTok(Tok, "expected a variable name");

  // typeSuffix
  Ty = typeSuffix(Rest, Tok->Next, Ty);
  // ident
  // 变量名 or 函数名
  Ty->Name = Tok;
  return Ty;
}

// typeSuffix = "(" funcParams | "[" num "]" typeSuffix | ε
static Type *typeSuffix(Token **Rest, Token *Tok, Type *Ty) {
  // "(" funcParams
  if (equal(Tok, "("))
    return funcParams(Rest, Tok->Next, Ty);
  // "[" num "]"
  if (equal(Tok, "[")) {
    int Sz = getNumber(Tok->Next);
    Tok = skip(Tok->Next->Next, "]");
    Ty = typeSuffix(Rest, Tok, Ty); 
    // 最终会递归到 ε 然后设置Rest  *Rest = Tok, 如果在这里设置 会把原来的指向末尾的Rest 重新设置为 "["
    return arrayOf(Ty, Sz);
  }

  *Rest = Tok;
  return Ty;
}

// funcParams = (param ("," param)*)? ")"
// param = declspec declarator
static Type *funcParams(Token **Rest, Token *Tok, Type *Ty) {
  Type Head = {};
  Type *Cur = &Head;

  while (!equal(Tok, ")")) {
    // funcParams = param ("," param)*
    // param = declspec declarator
    if (Cur != &Head)
      Tok = skip(Tok, ",");
    Type *BaseTy = declspec(&Tok, Tok, NULL); // int
    Type *DeclarTy = declarator(&Tok, Tok, BaseTy); // int ***  []

    // 将类型复制到形参链表一份
    // DeclarTy出了这个函数就没了, 所有要copy
    Cur->Next = copyType(DeclarTy);
    Cur = Cur->Next;
  }

  // 封装一个函数节点
  Ty = funcType(Ty);
  // 传递形参
  Ty->Params = Head.Next;
  *Rest = Tok->Next;
  return Ty;
}

// declaration =
//    declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
static Node *declaration(Token **Rest, Token *Tok, Type *BaseTy) {
  Node Head = {};
  Node *Cur = &Head;
  // 对变量声明次数计数
  int I = 0;

  // (declarator ("=" expr)? ("," declarator ("=" expr)?)*)?
  while (!equal(Tok, ";")) {
    // 第1个变量不必匹配 ","
    if (I++ > 0)
      Tok = skip(Tok, ",");

    // declarator
    // 声明获取到变量类型，包括变量名
    Type *Ty = declarator(&Tok, Tok, BaseTy);
    if (Ty->Kind == TY_VOID)
      errorTok(Tok, "variable declared void");
    Obj *Var = newLVar(getIdent(Ty->Name), Ty);

    // 如果不存在"="则为变量声明，不需要生成节点，已经存储在Locals中了
    if (!equal(Tok, "="))
      continue;

    // 解析“=”后面的Token
    Node *LHS = newVarNode(Var, Ty->Name);
    // 解析递归赋值语句
    Node *RHS = assign(&Tok, Tok->Next);
    Node *Node = newBinary(ND_ASSIGN, LHS, RHS, Tok);
    // 存放在表达式语句中
    Cur->Next = newUnary(ND_EXPR_STMT, Node, Tok);
    Cur = Cur->Next;
  }

  // 将所有表达式语句，存放在代码块中
  Node *Nd = newNode(ND_BLOCK, Tok);
  Nd->Body = Head.Next;
  *Rest = Tok->Next;
  return Nd;
}

// 解析语句
// stmt = "return" expr ";"
//        | "if" "(" expr ")" stmt ("else" stmt)?
//        | "for" "(" exprStmt expr? ";" expr? ")" stmt
//        | "while" "(" expr ")" stmt
//        | "{" compoundStmt
//        | exprStmt
static Node *stmt(Token **Rest, Token *Tok) {
  // "return" expr ";"
  if (equal(Tok, "return")) {
    Node *Nd = newNode(ND_RETURN, Tok);
    Node *Exp = expr(&Tok, Tok->Next);
    *Rest = skip(Tok, ";");

    addType(Exp);
    // 对于返回值进行类型转换
    Nd->LHS = newCast(Exp, CurrentFn->Ty->ReturnTy);
    return Nd;
  }

  // "if" "(" expr ")" stmt ("else" stmt)?
  if (equal(Tok, "if")) {
    Node *Nd = newNode(ND_IF, Tok);
    // "(" expr ")"
    Tok = skip(Tok->Next, "(");
    Nd->Cond = expr(&Tok, Tok);
    Tok = skip(Tok, ")");
    // stmt 符合条件后语句
    Nd->Then = stmt(&Tok, Tok);
    // ("else" stmt)?   不符合条件后语句
    if (equal(Tok, "else")){
      Nd->Els = stmt(&Tok, Tok->Next);
    }
    *Rest = Tok;
    return Nd;
  }

  // "for" "(" exprStmt expr? ";" expr? ")" stmt
  if (equal(Tok, "for")) {
    Node *Nd = newNode(ND_FOR, Tok);
    // "("
    Tok = skip(Tok->Next, "(");
    // 进入for循环域
    enterScope();

    // exprStmt | declspec declaration
    if (isTypename(Tok)) {
      // 初始化循环变量
      Type *BaseTy = declspec(&Tok, Tok, NULL);
      Nd->Init = declaration(&Tok, Tok, BaseTy);
    } else {
      // 初始化语句
      Nd->Init = exprStmt(&Tok, Tok);
    }

    // expr? ";"
    if (!equal(Tok, ";"))
      Nd->Cond = expr(&Tok, Tok);
    Tok = skip(Tok, ";");

    // expr?
    if (!equal(Tok, ")"))
      Nd->Inc = expr(&Tok, Tok);
    // ")"
    Tok = skip(Tok, ")");

    // stmt
    Nd->Then = stmt(Rest, Tok);
    // 退出for循环域
    leaveScope();
    return Nd;
  }

  // "while" "(" expr ")" stmt
  if (equal(Tok, "while")) {
    Node *Nd = newNode(ND_FOR, Tok);
    // "(" expr ")"
    Tok = skip(Tok->Next, "(");
    Nd->Cond = expr(&Tok, Tok);
    Tok = skip(Tok, ")");
    // stmt
    Nd->Then = stmt(Rest, Tok);
    return Nd;
  }
  // "{" compoundStmt
  if (equal(Tok, "{")){
    return compoundStmt(Rest, Tok->Next);
  }

  // exprStmt
  return exprStmt(Rest, Tok);
}

// 解析复合语句
// compoundStmt = (typedef | declaration | stmt)* "}"
static Node *compoundStmt(Token **Rest, Token *Tok) {
  // 这里使用了和词法分析类似的单向链表结构
  Node *Nd = newNode(ND_BLOCK, Tok);  // 存储这里的tok
  Node Head = {};
  Node *Cur = &Head;

  // 进入新的域
  enterScope();

  // (declaration | stmt)* "}"
  while (!equal(Tok, "}")) {
    //declaration 
    if(isTypename(Tok)){
      VarAttr Attr = {};
      Type *BaseTy = declspec(&Tok, Tok, &Attr);
      // 解析typedef的语句
      if(Attr.IsTypedef){
        Tok = parseTypedef(Tok, BaseTy);
        continue;
      }

      // 解析变量声明语句
      Cur->Next = declaration(&Tok, Tok, BaseTy);

    }else {
      // stmt
      Cur->Next = stmt(&Tok, Tok);
    }
    Cur = Cur->Next;
    // 构造完AST后，为节点添加类型信息    // TODO   为什么在这里给节点添加信息???  我放到下面了
    addType(Cur);  // TODO CRUX 就tm这一句 卧槽卧槽卧槽  
  }

  // 结束当前的域
  leaveScope();

  // Nd的Body存储了{}内解析的语句
  
  Nd->Body = Head.Next;
  *Rest = Tok->Next;
  return Nd;
}

// 解析表达式语句
// exprStmt = expr? ";"
static Node *exprStmt(Token **Rest, Token *Tok) {
  // ";"
  if (equal(Tok, ";")){
    *Rest = Tok->Next;
    return newNode(ND_BLOCK, Tok);
  }
  //expr ";"
  Node *Nd = newNode(ND_EXPR_STMT, Tok);
  Nd->LHS = expr(&Tok, Tok);
  *Rest = skip(Tok, ";");
  return Nd;
}

// 解析表达式
// expr = assign ("," expr)?
static Node *expr(Token **Rest, Token *Tok) { 
  Node *Nd =  assign(&Tok, Tok); 
  if(equal(Tok, ",")){
    return newBinary(ND_COMMA, Nd, expr(Rest, Tok->Next), Tok);
  }
  *Rest = Tok;
  return Nd;
}

// 转换 A op= B为 TMP = &A, *TMP = *TMP op B
static Node *toAssign(Node *Binary) {
  // A B
  Node *A = Binary->LHS, *B = Binary->RHS;
  NodeKind op = Binary->Kind;
  addType(A);
  addType(B);
  Token *Tok = Binary->Tok;

  // TMP
  Obj *Var = newLVar("", pointerTo(A->Ty));

  // TMP = &A
  Node *Expr1 = newBinary(ND_ASSIGN, newVarNode(Var, Tok),
                          newUnary(ND_ADDR, A, Tok), Tok);

  // *TMP = *TMP op B
  Node *Expr2 = newBinary(
      ND_ASSIGN, 
      newUnary(ND_DEREF, newVarNode(Var, Tok), Tok),  // LHS
      //RHS  *TMP op B
      newBinary(op, newUnary(ND_DEREF, newVarNode(Var, Tok), Tok), B, Tok), 
      Tok);

  // TMP = &A, *TMP = *TMP op B
  return newBinary(ND_COMMA, Expr1, Expr2, Tok);
}

// 解析赋值
// assign = logOr (assignOp assign)?
// assignOp = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
static Node *assign(Token **Rest, Token *Tok) {
  // equality
  Node *Nd = logOr(&Tok, Tok);

  // 可能存在递归赋值，如a=b=1
  // ("=" assign)?
  if (equal(Tok, "="))
    return newBinary(ND_ASSIGN, Nd, assign(Rest, Tok->Next), Tok);

  // ("+=" assign)?
  if (equal(Tok, "+="))
    // return newBinary(ND_ASSIGN, Nd, newAdd(Nd, assign(Rest, Tok->Next), Tok), Tok);
    return toAssign(newAdd(Nd, assign(Rest, Tok->Next), Tok));

  // ("-=" assign)?
  if (equal(Tok, "-="))
    // return newBinary(ND_ASSIGN, Nd, newSub(Nd, assign(Rest, Tok->Next), Tok), Tok);
    return toAssign(newSub(Nd, assign(Rest, Tok->Next), Tok));

  // ("*=" assign)?
  if (equal(Tok, "*="))
    // return newBinary(ND_ASSIGN, Nd, newBinary(ND_MUL, Nd, assign(Rest, Tok->Next), Tok), Tok);
    return toAssign(newBinary(ND_MUL, Nd, assign(Rest, Tok->Next), Tok));

  // ("/=" assign)?
  if (equal(Tok, "/="))
    // return newBinary(ND_ASSIGN, Nd, newBinary(ND_DIV, Nd, assign(Rest, Tok->Next), Tok), Tok);
    return toAssign(newBinary(ND_DIV, Nd, assign(Rest, Tok->Next), Tok));

  // ("%=" assign)?
  if (equal(Tok, "%="))
    // return newBinary(ND_ASSIGN, Nd, newBinary(ND_MOD, Nd, assign(Rest, Tok->Next), Tok), Tok);
    return toAssign(newBinary(ND_MOD, Nd, assign(Rest, Tok->Next), Tok));

  // ("&=" assign)?
  if (equal(Tok, "&="))
    return toAssign(newBinary(ND_BITAND, Nd, assign(Rest, Tok->Next), Tok));

  // ("|=" assign)?
  if (equal(Tok, "|="))
    return toAssign(newBinary(ND_BITOR, Nd, assign(Rest, Tok->Next), Tok));

  // ("^=" assign)?
  if (equal(Tok, "^="))
    return toAssign(newBinary(ND_BITXOR, Nd, assign(Rest, Tok->Next), Tok));
  
  *Rest = Tok;
  return Nd;
}

// 逻辑或
// logOr = logAnd ("||" logAnd)*
static Node *logOr(Token **Rest, Token *Tok) {
  Node *Nd = logAnd(&Tok, Tok);
  while (equal(Tok, "||")) {
    Token *Start = Tok;
    Nd = newBinary(ND_LOGOR, Nd, logAnd(&Tok, Tok->Next), Start);
  }
  *Rest = Tok;
  return Nd;
}

// 逻辑与
// logAnd = bitOr ("&&" bitOr)*
static Node *logAnd(Token **Rest, Token *Tok) {
  Node *Nd = bitOr(&Tok, Tok);
  while (equal(Tok, "&&")) {
    Token *Start = Tok;
    Nd = newBinary(ND_LOGAND, Nd, bitOr(&Tok, Tok->Next), Start);
  }
  *Rest = Tok;
  return Nd;
}

// 按位或
// bitOr = bitXor ("|" bitXor)*
static Node *bitOr(Token **Rest, Token *Tok) {
  Node *Nd = bitXor(&Tok, Tok);
  while (equal(Tok, "|")) {
    Token *Start = Tok;
    Nd = newBinary(ND_BITOR, Nd, bitXor(&Tok, Tok->Next), Start);
  }
  *Rest = Tok;
  return Nd;
}

// 按位异或
// bitXor = bitAnd ("^" bitAnd)*
static Node *bitXor(Token **Rest, Token *Tok) {
  Node *Nd = bitAnd(&Tok, Tok);
  while (equal(Tok, "^")) {
    Token *Start = Tok;
    Nd = newBinary(ND_BITXOR, Nd, bitAnd(&Tok, Tok->Next), Start);
  }
  *Rest = Tok;
  return Nd;
}

// 按位与
// bitAnd = equality ("&" equality)*
static Node *bitAnd(Token **Rest, Token *Tok) {
  Node *Nd = equality(&Tok, Tok);
  while (equal(Tok, "&")) {
    Token *Start = Tok;
    Nd = newBinary(ND_BITAND, Nd, equality(&Tok, Tok->Next), Start);
  }
  *Rest = Tok;
  return Nd;
}

// 解析相等性
// equality = relational ("==" relational | "!=" relational)*
static Node *equality(Token **Rest, Token *Tok) {
  // relational
  Node *Nd = relational(&Tok, Tok);

  // ("==" relational | "!=" relational)*
  while (true) {
    Token *Start = Tok;
    // "==" relational
    if (equal(Tok, "==")) {
      Nd = newBinary(ND_EQ, Nd, relational(&Tok, Tok->Next), Start);
      continue;
    }

    // "!=" relational
    if (equal(Tok, "!=")) {
      Nd = newBinary(ND_NE, Nd, relational(&Tok, Tok->Next), Start);
      continue;
    }

    *Rest = Tok;
    return Nd;
  }
}

// 解析比较关系
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
static Node *relational(Token **Rest, Token *Tok) {
  // add
  Node *Nd = add(&Tok, Tok);

  // ("<" add | "<=" add | ">" add | ">=" add)*
  while (true) {
    Token *Start = Tok;
    // "<" add
    if (equal(Tok, "<")) {
      Nd = newBinary(ND_LT, Nd, add(&Tok, Tok->Next), Start);
      continue;
    }

    // "<=" add
    if (equal(Tok, "<=")) {
      Nd = newBinary(ND_LE, Nd, add(&Tok, Tok->Next), Start);
      continue;
    }

    // ">" add
    // X>Y等价于Y<X
    if (equal(Tok, ">")) {
      Nd = newBinary(ND_LT, add(&Tok, Tok->Next), Nd, Start);
      continue;
    }

    // ">=" add
    // X>=Y等价于Y<=X
    if (equal(Tok, ">=")) {
      Nd = newBinary(ND_LE, add(&Tok, Tok->Next), Nd, Start);
      continue;
    }

    *Rest = Tok;
    return Nd;
  }
}

// 解析各种加法   num+num  or  ptr + num (num + ptr)
static Node *newAdd(Node *LHS, Node *RHS, Token *Tok) {
  // 为左右部添加类型
  addType(LHS);
  addType(RHS);

  // num + num
  if (isInteger(LHS->Ty) && isInteger(RHS->Ty))
    return newBinary(ND_ADD, LHS, RHS, Tok);

  // 不能解析 ptr + ptr
  if (LHS->Ty->Base && RHS->Ty->Base)
    errorTok(Tok, "invalid operands");

  // 将 num + ptr 转换为 ptr + num
  if (!LHS->Ty->Base && RHS->Ty->Base) {
    Node *Tmp = LHS;
    LHS = RHS;
    RHS = Tmp;
  }

  // ptr + num ==> ptr + num * 8
  // 指针加法，ptr+1，这里的1不是1个字节，而是1个元素的空间，所以需要 ×Size 操作
  // 指针用long类型存储
  RHS = newBinary(ND_MUL, RHS, newLong(LHS->Ty->Base->Size, Tok), Tok);
  return newBinary(ND_ADD, LHS, RHS, Tok);
}

// 解析各种减法  num - num  or prt - num  or ptr - ptr  else errorTok
static Node *newSub(Node *LHS, Node *RHS, Token *Tok) {
  // 为左右部添加类型
  addType(LHS);
  addType(RHS);

  // num - num
  if (isInteger(LHS->Ty) && isInteger(RHS->Ty))
    return newBinary(ND_SUB, LHS, RHS, Tok);

  // ptr - num  ==>  ptr - 8*num
  if (LHS->Ty->Base && isInteger(RHS->Ty)) {
    RHS = newBinary(ND_MUL, RHS, newLong(LHS->Ty->Base->Size, Tok), Tok);
    addType(RHS);
    Node *Nd = newBinary(ND_SUB, LHS, RHS, Tok);
    // 节点类型为指针
    Nd->Ty = LHS->Ty;
    return Nd;
  }

  // ptr - ptr，返回两指针间有多少元素  ==> (ptr - ptr)/8
  if (LHS->Ty->Base && RHS->Ty->Base) {
    Node *Nd = newBinary(ND_SUB, LHS, RHS, Tok);
    Nd->Ty = TyInt;
    return newBinary(ND_DIV, Nd, newNum(LHS->Ty->Base->Size, Tok), Tok);
  }

  errorTok(Tok, "invalid operands");
  return NULL;
}

// 解析加减
// add = mul ("+" mul | "-" mul)*
static Node *add(Token **Rest, Token *Tok) {
  // mul
  Node *Nd = mul(&Tok, Tok);

  // ("+" mul | "-" mul)*
  while (true) {
    Token *Start = Tok;
    // "+" mul
    if (equal(Tok, "+")) {
      Nd = newAdd(Nd, mul(&Tok, Tok->Next), Start);
      continue;
    }

    // "-" mul
    if (equal(Tok, "-")) {
      Nd = newSub(Nd, mul(&Tok, Tok->Next), Start);
      continue;
    }

    *Rest = Tok;
    return Nd;
  }
}

// 解析乘除
// mul = cast ("*" cast | "/" cast | "%" cast)*
static Node *mul(Token **Rest, Token *Tok) {
  // cast
  Node *Nd = cast(&Tok, Tok);

  // ("*" cast | "/" cast)*
  while (true) {
    Token *Start = Tok;
    // "*" cast
    if (equal(Tok, "*")) {
      Nd = newBinary(ND_MUL, Nd, cast(&Tok, Tok->Next), Start);
      continue;
    }

    // "/" cast
    if (equal(Tok, "/")) {
      Nd = newBinary(ND_DIV, Nd, cast(&Tok, Tok->Next), Start);
      continue;
    }

    // "%" cast
    if (equal(Tok, "%")) {
      Nd = newBinary(ND_MOD, Nd, cast(&Tok, Tok->Next), Start);
      continue;
    }

    *Rest = Tok;
    return Nd;
  }
}

// 解析类型转换
// cast = ("(" typeName ")" cast) | unary
static Node *cast(Token **Rest, Token *Tok) {
  // cast = "(" typeName ")" cast
  if (equal(Tok, "(") && isTypename(Tok->Next)) {
    Token *Start = Tok;
    Type *Ty = typename(&Tok, Tok->Next);
    Tok = skip(Tok, ")");
    // 解析嵌套的类型转换
    Node *Nd = newCast(cast(Rest, Tok), Ty);
    Nd->Tok = Start;
    return Nd;
  }

  // unary
  return unary(Rest, Tok);
}

// 解析一元运算
// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
//       | ("++" | "--") unary
//       | postfix
static Node *unary(Token **Rest, Token *Tok) {
  // "+" cast
  if (equal(Tok, "+"))
    return cast(Rest, Tok->Next);
  // "-" cast
  if (equal(Tok, "-"))
    return newUnary(ND_NEG, cast(Rest, Tok->Next), Tok);
  // "*" cast
  if (equal(Tok, "*"))
    return newUnary(ND_DEREF, cast(Rest, Tok->Next), Tok);
  // "-" cast
  if (equal(Tok, "&"))
    return newUnary(ND_ADDR, cast(Rest, Tok->Next), Tok);
  // "!" cast
  if (equal(Tok, "!"))
    return newUnary(ND_NOT, cast(Rest, Tok->Next), Tok);
  // "~" cast
  if (equal(Tok, "~"))
    return newUnary(ND_BITNOT, cast(Rest, Tok->Next), Tok);
  
  // 转换 ++i 为 i+=1
  // "++" unary
  if (equal(Tok, "++"))
    return toAssign(newAdd(unary(Rest, Tok->Next), newNum(1, Tok), Tok));

  // 转换 +-i 为 i-=1
  // "--" unary
  if (equal(Tok, "--"))
    return toAssign(newSub(unary(Rest, Tok->Next), newNum(1, Tok), Tok));
    
  // postfix
  return postfix(Rest, Tok);
}

// structMembers = (declspec declarator (","  declarator)* ";")* "}"
static void structMembers(Token **Rest, Token *Tok, Type *Ty) {
  Member Head = {};
  Member *Cur = &Head;

  while (!equal(Tok, "}")) {
    // declspec
    Type *BaseTy = declspec(&Tok, Tok, NULL);
    int First = true;

    while (!consume(&Tok, Tok, ";")) {
      if (!First)
        Tok = skip(Tok, ",");
      First = false;

      Member *Mem = calloc(1, sizeof(Member));
      // declarator
      Mem->Ty = declarator(&Tok, Tok, BaseTy);
      Mem->Name = Mem->Ty->Name;
      Cur = Cur->Next = Mem;
    }
  }

  *Rest = Tok->Next;
  Ty->Mems = Head.Next;
}

// structUnionDecl = ident? ("{" structMembers)?
static Type *structUnionDecl(Token **Rest, Token *Tok) {
  Token *Tag = NULL;
  if(Tok->Kind == TK_IDENT){
    Tag = Tok;
    Tok = Tok->Next;
  }
  if (Tag && !equal(Tok, "{")){  // 声明过struct
    Type *Ty = findTag(Tag);
    if(!Ty){
      errorTok(Tag, "unknown struct type");
    }
    *Rest = Tok;
    return Ty;
  }
  Tok = skip(Tok, "{");

  // 构造一个结构体
  Type *Ty = calloc(1, sizeof(Type));
  Ty->Kind = TY_STRUCT;
  structMembers(Rest, Tok, Ty);
  Ty->Align = 1;

  // 如果有名称就注册结构体类型
  if (Tag)
    pushTagScope(Tag, Ty);
  return Ty;
}  

// structDecl = structUnionDecl
static Type *structDecl(Token **Rest, Token *Tok) {
  Type *Ty = structUnionDecl(Rest, Tok);

  // 计算结构体内成员的偏移量
  int Offset = 0;
  for (Member *Mem = Ty->Mems; Mem; Mem = Mem->Next) {
    Offset = alignTo(Offset, Mem->Ty->Align);
    Mem->Offset = Offset;
    Offset += Mem->Ty->Size;
    // struct的默认对齐为1, 存在member则为member中的最大对齐
    if (Ty->Align < Mem->Ty->Align)
      Ty->Align = Mem->Ty->Align;
  }
  Ty->Size = alignTo(Offset, Ty->Align);
  return Ty;
}

// unionDecl = structUnionDecl
static Type *unionDecl(Token **Rest, Token *Tok) {
  Type *Ty = structUnionDecl(Rest, Tok);
  Ty->Kind = TY_UNION;

  // 联合体需要设置为最大的对齐量与大小，变量偏移量都默认为0
  for (Member *Mem = Ty->Mems; Mem; Mem = Mem->Next) {
    if (Ty->Align < Mem->Ty->Align)
      Ty->Align = Mem->Ty->Align;
    if (Ty->Size < Mem->Ty->Size)
      Ty->Size = Mem->Ty->Size;
  }
  // 将大小对齐
  Ty->Size = alignTo(Ty->Size, Ty->Align);
  return Ty;
}

// 获取结构体成员 遍历Mems
static Member *getStructMember(Type *Ty, Token *Tok) {
  for (Member *Mem = Ty->Mems; Mem; Mem = Mem->Next)
    if (Mem->Name->Len == Tok->Len &&
        !strncmp(Mem->Name->Loc, Tok->Loc, Tok->Len))
      return Mem;
  errorTok(Tok, "no such member");
  return NULL;
}

// 构建结构体成员的节点
static Node *structRef(Node *LHS, Token *Tok) {
  addType(LHS);
  if (LHS->Ty->Kind != TY_STRUCT && LHS->Ty->Kind != TY_UNION)
    errorTok(LHS->Tok, "not a struct nor a union");

  Node *Nd = newUnary(ND_MEMBER, LHS, Tok);
  Nd->Mem = getStructMember(LHS->Ty, Tok);
  return Nd;
}

// 转换 A++ 为 `(typeof A)((A += 1) - 1)`
// Increase Decrease
static Node *newIncDec(Node *Nd, Token *Tok, int Addend) {
  addType(Nd);
  return newCast(newSub(toAssign(newAdd(Nd, newNum(Addend, Tok), Tok)),
                        newNum(Addend, Tok), Tok),
                 Nd->Ty);
}

// postfix = primary ("[" expr "]" | "." ident | "->" ident | "++" | "--")*
static Node *postfix(Token **Rest, Token *Tok) {
  // primary
  Node *Nd = primary(&Tok, Tok);  //primary(Rest, Tok);  rest之后在末尾会使用

  // ("[" expr "]" | "." ident)*
  while (true) {
    // "[" expr "]"
    if (equal(Tok, "[")) {
      // x[y] 等价于 *(x+y)
      Token *Start = Tok;
      Node *Idx = expr(&Tok, Tok->Next);
      Tok = skip(Tok, "]");
      Nd = newUnary(ND_DEREF, newAdd(Nd, Idx, Start), Start);
      continue;
    }

    // "." ident
    if (equal(Tok, ".")) {
      Nd = structRef(Nd, Tok->Next);
      Tok = Tok->Next->Next;
      continue;
    }

    // "->" ident
    if (equal(Tok, "->")) {
      // x->y 等价于 (*x).y
      Nd = newUnary(ND_DEREF, Nd, Tok);
      Nd = structRef(Nd, Tok->Next);
      Tok = Tok->Next->Next;
      continue;
    }

    if (equal(Tok, "++")) {
      Nd = newIncDec(Nd, Tok, 1);
      Tok = Tok->Next;
      continue;
    }

    if (equal(Tok, "--")) {
      Nd = newIncDec(Nd, Tok, -1);
      Tok = Tok->Next;
      continue;
    }

    *Rest = Tok;
    return Nd;
  }
}

// 解析括号、数字、变量
// primary = "(" "{" stmt+ "}" ")"
//         | "(" expr ")"
//         | "sizeof" "(" typeName ")"
//         | "sizeof" unary
//         | ident funcArgs?
//         | str
//         | num
static Node *primary(Token **Rest, Token *Tok) {
  Token *Start = Tok;
  
  // "(" "{" stmt+ "}" ")"
  if (equal(Tok, "(") && equal(Tok->Next, "{")) {
    // This is a GNU statement expresssion.
    Node *Nd = newNode(ND_STMT_EXPR, Tok);
    //  stmt+ "} ==> compoundStmt
    Nd->Body = compoundStmt(&Tok, Tok->Next->Next)->Body;
    *Rest = skip(Tok, ")");
    return Nd;
  }
  // "(" expr ")"
  if (equal(Tok, "(")) {
    Node *Nd = expr(&Tok, Tok->Next);
    *Rest = skip(Tok, ")");
    return Nd;
  }

  // num
  if (Tok->Kind == TK_NUM) {
    Node *Nd = newNum(Tok->Val, Tok);
    *Rest = Tok->Next;
    return Nd;
  }

  // ident args?
  if (Tok->Kind == TK_IDENT){
    // 函数调用
    // args = "(" ")"
    if (equal(Tok->Next, "(")) { // 下一个为 "(", 当前是 标识符
      return funCall(Rest, Tok);
    }
    // 查找变量（或枚举常量）
    VarScope *S = findVar(Tok);
    // 如果变量（或枚举常量）不存在，就在链表中新增一个变量
    if(!S || (!S->Var && !S->EnumTy)){
      errorTok(Tok, "undefined variable");
    }
    Node *Nd;
    if(S->Var){
      Nd = newVarNode(S->Var, Tok);
    }else{
      Nd = newNum(S->EnumVal, Tok);
    }
    *Rest = Tok->Next;
    return Nd;
  }

  // "sizeof" "(" typeName ")"
  if (equal(Tok, "sizeof") && equal(Tok->Next, "(") &&
      isTypename(Tok->Next->Next)) {
    Type *Ty = typename(&Tok, Tok->Next->Next);
    *Rest = skip(Tok, ")");
    return newNum(Ty->Size, Start);
  }

  // "sizeof" unary
  if (equal(Tok, "sizeof")){
    Node *Nd = unary(Rest, Tok->Next); // 跳过sizeof, 防止变函数 sizeof函数
    addType(Nd);
    return newNum(Nd->Ty->Size, Tok);
  }

  if (Tok->Kind == TK_STR){
    Obj *Var = newStringLiteral(Tok->Str, Tok->Ty);
    *Rest = Tok->Next;
    return newVarNode(Var, Tok);
  }

  errorTok(Tok, "expected an expression");
  return NULL;
}

// 解析类型别名
static Token *parseTypedef(Token *Tok, Type *BaseTy) {
  bool First = true;

  while (!consume(&Tok, Tok, ";")) {
    if (!First)
      Tok = skip(Tok, ",");
    First = false;

    Type *Ty = declarator(&Tok, Tok, BaseTy);
    // 类型别名的变量名存入变量域中，并设置类型
    pushScope(getIdent(Ty->Name))->Typedef = Ty;
  }
  return Tok;
}

// 解析函数调用
// funcall = ident "(" (assign ("," assign)*)? ")"
static Node *funCall(Token **Rest, Token *Tok) {
  Token *Start = Tok;
  Tok = Tok->Next->Next;  // ident "(" 

  // 查找函数名
  VarScope *S = findVar(Start);
  if (!S)
    errorTok(Start, "implicit declaration of a function");
  if (!S->Var || S->Var->Ty->Kind != TY_FUNC)
    errorTok(Start, "not a function");

  // 函数名的类型
  Type *Ty = S->Var->Ty;
  // 函数形参的类型
  Type *ParamTy = Ty->Params;

  Node Head = {};
  Node *Cur = &Head;

  while (!equal(Tok, ")")) {
    if (Cur != &Head)
      Tok = skip(Tok, ",");
    // assign
    Node *Arg = assign(&Tok, Tok);
    addType(Arg);

    if(ParamTy){
      if (ParamTy->Kind == TY_STRUCT || ParamTy->Kind == TY_UNION)
        errorTok(Arg->Tok, "passing struct or union is not supported yet");
      // 将参数节点的类型进行转换
      Arg = newCast(Arg, ParamTy);
      // 前进到下一个形参类型
      ParamTy = ParamTy->Next;
    }
    // 对参数进行存储
    Cur->Next = Arg;
    Cur = Cur->Next;
    addType(Cur);  // 上面访问 Ty了
  }

  *Rest = skip(Tok, ")");

  Node *Nd = newNode(ND_FUNCALL, Start);
  // ident
  Nd->FuncName = strndup(Start->Loc, Start->Len);
  // 函数类型
  Nd->FuncType = Ty;
  // 读取的返回类型
  Nd->Ty = Ty->ReturnTy;
  Nd->Args = Head.Next;
  return Nd;
}

// 将形参添加到Locals
static void createParamLVars(Type *Param) {
  if (Param) {
    // 递归到形参最底部
    // 先将最底部的加入Locals中，之后的都逐个加入到顶部，保持顺序不变
    createParamLVars(Param->Next);
    // 添加到Locals中
    newLVar(getIdent(Param->Name), Param);
  }
}

// functionDefinition = declarator ("{" compoundStmt | ";" )
static Token *function(Token *Tok, Type *BaseTy, VarAttr *Attr) {
  Type *Ty = declarator(&Tok, Tok, BaseTy);

  Obj *Fn = newGVar(getIdent(Ty->Name), Ty);  // 函数为全局变量
  Fn->IsFunction = true;
  Fn->IsDefinition = !consume(&Tok, Tok, ";");
  Fn->IsStatic = Attr->IsStatic;

  // 判断是否没有函数定义
  if (!Fn->IsDefinition)
    return Tok;

  CurrentFn = Fn;

  // 清空本地变量Locals
  Locals = NULL;
  // 进入新的域
  enterScope();
  // 函数参数
  createParamLVars(Ty->Params);
  Fn->Params = Locals;

  Tok = skip(Tok, "{");
  // 函数体存储语句的AST，Locals存储变量
  Fn->Body = compoundStmt(&Tok, Tok);
  // addType(Fn->Body);   // TODO CRUX 就tm这一句 卧槽卧槽卧槽   不知道什么时候删了 真特么啥币
  Fn->Locals = Locals;
  // 结束当前域
  leaveScope();
  return Tok;
}
// int
// a,b[10],*c;
// 构造全局变量
// global-variable = declarator?("," declarator)* ";"
static Token *globalVariable(Token *Tok, Type *Basety) {
  bool First = true;

  while (!consume(&Tok, Tok, ";")) {
    if (!First)
      Tok = skip(Tok, ",");
    First = false;

    Type *Ty = declarator(&Tok, Tok, Basety);
    newGVar(getIdent(Ty->Name), Ty);
  }
  return Tok;
}

// 区分 函数还是全局变量
static bool isFunction(Token *Tok) {
  if (equal(Tok, ";"))  // int;
    return false;

  // 虚设变量，用于调用declarator, 判断下后面的类型
  Type Dummy = {};
  Type *Ty = declarator(&Tok, Tok, &Dummy);
  return Ty->Kind == TY_FUNC;
}

// 语法解析入口函数
// program = (typedef | functionDefinition* | global-variable)*
Obj *parse(Token *Tok) {
  Globals = NULL;
  while (Tok->Kind != TK_EOF) {
    VarAttr Attr = {};
    Type *BaseTy = declspec(&Tok, Tok, &Attr);

    // typedef
    if (Attr.IsTypedef) {
      Tok = parseTypedef(Tok, BaseTy);
      continue;
    }
    // 函数
    if(isFunction(Tok)){
      Tok = function(Tok, BaseTy, &Attr);
    }else{
      Tok = globalVariable(Tok, BaseTy);
    }
  }
  return Globals;
}
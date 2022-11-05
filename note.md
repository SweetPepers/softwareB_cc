​	shell 别有事没事加空格

fprintf(stderr, "%*s", Pos, "");   打印了pos個空格
### 4 token流 构造

```c
// 为每个终结符都设置种类来表示
typedef enum
{
    TK_PUNCT, // 操作符如： + - 
    TK_NUM,   // 数字
    TK_EOF,   // 文件终止符，即文件的最后
} TokenKind;

// 终结符结构体
typedef struct Token Token;
struct Token
{
    TokenKind Kind; // 种类
    Token *Next;    // 指向下一终结符
    int Val;        // 值
    char *Loc;      // 在解析的字符串内的位置
    int Len;        // 长度
};

```
跳过空格啥的

### 5 支持 * / () 也就是优先级
简单的加减乘除的抽象语法树  加上括号  表示优先级
![](./picture/%E5%9B%9B%E5%88%99%E8%BF%90%E7%AE%97%E8%AF%AD%E6%B3%95%E6%A0%91.jpg)

```c
// AST节点种类
typedef enum {
  ND_ADD, // +
  ND_SUB, // -
  ND_MUL, // *
  ND_DIV, // /
  ND_NUM, // (整型 int)  数字
} NodeKind;


// AST中二叉树节点
typedef struct Node Node;
struct Node {
  NodeKind Kind; // 节点种类
  Node *LHS;     // 左部，left-hand side
  Node *RHS;     // 右部，right-hand side
  int Val;       // 存储ND_NUM种类的值
};

// expr = mul ("+" mul | "-" mul)*
// mul = primary ("*" primary | "/" primary)*
// primary = "(" expr ")" | num
static Node *expr(Token **Rest, Token *Tok);
static Node *mul(Token **Rest, Token *Tok);
static Node *primary(Token **Rest, Token *Tok);

static void genExpr(Node *Nd) 
// 递归将最右节点入栈  解析完左子树之后弹出
```

### 6 一元运算符
一元运算符优先级高于乘除

```c
// expr = mul ("+" mul | "-" mul)*
// mul = unary ("*" unary | "/" unary)*
// unary = ("+" | "-") unary | primary
// primary = "(" expr ")" | num
```

### 7 == != > < >= <=
优先级
```c

// 优先级
// expr = equality
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
// mul = unary ("*" unary | "/" unary)*
// unary = ("+" | "-") unary | primary
// primary = "(" expr ")" | num
```

判断操作符占几个字节(1 or 2)
```c
// 判断Str是否以SubStr开头
static bool startsWith(char *Str, char *SubStr) {
  // 比较LHS和RHS的N个字符是否相等
  return strncmp(Str, SubStr, strlen(SubStr)) == 0;
}

// 读取操作符
static int readPunct(char *Ptr) {
  // 判断2字节的操作符
  if (startsWith(Ptr, "==") || startsWith(Ptr, "!=") || startsWith(Ptr, "<=") ||
      startsWith(Ptr, ">="))
    return 2;

  // 判断1字节的操作符
  return ispunct(*Ptr) ? 1 : 0;
}
```


### 8 代码重构, 将main分割为多个文件
- `codegen.c` 语义分析与代码生成  通过栈操作解析语法树生成代码
- `parse.c`   生成AST, 根据token序列生成抽象语法树   语法分析
- `tokenize.c`将输入字符串解析为一个一个token   词法分析


### 9 支持;分隔语句

- cc.h 里面
添加NodeKind::ND_EXPR_STMT, // 表达式语句 : 表示是一个语句

- parse.h 添加新语法规则
语法规则
```c
// program = stmt*
// stmt = exprStmt
// exprStmt = expr ";"
// expr = equality
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
// mul = unary ("*" unary | "/" unary)*
// unary = ("+" | "-") unary | primary
// primary = "(" expr ")" | num

```
program由多个表达式语句构成, 采用链表存储多个语句
```c
struct Node {
  // new!!
  Node *Next;    // 下一节点，指代下一语句
};
```
- codegen.c
生成多个语句, 为每个语句生成代码


### 10 支持单字母本地变量
- cc.h
  - add TokenKind::TK_IDENT  标记符
  - add NodeKind::ND_ASSIGN  赋值 NodeKind::ND_VAR 变量
  - add Node::char Name 变量名字

- tokenize.c
a-z 自动识别为变量

- parse.c
```c
// 语法
// program = stmt*
// stmt = exprStmt
// exprStmt = expr ";"
// expr = assign                                     new
// assign = equality ("=" assign)?                   new
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
// mul = unary ("*" unary | "/" unary)*
// unary = ("+" | "-") unary | primary
// primary = "(" expr ")" | ident | num             new
```
primary() 
```c
  // ident
  if (Tok->Kind == TK_IDENT){
    Node *Nd = newVarNode(*(Tok->Loc));  // 用字符位置
    *Rest = Tok->Next;
    return Nd;
  }
```


- codegen.c
入口函数初始化栈, 自动在栈上生成24个变量 a-z, 并存储a的地址fp
`Offset = (Nd->Name - 'a' + 1) * 8; `
之后变量的地址就是 
`addi a0, fp, %d (-Offset) `
最后释放


### 11 支持多字母本地变量
- cc.h
定义变量的结构体
```c
// 本地变量
typedef struct Obj Obj;
struct Obj {
  Obj *Next;  // 指向下一对象
  char *Name; // 变量名
  int Offset; // fp的偏移量
};

// 不再以node为入口, 函数由语法树及其附属结构(变量表)组成 
// 函数  
typedef struct Function Function;
struct Function {
  Node *Body;    // 函数体
  Obj *Locals;   // 本地变量
  int StackSize; // 栈大小
};

```
- tokennize.c
判断变量名是否合法

```c
// 解析标记符  [a-zA-Z_][a-zA-Z0-9_]*
    if (isIdent1(*P)){
      char *Start = P;
      do{
        ++P;
      }while(isIdent2(*P));


// 判断标记符的首字母规则
// [a-zA-Z_]
static bool isIdent1(char C) {
  // a-z与A-Z在ASCII中不相连，所以需要分别判断
  return ('a' <= C && C <= 'z') || ('A' <= C && C <= 'Z') || C == '_';
}

// 判断标记符的非首字母的规则
// [a-zA-Z0-9_]
static bool isIdent2(char C) { return isIdent1(C) || ('0' <= C && C <= '9'); }

```

- parse.c
因为变量由 char升级为 Obj, 做一些相应的修改
```c
// 维持一个链表结构存储本地变量名
Obj *Locals;
// 通过名称，查找一个本地变量
static Obj *findVar(Token *Tok);

// 新变量
static Node *newVarNode(Obj *Var);
// 在链表中新增一个变量
static Obj *newLVar(char *Name);
```

- codegen.c

`static void assignLVarOffsets(Function *Prog) `
根据链表长度计算初始化栈的大小, 并对齐16位`Prog->StackSize = alignTo(Offset, 16)`, 修改初始化栈大小


### 12 支持return语句
目前成果 
```c
foo2=70; bar4=4;return foo2+bar4;

编译结果:
  .globl main
main:
  addi sp, sp, -8
  sd fp, 0(sp)
  mv fp, sp
  addi sp, sp, -16
  addi a0, fp, -16
  addi sp, sp, -8
  sd a0, 0(sp)
  li a0, 70
  ld a1, 0(sp)
  addi sp, sp, 8
  sd a0, 0(a1)
  addi a0, fp, -8
  addi sp, sp, -8
  sd a0, 0(sp)
  li a0, 4
  ld a1, 0(sp)
  addi sp, sp, 8
  sd a0, 0(a1)
  addi a0, fp, -8
  ld a0, 0(a0)
  addi sp, sp, -8
  sd a0, 0(sp)
  addi a0, fp, -16
  ld a0, 0(a0)
  ld a1, 0(sp)
  addi sp, sp, 8
  add a0, a0, a1
  j .L.return
.L.return:
  mv sp, fp
  ld fp, 0(sp)
  addi sp, sp, 8
  ret
```

- cc.h
  - TokenKind::TK_KEYWORD
  - NodeKind::ND_RETURN
- tokenize.c
  void convertKeywords(Token *Tok) : 扫描token, 将 字符为`return`的token的Kind换为TK_KEYWORD
- parse.c
  语法更新
  `stmt = "return" expr ";" | exprStmt`
  为returntoken建立单叉树

- codegen.c
  现在stmt由return语句或者exprstmt组成, 写一个switch分别翻译, 并在epilogue上加入 `.L.return`的跳转标签

### 13 支持{...}代码块


- rcvv.h
  - NodeKind::ND_BLOCK
  - Node::Body // 代码块

- parse.c
  // program = "{" compoundStmt
  // compoundStmt = stmt* "}"
  // stmt = "return" expr ";" | "{" compoundStmt | exprStmt
  // exprStmt = expr ";"

  parse()  为{}, 必须以{开始, 到}结束, 多个stmt用链表存储
- codegen.c
  ```c
  genStmt()
   switch(Nd->Kind){
    // 生成代码块，遍历代码块的语句链表
    case ND_BLOCK: 从Nd->Body开始一个一个按照stmt解析
    ...
    }
  ```

### 14 支持空语句 ;;;
// exprStmt = expr? ";"
- parse.c
```c
  // ";"
  if (equal(Tok, ";")){
    *Rest = Tok->Next;
    return newNode(ND_BLOCK);
  }
```


### 15 if
- cc.h
  - NodeKind::ND_IF
  - Node
    - Node *Cond; // 条件内的表达式
    - Node *Then; // 符合条件后的语句
    - Node *Els;  // 不符合条件后的语句
- tokenize.c
  关键字不再只有return, isKeyWord(Token *Tok) 判断是否为关键字
- parse.c
  ```c
  // stmt = "return" expr ";"
  //        | "if" "(" expr ")" stmt ("else" stmt)?
  //        | "{" compoundStmt
  //        | exprStmt
  if "if":
    Node *Nd = newNode(ND_IF);
    并根据token对 Cond, Then, Els 赋值
  ```
  
- codegen.c
static int count 记录每个if else


if 解释规则
```c
  case ND_IF: 
    // 代码段计数
    int C = count();
    // 生成条件内语句
    genExpr(Nd->Cond);
    // 判断结果是否为0，为0则跳转到else标签
    printf("  beqz a0, .L.else.%d\n", C);
    // 生成符合条件后的语句
    genStmt(Nd->Then);
    // 执行完后跳转到if语句后面的语句
    printf("  j .L.end.%d\n", C);
    // else代码块，else可能为空，故输出标签
    printf(".L.else.%d:\n", C);
    // 生成不符合条件后的语句
    if (Nd->Els)
      genStmt(Nd->Els);
    // 结束if语句，继续执行后面的语句
    printf(".L.end.%d:\n", C);
    return;`
```


### 16 for
- cc.h
  ND_FOR
  - Node
    - Node *Init; // 初始化语句
    - Node *Inc;  // 递增语句
- tokenize.c 
  简单把isKeyword 加上一个for就行
- parse.c
```c
  // stmt = "return" expr ";"
  //        | "if" "(" expr ")" stmt ("else" stmt)?
  //        | "for" "(" exprStmt expr? ";" expr? ")" stmt
  //        | "{" compoundStmt
  //        | exprStmt
```
 `"for" "(" exprStmt expr? ";" expr? ")" stmt`
```c
  Nd->Init = exprStmt
  Nd->Cond = expr? ";"
  Nd->Inc = expr?
  Nd->Then = stmt
```
- codegen.c
`{j = 0;for (i=0; i<=10; i=i+1) j=i+j; return j; } => 55`
```
  .globl main
main:
  addi sp, sp, -8
  sd fp, 0(sp)
  mv fp, sp
  addi sp, sp, -16
  addi a0, fp, -16
  addi sp, sp, -8
  sd a0, 0(sp)
  li a0, 0
  ld a1, 0(sp)
  addi sp, sp, 8
  sd a0, 0(a1)
  addi a0, fp, -8
  addi sp, sp, -8
  sd a0, 0(sp)
  li a0, 0             # init 
  ld a1, 0(sp)
  addi sp, sp, 8
  sd a0, 0(a1)
.L.begin.1:            # Cond
  li a0, 10
  addi sp, sp, -8
  sd a0, 0(sp)
  addi a0, fp, -8
  ld a0, 0(a0)
  ld a1, 0(sp)
  addi sp, sp, 8
  slt a0, a1, a0
  xori a0, a0, 1
  beqz a0, .L.end.1
  addi a0, fp, -16     # Then
  addi sp, sp, -8
  sd a0, 0(sp)
  addi a0, fp, -16
  ld a0, 0(a0)
  addi sp, sp, -8
  sd a0, 0(sp)
  addi a0, fp, -8
  ld a0, 0(a0)
  ld a1, 0(sp)
  addi sp, sp, 8
  add a0, a0, a1
  ld a1, 0(sp)
  addi sp, sp, 8
  sd a0, 0(a1)
  addi a0, fp, -8
  addi sp, sp, -8
  sd a0, 0(sp)
  li a0, 1
  addi sp, sp, -8
  sd a0, 0(sp)
  addi a0, fp, -8
  ld a0, 0(a0)
  ld a1, 0(sp)
  addi sp, sp, 8
  add a0, a0, a1
  ld a1, 0(sp)
  addi sp, sp, 8
  sd a0, 0(a1)
  j .L.begin.1
.L.end.1:             # end
  addi a0, fp, -16
  ld a0, 0(a0)
  j .L.return
.L.return:
  mv sp, fp
  ld fp, 0(sp)
  addi sp, sp, 8
  ret
```

### 17 while
while 本质上和for一样
```c
// stmt = "return" expr ";"
//        | "if" "(" expr ")" stmt ("else" stmt)?
//        | "for" "(" exprStmt expr? ";" expr? ")" stmt
//        | "while" "(" expr ")" stmt
//        | "{" compoundStmt
//        | exprStmt
```

### 18 更新辅助信息
简单的把代码生成部分每段打印个注释

### 19 为节点添加相应的终结符，以改进报错信息
AST中每个节点添加 Tok结构

QUESTION:

解析正则表达式带*的  怎么停止的??
`add = mul ("+" mul | "-" mul)*`
```c 
static Node *add(Token **Rest, Token *Tok) {
  // mul
  Node *Nd = mul(&Tok, Tok);

  // ("+" mul | "-" mul)*
  while (true) {
    Token *Start = Tok;
    // "+" mul
    if (equal(Tok, "+")) {
      Nd = newBinary(ND_ADD, Nd, mul(&Tok, Tok->Next), Start);
      continue;
    }

    // "-" mul
    if (equal(Tok, "-")) {
      Nd = newBinary(ND_SUB, Nd, mul(&Tok, Tok->Next), Start);
      continue;
    }

    *Rest = Tok;
    return Nd;  // 瞎了艹, return在while里面, 之前还看清楚的
  }
}

```
### 20 支持一元& *运算符
主要难点在于代码生成节点

- cc.h
  添加两个node类型 ND_ADDR, ND_DEREF(dereference)
- parse.c 
  新语法`unary = ("+" | "-" | "*" | "&") unary | primary`简单在unary加两个类型即可
- codegen.c
  对于取地址函数, 目前只有变量存在地址, 且该函数(genAddr)的调用者只有genExpr, 
  - 对于*解引用, 将节点左部的结果按照expr生成代码,结果存入a0, 然后将a0当地址来使用 `ld a0, 0(a0)`
  - 对于&取址, 直接把左部的地址存入a0即可, 现在的左部可以为变量,或者解引用*语句, &*直接怼掉, 直接解析后面的Nd->LHS即可 


### 21 支持指针的算术运算
类型改变了语法但**暂时**和代码生成没有关系, 所有类型大小目前都为8
目前的类型只有 指针及int类型  TY_INT, // int整型    TY_PTR, // 指针

- cc.h
AST节点中添加type信息  `Type *Node::Ty`

类型系统需要的数据结构
```c
typedef enum {
  TY_INT, // int整型
  TY_PTR, // 指针
} TypeKind;

struct Type{
  TypeKind Kind;  // 种类
  Type *Base;     // 指向的类型
};

// 全局变量
extern Type *TyInt;
```
需要的函数
```c
// 判断是否为整型
bool isInteger(Type *TY);
// 为节点内的所有节点添加类型
void addType(Node *Nd);
```

添加新文件
- type.c
函数
  - `Type *pointerTo(Type *Base)` 生成一个指向Base的TY_PTR的 Type结构
  - `void addType(Node *Nd)` 递归给语法树中的节点添加类型 先递归再判断类型
  ```c
    // 递归访问所有节点以增加类型
    addType(Nd->LHS);
    addType(Nd->RHS);
    addType(Nd->Cond);
    addType(Nd->Then);
    addType(Nd->Els);
    addType(Nd->Init);
    addType(Nd->Inc);

    // 访问链表内的所有节点以增加类型
    for (Node *N = Nd->Body; N; N = N->Next)
      addType(N);
  ```
  判断类型
    - ND_ADD, ND_SUB, ND_MUL, ND_DIV, ND_NEG, ND_ASSIGN: 设置为左值的类型
    - ND_EQ, ND_NE, ND_LT, ND_LE, ND_VAR, ND_NUM: int类型
    - ND_ADDR: & 使用pointerTo函数 设置为指向左值类型的ND_PTR类型
    - ND_DEREF:* 解引用, 左值要是指针, 设置为指针指向内容(`Nd->LHS->Ty->Base`)的类型, 否则设置为int, 嵌套(`&***&&(int)`)也会嵌套下去

- parse.c
  主要针对指针算术运算(+ or -)以及节点的类型做了修改
  - 节点类型, 在构造完AST后, 用`addType`函数为节点添加类型信息, 所有类型大小目前都为8
  newAdd(newSub)替换原来的直接构建, newAdd(newSub)判断左部右部的类型进行响应的修改, 同时注意, 此步骤需要在构建AST中就需要知道节点类型, 所以进入函数需要先
  ```c
    addType(LHS);
    addType(RHS);
  ```
  - add:
    - num+num: 正常构建
    - ptr+ptr: 错误
    - ptr + num (num + ptr) ==> ptr + num * 8
  - sub:
    - num-num : 正常构造
    - ptr - num  ==>  ptr - 8*num
    - ptr - ptr : 返回两只真之间的元素数量: ==> (ptr - ptr)/8

### 22 支持int关键字以及定义变量
发生了一个段错误哈哈
测试`{ int i=0; while(i<10) { i=i+1; } return i; }`
在parse.c:393, 此时Loc = `$1 = 0x7fffffffdf38 "+1; } return i; }"` 出错
右子树出错了, 右子树没有赋值
```c
(gdb) p RHS->Ty
$3 = (Type *) 0x0
```

破案了: parse.c::addType()函数, 忘记加入对于ND_NUM类型的type赋值了(自己删的哈哈)

- cc.h
给变量 Obj加入类型 `Type *Obj::Ty`
给type加入name  `Token* Type::Name` (TODO : 为什么是Token类型的??)

- type.c
  ND_VAR的type不再直接设置为INT, 改为Nd->Var->Ty
  另外解引用只能对指针进行, 不再对INT适用
- token.c
  - 添加函数 `consume()`:消耗指定的Token, 返回`true/false`, 与`skip()`相比, 不强制要求存在(`skip()`不存在会直接报错, 退出程序)
- parse.c
  语法添加声明
  ```c
    // compoundStmt = (declaration | stmt)* "}"
    // declaration =
    //         declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
    // declspec = "int"
    // declarator = "*"* ident
  ```
  新语法对compoundStmt小幅修改, 若开头为 `"int"`则进入`declaration()`
  - `declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"`
    `              int         a          =  expr     ,     b         =  expr2  ;`
    `int a, b = 10;`
    `int;` 可以单独出现???  TODO : 搞懂这一句  
    改成`declaration = declspec declarator ("=" expr)? ("," declarator ("=" expr)?)* ";"`这样不就可以了吗??
  - `Node *declaration(Token **Rest, Token *Tok)`
    这里把每个声明短句都看作一个句子, 就是一行声明语句可能为多个句子, 用list存储一下, 存储到block里面
    类似于这种
    ```c
    int {
      a = 1,
      b = 2, 
      c = 3,
      d = 4
    };
    ```
    左边为VAR, 右边为assign
    ```c
    // 解析“=”后面的Token
    Node *LHS = newVarNode(Var, Ty->Name);
    // 解析递归赋值语句
    Node *RHS = assign(&Tok, Tok->Next);
    Node *Node = newBinary(ND_ASSIGN, LHS, RHS, Tok);
    // 存放在表达式语句中
    Cur->Next = newUnary(ND_EXPR_STMT, Node, Tok);
    ```
  - `Type *declarator(Token **Rest, Token *Tok, Type *Ty)`// declarator = "*"* ident
  - `char *getIdent(Token *Tok)` Obj *Var = newLVar(getIdent(Ty->Name), Ty), 就获取一下变量的名字


### 插曲 episode
```sh
cat <<EOF | riscv64-linux-gnu-gcc -xc -c -o tmp2.o -
int ret3() { return 3; }
int ret5() { return 5; }
EOF
```
cat 输出多行的方法
```bash
cat <<EOF
content 内容
EOF
```
然后加上管道就变成了后面的输入数据

### 23 支持零参数函数调用
主要是test.sh `cat <<EOF` 的用法
语法  注意是函数调用, 不是函数声明
// primary = "(" expr ")" | ident args? | num
// args = "(" ")"
- cc.h
  添加 `NodeKind::ND_FUNCALL`
  添加 `Node::char *FuncName; // 函数名`
- parse.c 根据新语法简单修改即可
- codegen.c  加一个`call %s\n", Nd->FuncName`
  然后 fp之外多一个ra寄存器, sp初始减16


### 24 支持最多6个参数的函数调用
对于 fun语法 核心两个指针
- cc.h
  ```c
  struct Node{
    // 函数调用
    char *FuncName; // 函数名
    Node *Args; // 函数参数  new
  }
  ```
- parse.c
  语法更新为
  ```c
  // primary = "(" expr ")" | ident func-args? | num
  
  // funcall = ident "(" (assign ("," assign)*)? ")"
  ```
  解析primary时, 当前`Tok->Kind == TK_IDENT && Tok->Next == ")"`则进入 `funcall()`
  多条语句都用链表存储, 结果放入Nd->Args中
- codegen.c
  添加6个寄存器对应调用参数, 解析`Args`时, 数数量, 并将结果依次压入栈中然后倒着pop出来
  第1-6个参数分别对应`"a0", "a1", "a2", "a3", "a4", "a5"`


### 25 支持零参数函数定义
新的语法
// program = functionDefinition*
// functionDefinition = declspec declarator? ident "(" ")" "{" compoundStmt*
// declspec = "int"
// declarator = "*"* ident typeSuffix
// compoundStmt = (declaration | stmt)* "}"
// typeSuffix = ("(" ")")?

```c
int main(){

}

```
- cc.h
  添加类型:TypeKind::TY_FUNC
  仿照变量类型Obj改Function, 多个函数,增加`char *Name;      // 函数名` 和 `Function *Next;  // 下一函数`
  Type增加  `Type *ReturnTy; // 函数返回的类型`
- type.c
  `Type *funcType(Type *ReturnTy)`新建一个有ReturnTy的TY_FUNC的Type
- parse.c
  新语法
  ```c
  // program = functionDefinition*
  // functionDefinition = declspec declarator"{" compoundStmt*
  // declspec = "int"
  // declarator = "*"* ident typeSuffix
  // compoundStmt = (declaration | stmt)* "}"
  // typeSuffix = ("(" ")")?
  ```
  parse主体改为function
  `functionDefinition = declspec declarator"{" compoundStmt*`
  function中给Fn的`Name` `Body` `Locals`赋值
- codegen.c
  因为改为了多个函数, 所以要为每个函数分配参数啥的
  每个函数各自计算变量的offset
  然后用一个地址空间, 不过不用担心, 栈的赋值是相对赋值
  ```c
  printf("  addi sp, sp, -16\n"); // 分配两个位置
  printf("  sd ra, 8(sp)\n");
  // 将fp压入栈中，保存fp的值
  printf("  sd fp, 0(sp)\n");
  // 将sp写入fp
  printf("  mv fp, sp\n");
  ```
  ```c
  // 代码生成入口函数，包含代码块的基础信息
  void codegen(Function *Prog) {
    assignLVarOffsets(Prog);
    // 为每个函数单独生成代码
    for (Function *Fn = Prog; Fn; Fn = Fn->Next) {
      genFun(Fn);
    }
  }
  ```

### 26 支持最多6个参数的函数定义
增加参数的函数调用,本质上就是解析完括号中的参数, 然后将形参加入本地变量, 代码生成的时候, 将a0-a5寄存器的值存入sp中的参数地址中

- cc.h
  - `Type`::Type *Params;   // 形参  Type::Type *Next;     // 下一类型
  - `Function`::Obj *Params; // 形参
- type.c
  addtype时, 为每个形参也addType
- parse.c
  新语法
  // typeSuffix = ("(" funcParams? ")")?
  // funcParams = param ("," param)*
  // param = declspec declarator
  typeSuffix: 算是Type的补充, 如果有括号就不是变量类型, 就变为函数类型

  - function
  `Obj *newLVar(char *Name, Type *Ty)` 往参数列表中添加参数需要知道参数的name和type
  function函数中需要先用`createParamLVars(Ty->Params);`给locals添加上形参, 添加中倒着添加, 因为codegen是倒着出栈的
- codegen.c
  生成函数的时候将寄存器中的值load到参数地址中(`%d(fp)\n", ArgReg[I++]`)

### 26 支持一维数组

debug:
```c
return arrayOf(Ty, sz);
// 这里写成了arrayOf(Ty->Base, sz);
```
Type->Base->Size 和 Base->size区别:
准确说是理清Type 和 Type->Base的关系
- Type->Base:只有当前Type为指针或者数组时才有意义, 也就是指针类型才有Base

发现一个error(a quirk of the C grammar)
```c
  case ND_ADDR:{
    Type *Ty = Nd->LHS->Ty;     // 外面不加{}的话, 声明语句会报错
    if(Ty->Kind == TY_ARRAY)
      Nd->Ty = pointerTo(Ty->Base);
    else
      Nd->Ty = pointerTo(Ty);
    return;
  }

// 所以这东西语法是这样的
// case expr: ("{" compoundStmt) | stmt*
// 而stmt 如果不选择 "{" compoundStmt 是不会包含declaration语法的, 所以声明语句不行哈哈哈哈哈
```

节点一直没有Nd->Type, 破案了妈的  不知道哪一版重写function函数的时候把addType删了, 气死我了 debug了两个小时

- cc.h
  - `TokenKind::TY_ARRAY`
  - `Type::Size, Type::ArrayLen`
- type.c
  `Type *arrayOf(Type *Base, int Len)` 构建一个数组类型的Type
  赋值语句暂不支持对数组ident赋值
  并且ND_ADDR做限制, 数组类型为 `pointerTo(Ty->Base);`, 而指针类型为`pointerTo(Ty);`
- parse.c
  // typeSuffix = "(" funcParams | "[" num "]" | ε
  // funcParams = (param ("," param)*)? ")"
  注意更改newAdd newSub中指针运算时 ptr+1, 不再固定为8, 而是`LHS->Ty->Base->Size`
  ```c
    // 构造数组类型, 传入 数组基类, 元素个数
    Type *arrayOf(Type *Base, int Len) {
      Type *Ty = calloc(1, sizeof(Type));
      Ty->Kind = TY_ARRAY;
      // 数组大小为所有元素大小之和
      Ty->Size = Base->Size * Len; // Size为数组大小
      Ty->Base = Base;
      Ty->ArrayLen = Len;
      return Ty;
    }
  ```
- codegen.c
  每个变量分配字节由8改为size


`typeSuffix`函数是type的补充
```c
  int a;
  int b[5];
  int a();
```
碰到`int a`, 这时标识符`a`已经出现了, 标识符被判断为前面的`declspec "*"*`, 然后碰到`[` 或者 `(`需要改变标识符的类型

### 28 支持多维数组

// typeSuffix = "(" funcParams | "[" num "]" typeSuffix | ε
```c
if (equal(Tok, "[")) {
    int Sz = getNumber(Tok->Next);
    Tok = skip(Tok->Next->Next, "]");
    Ty = typeSuffix(Rest, Tok, Ty); 
    // 最终会递归到 ε 然后设置Rest  *Rest = Tok, 如果在这里设置 会把原来的指向末尾的Rest 重新设置为 "["
    return arrayOf(Ty, Sz);
  }
```

### 29 支持[]操作符
// unary = ("+" | "-" | "*" | "&") unary | postfix
// postfix = primary ("[" expr "]")*

x[y] 等价于 *(x+y)

### 30 支持sizeof
// primary = "(" expr ")" | "sizeof" unary | ident funcArgs? | num
```c
  if (equal(Tok, "sizeof")){
    Node *Nd = unary(Rest, Tok);
    addType(Nd);
    return newNum(Nd->Ty->Size, Tok);
  }

```
  - CRUX:注意这个要放到`ident funcArgs?`前面, 否则, 会将sizeof()判定为函数, 因为`ident funcArgs?`除了当前tok还会判断下一个tok,
  - 明明有`TK_KEYWORD`为什么`sizeof`仍然被判定为`TK_IDENT`?
    破案了 当时`convertKeywords`条件被我改了, 原版是 `T->Kind != TK_EOF`, 被我改成了`T->Kind == TK_IDENT`, 导致中间出现个别的类型的tok就停止了 智障啊 

### 31 融合var和function
- cc.h
```c
// 本地变量
typedef struct Obj Obj;
struct Obj {
  Obj *Next;  // 指向下一对象
  char *Name; // 变量名
  Type *Ty;   // 变量类型
  int Offset; // fp的偏移量
};

// 函数
typedef struct Function Function;
struct Function {
  Function *Next;  // 下一函数
  char *Name;      // 函数名

  Obj *Params;     // 形参
  Node *Body;      // 函数体
  Obj *Locals;     // 本地变量
  int StackSize;   // 栈大小
};
```
两者相同点多了去了 
更改为
```c
// 变量 或 函数
typedef struct Obj Obj;
struct Obj {
  Obj *Next;       // 指向下一对象
  char *Name;      // 变量名
  Type *Ty;        // 变量类型
  bool IsLocal;    // 局部变量还是全局变量

  // 局部变量
  int Offset;      // fp的偏移量

  // 函数或全局变量
  bool IsFunction;

  // 函数
  Obj *Params;     // 形参
  Node *Body;      // 函数体
  Obj *Locals;     // 本地变量
  int StackSize;   // 栈大小
};
```
eposide::本地变量存哪去了???
`newLVar`函数会维护本地变量

- parse.c
// program = (functionDefinition | global-variable)*
将函数存放在全局变量里

- codegen.c
  注意输入Prog为Globals,(目前Globals中存储的全部为变量, 后面会加入全局变量), 要判断一下 `bool IsFunction`

### 32 支持全局变量声明
不支持初始化
- parse.c
  // program = (functionDefinition | global-variable)*
  对每个需要判断是函数类型还是全局变量类型`isFunction()`
  ```c
    // 区分 函数还是全局变量
    static bool isFunction(Token *Tok) {
      if (equal(Tok, ";"))  // int;
        return false;
  
      // CRUX 虚设变量，用于调用declarator, 判断下后面的类型
      Type Dummy = {};
      Type *Ty = declarator(&Tok, Tok, &Dummy);
      return Ty->Kind == TY_FUNC;
    }
  ```
  global-variable = declarator?("," declarator)* ";"
  `while(!consume(&Tok, Tok, ";"))` 用 `newGVar`加入到Globals中
- codegen.c
  全局变量存放在`.data`中, `printf("  la a0, %s\n", Nd->Var->Name);  // 全局变量存放在符号表中, data段`
  用下述函数生成.data
  ```c
  // .data 全局变量
    static void emitData(Obj *Prog) {
      for (Obj *Var = Prog; Var; Var = Var->Next) {
        if (Var->IsFunction)
          continue;
  
        printf("  # 数据段标签\n");
        printf("  .data\n");
        printf("  .globl %s\n", Var->Name);
        printf("  # 全局变量%s\n", Var->Name);
        printf("%s:\n", Var->Name);
        printf("  # 零填充%d位\n", Var->Ty->Size);
        printf("  .zero %d\n", Var->Ty->Size);
      }
    }
  ```
  效果如下
  ```armasm
      # 数据段标签
      .data
      .globl x
      # 全局变量x
    x:
      # 零填充32位
      .zero 32
  
      # 定义全局main段
      .globl main
      .text
    # =====main段开始===============
    # main段标签
    main:
      # 将ra寄存器压栈,保存ra的值
      addi sp, sp, -16
      sd ra, 8(sp)
      ...
  ```


### 33 char类型

`TypeKind::TY_CHAR`
增加 TyChar变量
Type *TyChar = &(Type){TY_CHAR, 1};

// declspec = "char" | "int"
后面代码生成时要判断一下 `ty->size`, 选择 `lb / ld   sb/sd`

### 34 字符串字面量 
'\0'在哪里存储?

- cc.h
```c
TokenKind::TK_STR;  // tokenize碰到'"' 检测
Type* Token::Ty;    // TK_STR使用  parse.c::newStringLiteral()使用
char *Obj::InitData;  // 字符串初始化值 存在.data中
```

- tokenize.c
  "abc", start = "(the left one), endP = " (the right one)
  `Tok->Ty = arrayOf(TyChar, P - Start);` 开辟n+1个空间,最后一个存储'\0'
  `Tok->Str = strndup(Start + 1, P - Start - 1);` 复制abc

- parse.c
  创建全局变量需要
  `Obj *newGVar(char *Name, Type *Ty)`
  然而字符串是没名字的, 所以需要建立匿名唯一名称
  在`newStringLiteral()`中, `tok->str`会被复制到新创建的匿名全局变量的`InitData`中
  关于 `Tok->Ty`, 因为对ty的判断提前到了tokenize中, 所有后续不用再弄了
  ```c
    if (Tok->Kind == TK_STR){
      Obj *Var = newStringLiteral(Tok->Str, Tok->Ty);
      *Rest = Tok->Next;
      return newVarNode(Var, Tok);
    }
  ```
  TK_STR被定义为VAR, 但var中的某些字段不同(`InitData`以及`Ty`(`Ty`为n+1长度的数组))
- codegen.c
  之前的全局变量只声明, 并没有初始化, 但str类型有初始化, 需要判断(`Var->InitData != null`)  
  生成代码如下格式  
  源代码
  ```c
  int a[10]; 
  int main() { return sizeof("abc"); }
  ```
  ```armasm
      # 数据段标签
      .data
    .L..0:
      # 字符串字面量
      .byte 97	# a
      .byte 98	# b
      .byte 99	# c
      .byte 0
      # 数据段标签
      .data
      # 全局段a
      .globl a
    a:
      # 全局变量零填充80位
      .zero 80
  
      # 定义全局main段
      .globl main
      .text
    # =====main段开始===============
    # main段标签
    main:
    ...
  ```


### 35 格式化字符串输入
CRUX:搞懂原理
```c
// 格式化后返回字符串
char *format(char *Fmt, ...) {
  char *Buf;
  size_t BufLen;
  // 将字符串对应的内存作为I/O流
  FILE *Out = open_memstream(&Buf, &BufLen);  //函数可以实现在一段内存上进行IO操作
  // open_memstream() 函数可以为用户动态申请和扩展内存，并将内存大小通过参数返回
  // 另一个函数是fmemopen(), 但fmemopen需要调用者提供一段已经分配好的内存
  va_list VA;
  va_start(VA, Fmt);
  // 向流中写入数据
  vfprintf(Out, Fmt, VA);
  va_end(VA);

  fclose(Out);
  return Buf;
}
```

### 36 支持多个转义字符
主要是 对转移字符 '\\'的处理, 要把"\t"类似的字符串转换为'\t'
对应如下
|   转义字符   |  意思   |
| --- | ---|
|  \a   |  响铃(报警)  |
|  \b   |  退格  |
|  \t   |  水平制表符  |
|  \n   |  换行  |
|  \v   |  垂直制表符  |
|  \f   |  换页  |
|  \r   |  回车  |
|  \e   |  转义符(GNU C拓展)  |

### 37 支持八进制转义字符
转义字符还能八进制?? `\123` ==> `(1*8+2)*8+3`   不能长于3位, `\1500` ==> `"'104'0"`
- tokenize.c
  在readEscapedChar 更改P的位置, 传入`&P`
  - `\t` 跳到 t下一个
  - `\123` 跳到3下一个

### 38 支持十六进制转义字符
16进制可以是多位

### 39 [GNU]语句表达式
`({ int x=3; x; })` 这个就叫语句表达式

NodeKind::ND_STMT_EXPR
// 解析括号、数字、变量
// primary = "(" "{" stmt+ "}" ")"
//         | "(" expr ")"
//         | "sizeof" unary
//         | ident funcArgs?
//         | str
//         | num

按照 "(" "{" compoundStmt ")"解析, 结果存入Node->Body中

注意addType中// 节点类型为 最后的表达式语句的类型
```c
Node *Stmt = Nd->Body;
while (Stmt->Next)
  Stmt = Stmt->Next;
if (Stmt->Kind == ND_EXPR_STMT) {
  Nd->Ty = Stmt->LHS->Ty;
  return;
}
```


### 40从文件中读取代码,改进报错信息
输入格式更改
核心两个函数**CRUX**, 好好学  太秀了
从文件读取代码
```c

// 返回指定文件的内容  把文件转为字符串
  static char *readFile(char *Path) {
    FILE *FP;

    if (strcmp(Path, "-") == 0) {
      // 如果文件名是"-"，那么就从输入中读取
      FP = stdin;
    } else {
      FP = fopen(Path, "r");
      if (!FP)
        // errno为系统最后一次的错误代码
        // strerror以字符串的形式输出错误代码
        error("cannot open %s: %s", Path, strerror(errno));
    }

    // 要返回的字符串
    char *Buf;
    size_t BufLen;
    FILE *Out = open_memstream(&Buf, &BufLen);

    // 读取整个文件
    while(true) {
      char Buf2[4096];
      // fread从文件流中读取数据到数组中
      // 数组指针Buf2，数组元素大小1，数组元素个数4096，文件流指针
      int N = fread(Buf2, 1, sizeof(Buf2), FP);
      if (N == 0)
        break;
      // 数组指针Buf2，数组元素大小1，实际元素个数N，文件流指针
      fwrite(Buf2, 1, N, Out);
    }

    // 对文件完成了读取
    if (FP != stdin)
      fclose(FP);

    // 刷新流的输出缓冲区，确保内容都被输出到流中
    fflush(Out);
    // 确保最后一行以'\n'结尾
    if (BufLen == 0 || Buf[BufLen - 1] != '\n')
      // 将字符输出到流中
      fputc('\n', Out);
    fputc('\0', Out);
    fclose(Out);
    return Buf;
  }
```
改进报错信息
```c
  // 输出例如下面的错误，并退出
  // foo.c:10: x = y + 1;
  //               ^ <错误信息>
  static void verrorAt(char *Loc, char *Fmt, va_list VA) {
    // 查找包含loc的行
    char *Line = Loc;
    // Line递减到当前行的最开始的位置
    // Line<CurrentInput, 判断是否读取到文件最开始的位置
    // Line[-1] != '\n'，Line字符串前一个字符是否为换行符（上一行末尾）
    while (CurrentInput < Line && Line[-1] != '\n')
      Line--;

    // End递增到行尾的换行符
    char *End = Loc;
    while (*End != '\n')
      End++;

    // 获取行号, 一个字符一个字符的遍历, 数'\n'个数
    int LineNo = 1;
    for (char *P = CurrentInput; P < Line; P++)
      // 遇到换行符则行号+1
      if (*P == '\n')
        LineNo++;

    // 输出 文件名:错误行
    // Indent记录输出了多少个字符
    int Indent = fprintf(stderr, "%s:%d: ", CurrentFilename, LineNo); // foo.c:10
    // 输出Line的行内所有字符（不含换行符）
    fprintf(stderr, "%.*s\n", (int)(End - Line), Line); // 从Line开始 打印出 int(End-Line)个字符

    // 计算错误信息位置，在当前行内的偏移量+前面输出了多少个字符
    int Pos = Loc - Line + Indent;

    // 将字符串补齐为Pos位，因为是空字符串，所以填充Pos个空格。
    fprintf(stderr, "%*s", Pos, "");  // Pos个空格。
    fprintf(stderr, "^ ");
    vfprintf(stderr, Fmt, VA);
    fprintf(stderr, "\n");
    va_end(VA);
  }
```
**CRUX**  
[格式化输出字符串](https://www.cnblogs.com/dapaitou2006/p/6428122.html)

对于m.n的格式还可以用如下方法表示（例）  
char ch[20];  
printf("%\*.\*s/n",m,n,ch);  
前边的*定义的是总的宽度，后边的定义的是输出的个数。分别对应外面的参数m和n 。我想这种方法的好处是可以在语句之外对参数m和n赋值，从而控制输出格式。

| 格式代码 | A  |ABC | ABCDEFGH |
| -- | --- | -- | -- |
|%s  | A   |ABC | ABCDEFGH |
|%5s | ####A   |##ABC | ABCDEFGH |
|%.5s  | A   |ABC | ABCDE |
|%5.5s  | ####A   |##ABC | ABCDE |
|%-5s  | A####   |ABC## | ABCDEFGH |

`%m.ns` : 宽度为m(不够了补齐, 默认右对齐, `-`左对齐), 长度为n
`%*.*s` : 自己指定m,n:`printf("%\*.\*s/n",m,n,ch);`
- 注意readfile中这么写的
```c
if (strcmp(Path, "-") == 0) {
      // 如果文件名是"-"，那么就从输入中读取
      FP = stdin;
```
因为sh中
```sh
echo "$input" | ./cc - > tmp.s || exit
```
管道的数据从stdin中进来, argv[1] = "-"
**CRUX****管道从stdin来**


### 42 支持-o和--help(-h)选项
输出格式的更改
`./cc input.c -o output`
`./cc --help` / `./cc -h`  ==> `cc [ -o <path> ] <file>`

- main.c中添加`parseArgs()`, 解析输入参数
输入参数为 `-` 则 `OptO = stdout`
`-oXXX` 或者 `-o XXX`, `OptO = openFile(XXX)`

至于`test-dirver.sh`应该是后续测试需要的

### 43 支持行注释和块注释
```c
    // 跳过行注释
    if (startsWith(P, "//")) {
      P += 2;
      while (*P != '\n')
        P++;
      continue;
    }

    // 跳过块注释
    if (startsWith(P, "/*")) {
      // 查找第一个"*/"的位置
      char *Q = strstr(P + 2, "*/");
      if (!Q)
        errorAt(P, "unclosed block comment");
      P = Q + 2;
      continue;
    }
```

### 44 处理代码块域
```c
// 局部和全局变量的域
typedef struct VarScope VarScope;
struct VarScope {
  VarScope *Next; // 下一变量域
  char *Name;     // 变量域名称
  Obj *Var;       // 对应的变量  Var->Next 本身就是一个链表结构
};

// 表示一个块域
typedef struct Scope Scope;
struct Scope {
  Scope *Next;    // 指向上一级的域
  VarScope *Vars; // 指向当前域内的变量
};

// 所有的域的链表   CRUX 不是一个链表结构, 而是一个树状结构  Scp指向当前的作用域,而非最后一个作用域
static Scope *Scp = &(Scope){};
```
维持这个结构需要的函数
```c
// 类似于栈
// 进入域
static void enterScope(void) {
  Scope *S = calloc(1, sizeof(Scope));
  // 后来的在链表头部
  // 类似于栈的结构，栈顶对应最近的域
  S->Next = Scp;
  Scp = S;
}

void leaveScope(void) { Scp = Scp->Next; }  // 这个函数非常的牛逼
```
`{}`对应着变量域, 在函数定义`function`和复合语句`compoundStat`存在变量域的概念, 进入函数时 `enterScope`, 离开时`leaveScope`, 改变`Scp`指针的位置

// TODO : 暂不支持检测变量是否在同一作用域内声明过`redefined`
加入作用域后
```c
// 通过名称，查找一个变量  
// 一个树状结构
static Obj *findVar(Token *Tok) {
  // 此处越先匹配的域，越深层
  for (Scope *S = Scp; S; S = S->Next)  // next指向上一级的域
    // 遍历域内的所有变量
    for (VarScope *S2 = S->Vars; S2; S2 = S2->Next)  // next指向下一变量域
      if (equal(Tok, S2->Name))
        return S2->Var;
  return NULL;
}
```

### 45 C重写测试
这命令行sh够我看一礼拜


```sh
# 短短两行代码, 我得用一生治愈
test/%.exe: cc test/%.c
	$(CC) -o- -E -P -C test/$*.c | ./cc -o test/$*.s -
	riscv64-linux-gnu-gcc -static -o $@ test/$*.s -xc test/common
```
makefile里面这几行shell命令 CRUX TODO  吐了
```sh
TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

# 测试标签，运行测试
test/%.exe: cc test/%.c
	$(CC) -o- -E -P -C test/$*.c | ./cc -o test/$*.s -
	# riscv64-linux-gnu-gcc -o- -E -P -C test/$*.c | ./cc -o test/$*.s -
	# $(CC) -static -o $@ test/$*.s -xc test/common
	riscv64-linux-gnu-gcc -static -o $@ test/$*.s -xc test/common

test: $(TESTS)
	# for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	for i in $^; do echo $$i; qemu-riscv64 -L $(RISCV)/sysroot ./$$i || exit 1; echo; done
#	for i in $^; do echo $$i; $(RISCV)/bin/spike --isa=rv64gc $(RISCV)/riscv64-unknown-linux-gnu/bin/pk ./$$i || exit 1; echo; done
	test/driver.sh
# 清理标签，清理所有非源代码文件
clean:
	rm -rf cc tmp* $(TESTS) test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'
```

运行起来了

`riscv64-linux-gnu-gcc -static -o test/arith.exe test/arith.s -xc test/common`
用`riscv64-gcc` 以C程序编译的common文件

`gcc -o- -E -P -C test/arith.c | ./cc -o test/arith.s -`
- -E  Preprocess only; do not compile, assemble or link.
- -P  // TODO 没找到
- -C  // TODO 没找到

### 46 为Token预计算行号
在tokenize中算好每个token在哪一行

### 47 生成.file和.loc汇编指令
将 `.file 1 "-"`  和 `.loc 1 39` 标注输入文件和文件行号的汇编指令写入`.s`文件

### 48 支持 , 运算符
- ND_COMMA, // , 逗号
- 语法`expr = assign ("," expr)?`
, 干嘛的??  就是 `,`连接起来的一系列表达式, 相比stmt 的`expr*`解释多一个尾部值的Type
```c
  ASSERT(5, ({ int i=2, j=3; (i=5,j)=6; i; }));
  ASSERT(6, ({ int i=2, j=3; (i=5,j)=6; j; }));
```
有两种
- 正常的并列语句
- 赋值语句使用(也就是上面的例子), `括号 ASSIGN Val`, 赋值为为括号中的,右部变量赋值

### 49 支持struct
下述格式的
```c
{ 
  struct {
    char a[3]; 
    char b[5];
  }x; 
  char *p=&x; 
  x.b[0]=7; 
  p[3]; 
}
```
**CRUX**x为变量 ND_VAR类型, 但是member为ND_MEMBER类型, ND_MEMBER类型不存储在locals或者globals中, 而是依托于x存在 


- cc.h
  ```cpp
  NodeKind::ND_MEMBER // . 结构体成员访问   那个点
  Member *Node::Mem;
  TypeKind::TY_STRUCT


  // 结构体成员
  struct Member {
    Member *Next; // 下一成员
    Type *Ty;     // 类型
    Token *Name;  // 名称
    int Offset;   // 偏移量
  };

  ```

- tokenize.c
简单的把关键字`struct`加进去就行

- parse.c
新语法
  ```c
  // declspec = "char" | "int" | structDecl

  // structDecl = "{" structMembers
  // structMembers = (declspec declarator (","  declarator)* ";")*
  // postfix = primary ("[" expr "]" | "." ident)*
  ```
  以新语法修改下列函数
  ```c
  bool isTypename(Token *Tok);
  Type *declspec(Token **Rest, Token *Tok);
  Node *postfix(Token **Rest, Token *Tok);
  ```
  新增下列函数
  ```c
  // structMembers = (declspec declarator (","  declarator)* ";")* "}"
  void structMembers(Token **Rest, Token *Tok, Type *Ty);
  // 每一个按照 Member 构建, , 存入链表中Member.Next

  // structDecl = "{" structMembers
  Type *structDecl(Token **Rest, Token *Tok);
  // 判断类型, 同时遍历链表计算偏移量

  // 获取结构体成员 遍历Mems
  Member *getStructMember(Type *Ty, Token *Tok);

  // 构建结构体成员的节点  用于 struct_a.member_b这种语法
  // Nd中不直接存储成员, 而是存储在mem中
  Node *structRef(Node *LHS, Token *Tok) {
    addType(LHS);
    if (LHS->Ty->Kind != TY_STRUCT)
      errorTok(LHS->Tok, "not a struct");

    Node *Nd = newUnary(ND_MEMBER, LHS, Tok); // member ND的左子树存储struct_a, tok为member_b
    Nd->Mem = getStructMember(LHS->Ty, Tok);
    return Nd;
  }
  ```
- type.c
  ```c
  case ND_MEMBER:
    Nd->Ty = Nd->Mem->Ty;  
  ```
- codegen.c
  ND_MEMBER使用和ND_VAR相同,都是`genaddr`后`load`
  ```c
  genadd():
    case ND_MEMBER:
      genAddr(Nd->LHS);  // struct_a位置
      printLn("  # 计算成员变量的地址偏移量");
      printLn("  addi a0, a0, %d", Nd->Mem->Offset);
  ```

### 50 对齐结构体成员变量
- cc.h
`Type::Align`
同时给`char, int, struct`还有`struct.member`添加Align
struct的默认对齐为1, 存在member则为member中的最大对齐
```c
  // 修改成员offset的构建
  for (Member *Mem = Ty->Mems; Mem; Mem = Mem->Next) {
    Offset = alignTo(Offset, Mem->Ty->Align);
    Mem->Offset = Offset;
    Offset += Mem->Ty->Size;

    if (Ty->Align < Mem->Ty->Align)
      Ty->Align = Mem->Ty->Align;
  }
```
### 51 对齐局部变量
计算完`offset`后对齐一下
```c
  for (Obj *Var = Fn->Locals; Var; Var = Var->Next) {
      // 每个变量分配空间
      Offset += Var->Ty->Size;
      // 对齐变量
      Offset = alignTo(Offset, Var->Ty->Align);
      // 为每个变量赋一个偏移量，或者说是栈中地址
      Var->Offset = -Offset;
  }
```

### 52 支持结构体标签
下述格式的
```c
{ a
  struct a{
    char a[3]; 
    char b[5];
  };
  struct a x; // x is type(struct a)  
  char *p=&x; 
  x.b[0]=7; 
  p[3]; 
}
```

结构体标签域
整体的域结构
```c
// 局部和全局变量的域
typedef struct VarScope VarScope;
struct VarScope {
  VarScope *Next; // 下一变量域
  char *Name;     // 变量域名称
  Obj *Var;       // 对应的变量  
};

// 结构体标签的域
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

  // C有两个域：变量域，结构体标签域
  VarScope *Vars; // 指向当前域内的变量
  TagScope *Tags; // 指向当前域内的结构体标签
};
```
标识一个数据结构暂时有两种格式
- `ident` 变量 Var
- `"struct" ident` 标签 Tag

语法应该是下面这样的, 同时也支持之前不带标识符的语法(见49)
```c
// declspec = "char" | "int" | "struct" structDecl
// structDecl = ident? ("{" structMembers)?
{ 
  struct t {
    char a[2];
  }; 
  { 
    struct t {
      char a[4];
    }; 
  } 
  int t = 1;
  struct t y; 
  return sizeof(y); 
}
```
**CRUX : 终于明白为什么 `int;` 不违法了**
因为支持struct类型的声明, 下面几种struct都合法
```c
// 这个就对应 int;
struct a {
  int b;
  int c;
};

struct a s_a;

struct a {
  int b;
  int c;
}b;

```

### 53 支持->运算符
x->y <==> (*x).y
// postfix = primary ("[" expr "]" | "." ident)* | "->" ident)*
```c
  // "->" ident
  if (equal(Tok, "->")) {
    // x->y 等价于 (*x).y
    Nd = newUnary(ND_DEREF, Nd, Tok);  // *x
    Nd = structRef(Nd, Tok->Next);     // *x.y
    Tok = Tok->Next->Next;
    continue;
  }
```

### 54 支持union
联合体需要设置为最大的对齐量与大小，变量偏移量都默认为0
union就是一个偏移量都为0的结构体, 注意设置`size`和`align`为最大值

### 55 增加结构体赋值
下列形式
```c
{ 
  struct t {
    char a, b;
  } x, y; 
  x.a=5; 
  y=x;   // this line
  return y.a; 
}
```

对于`sturct/union`在ASSIGN阶段直接赋值, 复制`size`的内存

CRUX : load为什么直接跳过?
只有 `*a`(a为结构体)的情况才会调用`load`, 而`*a`的使用只能依托于`*a`的postfix和赋值语句, 单独`load` `*a`没有意义, 而且有错 
```c
// 加载a0指向的值
static void load(Type *Ty) {
  if (Ty->Kind == TY_ARRAY || Ty->Kind == TY_STRUCT || Ty->Kind == TY_UNION)
    return;

  printLn("  # 读取a0中存放的地址, 得到的值存入a0");
  if(Ty->Size == 1){
    printLn("  lb a0, 0(a0)");
  }else{
    printLn("  ld a0, 0(a0)");
  }
}
```

### 56 将int大小由8改为4
改一下`TyInt`
主要该`codegen.c`, 生成指令时判断一下size

### 57 支持long类型
主要是把默认类型int改为long, 如`ND_EQ,ND_NE,ND_LT,ND_LE,ND_NUM,ND_FUNCALL`
其次是把默认val类型由int改为int64_t
外加添加`long`关键字

### 58 short
添加一些

### 59 嵌套类型声明符
// declarator = "*"* ( "(" ident ")" | "(" declarator ")" | ident ) typeSuffix
嵌套类型声明符就是 `()`
以下情况
```c
({ char *x[3]; sizeof(x); }) => 24
({ char (*x)[3]; sizeof(x); }) => 8   // 一个指向 char[3] 类型的指针
({ char *x[3]; char y; x[0]=&y; y=3; x[0][0]; }) => 3
({ char x[3]; char (*y)[3]=x; y[0][0]=4; y[0][0]; }) => 4
```

`char (*x)[3];` 当作 `char[3] *x;`解析, 重新判断`type`
```c
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
```

TODO :: 所以`char (*x)[3]` 和 `char *x`相比有什么多余的作用吗?

### 60 支持函数声明
// functionDefinition = declarator ("{" compoundStmt | ";" )
```c
// function():
  Fn->IsDefinition = !consume(&Tok, Tok, ";");

  // 判断是否没有函数定义
  if (!Fn->IsDefinition)
    return Tok;
```

暂时没什么实际作用, 因为这个编译器没有先后顺序

### 61 void
处处受限制, 应该是指针强转, 类接口的需要
```c
if (Nd->LHS->Ty->Base->Kind == TY_VOID)  // 不能解引用
  errorTok(Nd->Tok, "dereferencing a void pointer"); 

if (Ty->Kind == TY_VOID)   // 不能声明void类型(可以声明void *)
  errorTok(Tok, "variable declared void"); 
```

### 62 修正解析复杂类型声明
// declspec = ("void" | "char" | "short" | "int" | "long" | structDecl | unionDecl)+
主要解析 `short int,  int short, long int, int long`
1左移 2,4 ,6, 8, 10 代表
```c
  enum {
    VOID  = 1 << 0,
    CHAR  = 1 << 2,
    SHORT = 1 << 4,
    INT   = 1 << 6,
    LONG  = 1 << 8,
    OTHER = 1 << 10,
  };
```
循环用加法判断结果属于哪个

### 63 long long
`long long`, `long long int`

### 64 typedef

- 对于语句 `typedef struct a b;` :
  1. 设置Attr->IsTypedef = trye
  2. 返回 strcut a 的baseTy
  3. 然后调用`parseTypedef`, 设置 b->Typedef = baseTy
- 对于语句 `b a;`: `Istypename(b)`直接返回`findTypedef(b)`, 也就是b->Typedef

新结构
```c
struct VarScope {
  VarScope *Next; // 下一变量域
  char *Name;     // 变量域名称
  Obj *Var;       // 对应的变量  
  Type *Typedef;  // 别名    new
};

// 变量属性
typedef struct {
  bool IsTypedef; // 是否为类型别名
} VarAttr;
```

新函数
```c
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


// 解析类型别名 
// typedef struct a b  ==> 将 b 的Typedef 改为  struct a 
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
```

`struct a b` : a 存在标签域中
`typedef struct a b` : b 存在于变量域中

```c

// 一个 typedef struct a b进来 do
// 1. 设置Attr->IsTypedef = trye
// 2. 返回 strcut a 的baseTy
static Type *declspec(Token **Rest, Token *Tok, VarAttr *Attr) {
  // 类型的组合，被表示为例如：LONG+LONG=1<<9
  // 可知long int和int long是等价的。
  // ...................

  Type *Ty = TyInt;
  int Counter = 0; // 记录类型相加的数值

  // 遍历所有类型名的Tok
  while (isTypename(Tok)) {
    // 处理typedef关键字
    if (equal(Tok, "typedef")) {
      if (!Attr)
        errorTok(Tok, "storage class specifier is not allowed in this context");
      Attr->IsTypedef = true;
      Tok = Tok->Next;
      continue;
    }

    // 处理用户定义的类型
    // typedef struct a b;
    Type *Ty2 = findTypedef(Tok);   // struct a
    if (equal(Tok, "struct") || equal(Tok, "union") || Ty2) {
      if(Counter)  // 这几种不能嵌套
        break;
      if (equal(Tok, "struct")){
        Ty = structDecl(&Tok, Tok->Next);
      }else if (equal(Tok, "union")) {
        Ty = unionDecl(&Tok, Tok->Next);
      }else{
        // 将类型设为类型别名指向的类型
        Ty = Ty2;   // struct a 
        Tok = Tok->Next;
      }
      Counter += OTHER;
      continue;
    }
  }
   // ..............................

  *Rest = Tok;
  return Ty;

}

```

typedef 语法
```c
    // typedef struct a b;
    if(isTypename(Tok)){
      VarAttr Attr = {};
      Type *BaseTy = declspec(&Tok, Tok, &Attr);
      // 解析typedef的语句
      if(Attr.IsTypedef){
        Tok = parseTypedef(Tok, BaseTy);  // 将b放入变量域,并将b->Typedef = struct a
        continue;
      }
    }
```
### 65 对类型进行sizeof

`parse.c/primary() : sizeof(int[10])`
```c
  // "sizeof" "(" typeName ")"
  if (equal(Tok, "sizeof") && equal(Tok->Next, "(") &&
      isTypename(Tok->Next->Next)) {
    Type *Ty = typename(&Tok, Tok->Next->Next);
    *Rest = skip(Tok, ")");
    return newNum(Ty->Size, Start);
  }

static Type *typename(Token **Rest, Token *Tok) {
  // declspec
  Type *Ty = declspec(&Tok, Tok, NULL);
  // abstractDeclarator
  return abstractDeclarator(Rest, Tok, Ty);
}

```
`abstractDeclarator` 和 `declarator`比起来就是没有`ident`, 其他相同, 虚空声明一个, 算一下type的大小完事

### 66 增加对32位指令的支持
`codegen.c/genExpr()`
```c
  char *Suffix = Nd->LHS->Ty->Kind == TY_LONG || Nd->LHS->Ty->Base ? "" : "w";
  printLn("  add%s a0, a0, a1", Suffix);
```

### 67 类型转换
- parse.c
// mul = cast ("*" cast | "/" cast)*
// cast = ("(" typeName ")" cast) | unary
// unary = ("+" | "-" | "*" | "&") cast | postfix

```c
// 新转换节点
static Node *newCast(Node *Expr, Type *Ty) {
  addType(Expr);

  Node *Nd = calloc(1, sizeof(Node));
  Nd->Kind = ND_CAST;
  Nd->Tok = Expr->Tok;
  Nd->LHS = Expr;
  Nd->Ty = copyType(Ty);
  return Nd;
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
```
改变expr的类型

- codegen.c 对 ND_CAST的处理
```c
  case ND_CAST:
    genExpr(Nd->LHS);
    cast(Nd->LHS->Ty, Nd->Ty); //  cast(from , to)
    return;
```

**高位64-n位置零**
```c
// 类型枚举
enum { I8, I16, I32, I64 };  // 0 1 2 3 

// 获取类型对应的枚举值
static int getTypeId(Type *Ty) {
  switch (Ty->Kind) {
  case TY_CHAR:
    return I8;
  case TY_SHORT:
    return I16;
  case TY_INT:
    return I32;
  default:
    return I64;
  }
}

// 类型映射表. 高64-n位置零
// 先逻辑左移N位，再算术右移N位，就实现了将64位有符号数转换为64-N位的有符号数
static char i64i8[] = "  # 转换为i8类型\n"
                      "  slli a0, a0, 56\n"
                      "  srai a0, a0, 56";
static char i64i16[] = "  # 转换为i16类型\n"
                       "  slli a0, a0, 48\n"
                       "  srai a0, a0, 48";
static char i64i32[] = "  # 转换为i32类型\n"
                       "  slli a0, a0, 32\n"
                       "  srai a0, a0, 32";

// 所有类型转换表
static char *castTable[10][10] = {
    // clang-format off

    // 转换到 cast to
    // {i8,  i16,    i32,    i64}
    {NULL,   NULL,   NULL,   NULL}, // 从i8转换
    {i64i8,  NULL,   NULL,   NULL}, // 从i16转换
    {i64i8,  i64i16, NULL,   NULL}, // 从i32转换
    {i64i8,  i64i16, i64i32, NULL}, // 从i64转换

    // clang-format on
};

// 类型转换
static void cast(Type *From, Type *To) {
  if (To->Kind == TY_VOID)
    return;

  // 获取类型的枚举值
  int T1 = getTypeId(From);
  int T2 = getTypeId(To);
  if (castTable[T1][T2]) {
    printLn("  # 转换函数");
    printLn("%s", castTable[T1][T2]);
  }
}
```

### 68 实现常规算术转换
- type.c
```c
// 根据Ty1, Ty2的类型, 返回能容纳两者类型的类型
Type *getCommonType(Type *Ty1, Type *Ty2);
// 将左右部用newcast函数转换为上述函数获得的通用类型
void usualArithConv(Node **LHS, Node **RHS);
```

 ASSERT(0, 1073741824 * 100 / 100); // 0x4000 0000  
`integer overflow in expression of type ‘int’ results in ‘0’`

### 69 对未定义或未声明的函数报错 
funCall中查找
```c
  VarScope *S = findVar(Start);
  if (!S)
    errorTok(Start, "implicit declaration of a function");
  if (!S->Var || S->Var->Ty->Kind != TY_FUNC)
    errorTok(Start, "not a function");

  Type *Ty = S->Var->Ty->ReturnTy;
```

### 70 返回值的类型转换
stmt中的 return语句 用`newcast()`转换为 `CurrentFn->returnTy`

### 71 函数实参类型转换
添加了 `Node::FunctionTy`, 但暂时只赋值, 没有用到
- parse.c

Params中保存了所有参数的类型, 循环遍历参数中, 用cast逐一转换
`funCall()`  
```c
  // 函数名的类型
  Type *Ty = S->Var->Ty;
  // 函数形参的类型
  Type *ParamTy = Ty->Params;    // Params中保存了所有参数的类型

  Node Head = {};
  Node *Cur = &Head;

  while (!equal(Tok, ")")) {
    if (Cur != &Head)
      Tok = skip(Tok, ",");
    // assign
    Node *Arg = assign(&Tok, Tok);
    addType(Arg);
    // 逐一用cast转换
    if (ParamTy) {
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
    addType(Cur);
  }
```

### 72 _Bool类型

addType中, 会对ASSIGN语句进行newCast转换

- codegen.c/newCast()
```c
  if (To->Kind == TY_BOOL) {
    printLn("  # 转为bool类型：为0置0，非0置1");
    printLn("  snez a0, a0");
    return;
  }
```
其他就是简单的添加类型


### 73 字符字面量
和字符串字面量类似, 解析以 `'`开头的序列, 并设置位TK_NUM类型(int long char在tokenize中都为TK_NUM类型)
literal 文字, 字面

### 74 enum枚举类型
- type.c
TY_ENUM为整数
构造枚举类型
`Type *enumType(void) { return newType(TY_ENUM, 4, 4); }`  

- parse.c
VarScope中添加
```c
  Type *EnumTy;   // 枚举的类型   固定为enumType
  int EnumVal;    // 枚举的值
```

语法更新
// declspec =  ("void" | "_Bool" | "char" | "short" | "int" |"long" 
//            | "typedef"
//            | "struct" structDecl | "union" unionDecl
//            | "enum" enumSpecifier)+
// enumSpecifier = ident? "{" enumList? "}"
//                 | ident ("{" enumList? "}")?

几种enum语法
```c
enum { zero, five=5, three=3, four };
enum t { zero, one, two }; enum t y;
```
枚举常量 `zero, one, two`要当作变量, 存入pushScope
如果有别名(标签), 要`pushTagScope(Tag, Ty);`

`primary()`解析标识符时`VarScope *S = findVar(Tok);`, 需判断标识符的类型, 按照`S->Var`和`S->EnumTy` 是否为空判断是变量还是枚举常量, 变量为`newVarNode(S->Var, Tok)`, 枚举常量为`newNum(S->EnumVal, Tok)`

### 75 文件域内函数
也就是 `static`修饰函数的作用

函数用`Obj`存储, 在其中添加`bool IsStatic`
- parse.c  
`declspec` 检查是否有`static`
// declspec =  ("void" | "_Bool" | "char" | "short" | "int" |"long" 
//            | "typedef" | | "static"
//            | "struct" structDecl | "union" unionDecl
//            | "enum" enumSpecifier)+
并在`function()`参数列表中添加`VarAttr *Attr`, declspec 设置Attr, 然后将`Fn->IsStatic`赋值为`Attr->IsStatic`
- codegen.c  
生成函数代码时, 判断`IsStatic`, 判断是`.global` 还是 `.local`

### 76 for循环域内定义局部变量
```c
{ 
  int i=3; 
  int j=0; 
  for (int i=0; i<=10; i=i+1) 
    j=j+i; 
  return i; 
}
```
Stmt中进入for语句时, `enterScope`, 然后末尾`leaveScope`, 主要判断开头是否为TypeName `isTypeName()`


### 77 支持+= -= *= /=
- tokenize  
操作符中添加`+= -= *= /=`
- parse  

转换 `A op= B`为 `TMP = &A, *TMP = *TMP op B`
// TODO : 为什么不能转换为`A = A op B` ?, 等到后面版本改一下, 测试用例里面没有指针加减法
改了暂时也没问题
```c
  // ("+=" assign)?
  if (equal(Tok, "+="))
    return newBinary(ND_ASSIGN, Nd, newAdd(Nd, assign(Rest, Tok->Next), Tok), Tok);
    // return toAssign(newAdd(Nd, assign(Rest, Tok->Next), Tok));

  // ("-=" assign)?
  if (equal(Tok, "-="))
    return newBinary(ND_ASSIGN, Nd, newSub(Nd, assign(Rest, Tok->Next), Tok), Tok);
    // return toAssign(newSub(Nd, assign(Rest, Tok->Next), Tok));

  // ("*=" assign)?
  if (equal(Tok, "*="))
    return newBinary(ND_ASSIGN, Nd, newBinary(ND_MUL, Nd, assign(Rest, Tok->Next), Tok), Tok);
    // return toAssign(newBinary(ND_MUL, Nd, assign(Rest, Tok->Next), Tok));

  // ("/=" assign)?
  if (equal(Tok, "/="))
    return newBinary(ND_ASSIGN, Nd, newBinary(ND_DIV, Nd, assign(Rest, Tok->Next), Tok), Tok);
    // return toAssign(newBinary(ND_DIV, Nd, assign(Rest, Tok->Next), Tok));
```


转换 A op= B为 TMP = &A, *TMP = *TMP op B
```c
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
```

### 78 前置++ --
// unary = ("+" | "-" | "*" | "&") cast | ("++" | "--") unary | postfix
// 转换 ++i 为 i+=1  `toAssign(newAdd(unary(Rest, Tok->Next), newNum(1, Tok), Tok));`
// 转换 --i 为 i-=1

### 79 后置++ --
// postfix = primary ("[" expr "]" | "." ident)* | "->" ident | "++" | "--")*
转换 A++ 为 `(typeof A)((A += 1) - 1)`  
```c
if (equal(Tok, "++")) {
  Nd = newIncDec(Nd, Tok, 1); // -- -1
  Tok = Tok->Next;
  continue;
}

// 转换 A++ 为 `(typeof A)((A += 1) - 1)`
// Increase Decrease
static Node *newIncDec(Node *Nd, Token *Tok, int Addend) {
  addType(Nd);
  return newCast(newSub(toAssign(newAdd(Nd, newNum(Addend, Tok), Tok)),
                        newNum(Addend, Tok), Tok),
                 Nd->Ty);
}
```

### 80 2,8,16进制的数字
八进制：以0开头，由0~7组成的数。如 0126, 050000.  
511, 0777   # 8进制
0, 0x0   
10, 0xa  
10, 0XA  
48879, 0xbeef  
48879, 0xBEEF  
48879, 0XBEEF  
0, 0b0  
1, 0b1  
47, 0b101111  
47, 0B101111  

用到的几个函数
```c
/* Compare no more than N chars of S1 and S2, ignoring case.  */
int strncasecmp (const char *__s1, const char *__s2, size_t __n)  // <strings.h>
// a hexadecimal character, isxdigit() returns a non-zero integer.
int isxdigit( int arg ); // char类型会被转换为int     // ctype.h
// 字符串转无符号长整型, 并更改endptr的值
unsigned long int strtoul(const char *str, char **endptr, int base) // <stdlib.h>
```

### 81 !操作符
// unary = ("+" | "-" | "*" | "&" | "!") cast | ("++" | "--") unary | postfix
ND_NOT, 类型为TypeInt
codegen
```c
case ND_NOT:
  genExpr(Nd->LHS);
  printLn("  # 非运算");
  // seqz rd, rs  set rd to 1 if rs == 0
  printLn("  seqz a0, a0"); 
  return;
```

### 82 ~按位取反
// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
//       | ("++" | "--") unary
//       | postfix
ND_NOT, 类型为左部的类型
codegen 
```c
case ND_BITNOT:
  genExpr(Nd->LHS);
  printLn("  # 按位取反");
  // 这里的 not a0, a0 为 xori a0, a0, -1 的伪码
  // printLn("  not a0, a0");
  printLn("  xori a0, a0, -1");
  return;
```

### 83 % %=
与 `* *=`相同
汇编指令
```c
case ND_MOD: // % a0=a0%a1
  printLn("  # a0%%a1, 结果写入a0");
  printLn("  rem%s a0, a0, a1", Suffix);
  return;
```

### 84  & ^ |
优先级`& ^ | assign` 
// assign = bitOr (assignOp assign)?
// bitOr = bitXor ("|" bitXor)*
// bitXor = bitAnd ("^" bitAnd)*
// bitAnd = equality ("&" equality)*
// assignOp = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="

**CRUX BUG** : A & B, B变为了0   
代码生成放错位置了, 按照一元运算符去解析了 nlgbd
- cc.h
  ND_BITAND,    // &, 按位与
  ND_BITOR,     // |, 按位或
  ND_BITXOR,    // ^, 按位异或
- type.c
按照 * 规则来


- parse.c
```c
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
```

### 85 支持&&和||
其他基本和 ND_NOT, ND_BITAND等相同  
codegen有点不同
因为实际的 `&&` 和 `||` 有提前跳转的机制, 比如 `1 && 0 && 1 && 26`, 计算到`1 && 0`就会停止, 需要实际模仿这个过程  
放到 ND_NOT 下面  
TODO: 搞明白 为什么放到前面和放到后面
```c
  // 逻辑与
  case ND_LOGAND: {
    int C = count();
    printLn("\n# =====逻辑与%d===============", C);
    genExpr(Nd->LHS);
    // 判断是否为短路操作
    printLn("  # 左部短路操作判断, 为0则跳转");
    printLn("  beqz a0, .L.false.%d", C);
    genExpr(Nd->RHS);
    printLn("  # 右部判断, 为0则跳转");
    printLn("  beqz a0, .L.false.%d", C);
    printLn("  li a0, 1");
    printLn("  j .L.end.%d", C);
    printLn(".L.false.%d:", C);
    printLn("  li a0, 0");
    printLn(".L.end.%d:", C);
    return;
  }
  // 逻辑或
  case ND_LOGOR: {
    int C = count();
    printLn("\n# =====逻辑或%d===============", C);
    genExpr(Nd->LHS);
    // 判断是否为短路操作
    printLn("  # 左部短路操作判断, 不为0则跳转");
    printLn("  bnez a0, .L.true.%d", C);
    genExpr(Nd->RHS);
    printLn("  # 右部判断, 不为0则跳转");
    printLn("  bnez a0, .L.true.%d", C);
    printLn("  li a0, 0");
    printLn("  j .L.end.%d", C);
    printLn(".L.true.%d:", C);
    printLn("  li a0, 1");
    printLn(".L.end.%d:", C);
    return;
  }
```

|| 操作的汇编
左部右部存在1就把`a0`置1, 然后结束
```armasm
  #LHS
  bnez a0, .L.true
  #RHS
  bnez a0, .L.true
  li a0, 0
  j .L.end
.L.true:
  li a0,1
.L.end:
```
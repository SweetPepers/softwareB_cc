#include "cc.h"

// 语义分析与代码生成
static void genStmt(Node *Nd);
static void genExpr(Node *Nd);

// 输出文件
static FILE *OutputFile;

// 输出字符串到目标文件并换行
static void printLn(char *Fmt, ...) {
  va_list VA;

  va_start(VA, Fmt);
  vfprintf(OutputFile ,Fmt, VA);
  va_end(VA);

  fprintf(OutputFile, "\n");
}

// 代码段计数
static int count(void) {
  static int I = 1;
  return I++;
}

// 对齐到Align的整数倍
int alignTo(int N, int Align) {
  // (0,Align]返回Align
  return (N + Align - 1) / Align * Align;
}


// 记录栈深度
static int Depth;

// 用于函数参数的寄存器们
static char *ArgReg[] = {"a0", "a1", "a2", "a3", "a4", "a5"};

// 当前的函数
static Obj *CurrentFn;


// 压栈，将结果临时压入栈中备用
// sp为栈指针，栈反向向下增长，64位下，8个字节为一个单位，所以sp-8
// 当前栈指针的地址就是sp，将a0的值压入栈
// 不使用寄存器存储的原因是因为需要存储的值的数量是变化的。
static void push(void) {
  printLn("  addi sp, sp, -8");
  printLn("  sd a0, 0(sp)");
  Depth++;
}

// 弹栈，将sp指向的地址的值，弹出到a1
static void pop(char *Reg) {
  printLn("  ld %s, 0(sp)", Reg);
  printLn("  addi sp, sp, 8");
  Depth--;
}



// 计算给定节点的绝对地址
// 如果报错，说明节点不在内存中
static void genAddr(Node *Nd) {
  switch (Nd->Kind){
    case ND_VAR:
      if(Nd->Var->IsLocal){
        // 偏移量是相对于fp的    
        printLn("  # 获取局部变量%s的栈内地址为%d(fp)", Nd->Var->Name, Nd->Var->Offset);
        printLn("  addi a0, fp, %d", Nd->Var->Offset);
      }else{ // 全局变量
        printLn("  # 获取全局变量%s的地址", Nd->Var->Name);
        printLn("  la a0, %s", Nd->Var->Name);  // 全局变量存放在符号表中, data段
      }
      return;
    case ND_DEREF:
      // 解引用*
      genExpr(Nd->LHS);
      return;
    // 逗号
    case ND_COMMA:
      genExpr(Nd->LHS);
      genAddr(Nd->RHS);
      return;
    // 结构体成员
    case ND_MEMBER:
      genAddr(Nd->LHS);
      printLn("  # 计算成员变量的地址偏移量");
      printLn("  addi a0, a0, %d", Nd->Mem->Offset);
      return;
    default:
      break;
  }

  errorTok(Nd->Tok, "not an lvalue");
}

// 加载a0指向的值
static void load(Type *Ty) {
  if (Ty->Kind == TY_ARRAY || Ty->Kind == TY_STRUCT || Ty->Kind == TY_UNION)
    return;

  printLn("  # 读取a0中存放的地址, 得到的值存入a0");
  if(Ty->Size == 1){
    printLn("  lb a0, 0(a0)");
  }else if (Ty->Size == 2){
    printLn("  lh a0, 0(a0)");
  }else if (Ty->Size == 4){
    printLn("  lw a0, 0(a0)");
  }else{
    printLn("  ld a0, 0(a0)");
  }
}

// 将a0存入栈顶值(为一个地址)
static void store(Type *Ty) {
  pop("a1");
  
  if (Ty->Kind == TY_STRUCT || Ty->Kind == TY_UNION) {
    printLn("  # asign to %s", Ty->Kind == TY_STRUCT ? "struct" : "union");
    for (int I = 0; I < Ty->Size; ++I) {
      printLn("  lb a2, %d(a0)", I);
      printLn("  sb a2, %d(a1)", I);
    }
    return;
  }

  printLn("  # 将a0的值, 写入到a1中存放的地址");
  if (Ty->Size == 1){
    printLn("  sb a0, 0(a1)");
  }else if (Ty->Size == 2){
    printLn("  sh a0, 0(a1)");
  }else if (Ty->Size == 4){
    printLn("  sw a0, 0(a1)");
  }else{
    printLn("  sd a0, 0(a1)");
  }
};

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
  if (To->Kind == TY_BOOL) {
    printLn("  # 转为bool类型:为0置0,非0置1");
    printLn("  snez a0, a0");
    return;
  }

  // 获取类型的枚举值
  int T1 = getTypeId(From);
  int T2 = getTypeId(To);
  if (castTable[T1][T2]) {
    printLn("  # 转换函数");
    printLn("%s", castTable[T1][T2]);
  }
}

// 生成表达式
static void genExpr(Node *Nd) {
  // .loc 文件编号 行号
  printLn("  .loc 1 %d", Nd->Tok->LineNo);
  // 生成各个根节点
  switch (Nd->Kind) {
  // 加载数字到a0
  case ND_NUM:
    printLn("  li a0, %ld", Nd->Val);
    return;
  // 对寄存器取反
  case ND_NEG:
    genExpr(Nd->LHS);
    // neg a0, a0是sub a0, x0, a0的别名, 即a0=0-a0
    printLn("  neg a0, a0");
    // printLn("  sub a0, x0, a0");
    return;
  // 非运算
  case ND_NOT:
    genExpr(Nd->LHS);
    printLn("  # 非运算");
    // a0=0则置1，否则为0
    printLn("  seqz a0, a0");
    return;
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
  // 按位取非运算
  case ND_BITNOT:
    genExpr(Nd->LHS);
    printLn("  # 按位取反");
    // 这里的 not a0, a0 为 xori a0, a0, -1 的伪码
    // printLn("  not a0, a0");
    printLn("  xori a0, a0, -1");
    return;
  // 变量
  case ND_VAR:
  case ND_MEMBER:
    // 计算出变量的地址，然后存入a0
    genAddr(Nd);
    // 访问a0地址中存储的数据，存入到a0当中
    load(Nd->Ty);
    return;
  // 赋值
  case ND_ASSIGN:
    // 左部是左值，保存值到的地址
    genAddr(Nd->LHS);
    push();
    // 右部是右值，为表达式的值
    genExpr(Nd->RHS);
    store(Nd->Ty);
    return;
  // 解引用 *   *addr  load addr to a0   then load 0(a0) to a0
  case ND_DEREF:
    genExpr(Nd->LHS);
    load(Nd->Ty);
    return;
  // 取地址
  case ND_ADDR: // &a / & *a
    genAddr(Nd->LHS);
    return;
  // 语句表达式
  case ND_STMT_EXPR:
    for (Node *N = Nd->Body; N; N = N->Next)
      genStmt(N);
    return;
  // 逗号
  case ND_COMMA:
    genExpr(Nd->LHS);
    genExpr(Nd->RHS);
    return;
  // 类型转换
  case ND_CAST:
    genExpr(Nd->LHS);
    cast(Nd->LHS->Ty, Nd->Ty);
    return;
  // 函数调用
  case ND_FUNCALL: {
    // 记录参数个数
    int NArgs = 0;
    // 计算所有参数的值，正向压栈
    for (Node *Arg = Nd->Args; Arg; Arg = Arg->Next) {
      genExpr(Arg);
      push();
      NArgs++;
    }

    // 反向弹栈，a0->参数1，a1->参数2……
    for (int i = NArgs - 1; i >= 0; i--)
      pop(ArgReg[i]);

    // 调用函数
    printLn("  # 调用%s函数", Nd->FuncName);
    printLn("  call %s", Nd->FuncName);
    return;
  }
    return;
  default:
    break;
  }

  // 递归到最右节点
  genExpr(Nd->RHS);
  // 将结果压入栈
  push();
  // 递归到左节点
  genExpr(Nd->LHS);
  // 将结果弹栈到a1
  pop("a1");

  // 生成各个二叉树节点
  char *Suffix = Nd->LHS->Ty->Kind == TY_LONG || Nd->LHS->Ty->Base ? "" : "w";
  switch (Nd->Kind) {
  case ND_ADD: // + a0=a0+a1
    printLn("  add%s a0, a0, a1", Suffix);
    return;
  case ND_SUB: // - a0=a0-a1
    printLn("  sub%s a0, a0, a1", Suffix);
    return;
  case ND_MUL: // * a0=a0*a1
    printLn("  mul%s a0, a0, a1", Suffix);
    return;
  case ND_DIV: // / a0=a0/a1
    printLn("  div%s a0, a0, a1", Suffix);
    return;
  case ND_MOD: // % a0=a0%a1
    printLn("  # a0%%a1, 结果写入a0");
    printLn("  rem%s a0, a0, a1", Suffix);
    return;
  case ND_BITAND: // & a0=a0&a1
    printLn("  # a0&a1, 结果写入a0");
    printLn("  and a0, a0, a1");
    return;
  case ND_BITOR: // | a0=a0|a1
    printLn("  # a0|a1, 结果写入a0");
    printLn("  or a0, a0, a1");
    return;
  case ND_BITXOR: // ^ a0=a0^a1
    printLn("  # a0^a1, 结果写入a0");
    printLn("  xor a0, a0, a1");
    return;
  case ND_EQ:
  case ND_NE:
    // a0=a0^a1，异或指令
    printLn("  xor a0, a0, a1");

    if (Nd->Kind == ND_EQ)
      // a0==a1
      // a0=a0^a1, sltiu a0, a0, 1
      // 等于0则置1
      printLn("  seqz a0, a0");
    else
      // a0!=a1
      // a0=a0^a1, sltu a0, x0, a0
      // 不等于0则置1
      printLn("  snez a0, a0");
    return;
  case ND_LT:
    printLn("  slt a0, a0, a1");
    return;
  case ND_LE:
    // a0<=a1等价于
    // a0=a1<a0, a0=a1^1
    printLn("  slt a0, a1, a0");
    printLn("  xori a0, a0, 1");
    return;
  default:
    break;
  }

  errorTok(Nd->Tok, "invalid expression");
}

// 生成语句
static void genStmt(Node *Nd) {
  // .loc 文件编号 行号
  printLn("  .loc 1 %d", Nd->Tok->LineNo);
  switch(Nd->Kind){
    // 生成for / while循环语句
    case ND_FOR: {
      // 代码段计数
      int C = count();
      // 生成初始化语句
      if (Nd->Init)
        genStmt(Nd->Init);
      // 输出循环头部标签
      printLn(".L.begin.%d:", C);
      // 处理循环条件语句
      if (Nd->Cond) {
        // 生成条件循环语句
        genExpr(Nd->Cond);
        // 判断结果是否为0，为0则跳转到结束部分
        printLn("  beqz a0, .L.end.%d", C);
      }
      // 生成循环体语句
      genStmt(Nd->Then);
      // 处理循环递增语句
      if (Nd->Inc)
        // 生成循环递增语句
        genExpr(Nd->Inc);
      // 跳转到循环头部
      printLn("  j .L.begin.%d", C);
      // 输出循环尾部标签
      printLn(".L.end.%d:", C);
      return;
    }
    // 生成if语句
    case ND_IF: {
      // 代码段计数
      int C = count();
      // 生成条件内语句
      genExpr(Nd->Cond);
      // 判断结果是否为0，为0则跳转到else标签
      printLn("  beqz a0, .L.else.%d", C);
      // 生成符合条件后的语句
      genStmt(Nd->Then);
      // 执行完后跳转到if语句后面的语句
      printLn("  j .L.end.%d", C);
      // else代码块，else可能为空，故输出标签
      printLn(".L.else.%d:", C);
      // 生成不符合条件后的语句
      if (Nd->Els)
        genStmt(Nd->Els);
      // 结束if语句，继续执行后面的语句
      printLn(".L.end.%d:", C);
      return;
    }
    // 生成代码块，遍历代码块的语句链表
    case ND_BLOCK:
      for (Node *N = Nd->Body; N; N = N->Next)
        genStmt(N);
      return;
    case ND_RETURN:
      genExpr(Nd->LHS);
      // 无条件跳转语句，跳转到.L.return段
      // j offset是 jal x0, offset的别名指令
      printLn("  # 跳转到.L.return.%s段", CurrentFn->Name);
      printLn("  j .L.return.%s", CurrentFn->Name);
      return;
    // 生成表达式语句
    case ND_EXPR_STMT:
      genExpr(Nd->LHS);
      return;
    default:
      break;
  }

  errorTok(Nd->Tok, "invalid statement");
}


// 根据变量的链表计算出偏移量
static void assignLVarOffsets(Obj *Prog) {
  // 为每个函数计算其变量所用的栈空间
  for (Obj *Fn = Prog; Fn; Fn = Fn->Next) {
    if(! Fn->IsFunction) // 不是函数
      continue;
    int Offset = 0;
    // 读取所有变量
    for (Obj *Var = Fn->Locals; Var; Var = Var->Next) {
      // 每个变量分配size字节
      Offset += Var->Ty->Size;
      // 对齐变量
      Offset = alignTo(Offset, Var->Ty->Align);
      // 为每个变量赋一个偏移量，或者说是栈中地址
      Var->Offset = -Offset;
    }
    // 将栈对齐到16字节
    Fn->StackSize = alignTo(Offset, 16);
  }
}

// 将整形寄存器的值存入栈中
static void storeGeneral(int Reg, int Offset, int Size) {
  printLn("  # 将%s寄存器的值存入%d(fp)的栈地址", ArgReg[Reg], Offset);
  switch (Size) {
  case 1:
    printLn("  sb %s, %d(fp)", ArgReg[Reg], Offset);
    return;
  case 2:
    printLn("  sh %s, %d(fp)", ArgReg[Reg], Offset);
    return;
  case 4:
    printLn("  sw %s, %d(fp)", ArgReg[Reg], Offset);
    return;
  case 8:
    printLn("  sd %s, %d(fp)", ArgReg[Reg], Offset);
    return;
  }
  unreachable();
}

void genFun(Obj *Fn){
  if (Fn->IsStatic) {
    printLn("\n  # 定义局部%s函数", Fn->Name);
    printLn("  .local %s", Fn->Name);
  } else {
    printLn("\n  # 定义全局%s函数", Fn->Name);
    printLn("  .globl %s", Fn->Name);
  }
  printLn("  .text");  // 后面要有 .data
  printLn("# =====%s段开始===============", Fn->Name);
  printLn("# %s段标签", Fn->Name);
  printLn("%s:", Fn->Name);
  CurrentFn = Fn;

  // 栈布局
  //-------------------------------// sp
  //              ra
  //-------------------------------// ra = sp-8
  //              fp
  //-------------------------------// fp = sp-16
  //             变量
  //-------------------------------// sp = sp-16-StackSize
  //           表达式计算
  //-------------------------------//

  // Prologue, 前言
  // 将ra寄存器压栈,保存ra的值
  printLn("  # 将ra寄存器压栈,保存ra的值");
  printLn("  addi sp, sp, -16"); // 分配两个位置
  printLn("  sd ra, 8(sp)");
  // 将fp压入栈中，保存fp的值
  printLn("  sd fp, 0(sp)");
  // 将sp写入fp
  printLn("  mv fp, sp");

  // 偏移量为实际变量所用的栈大小
  printLn("  addi sp, sp, -%d", Fn->StackSize);

  int I = 0;
  for (Obj *Var = Fn->Params; Var; Var = Var->Next) {
    storeGeneral(I++, Var->Offset, Var->Ty->Size);
  }
  // 生成语句链表的代码
  genStmt(Fn->Body);
  assert(Depth == 0);

  // Epilogue，后语
  // 输出return段标签
  printLn(".L.return.%s:", Fn->Name);
  // 将fp的值改写回sp
  printLn("  mv sp, fp");
  // 将最早fp保存的值弹栈，恢复fp。
  printLn("  ld fp, 0(sp)");
  // 将ra寄存器弹栈,恢复ra的值
  printLn("  # 将ra寄存器弹栈,恢复ra的值");
  printLn("  ld ra, 8(sp)");
  printLn("  addi sp, sp, 16");

  // 返回
  printLn("  ret");
}

// .data 全局变量
static void emitData(Obj *Prog) {
  for (Obj *Var = Prog; Var; Var = Var->Next) {
    if (Var->IsFunction )
      continue;

    printLn("  # 数据段标签");
    printLn("  .data");
    // 判断是否有初始值
    if (Var->InitData) {
      printLn("%s:", Var->Name);
      // 打印出字符串的内容，包括转义字符
      printLn("  # 字符串字面量");
      for (int I = 0; I < Var->Ty->Size; ++I) {
        char C = Var->InitData[I];
        if (isprint(C))
          printLn("  .byte %d\t# %c", C, C);
        else
          printLn("  .byte %d", C);
      }
    } else {
      printLn("  # 全局段%s", Var->Name);
      printLn("  .globl %s", Var->Name);
      printLn("%s:", Var->Name);
      printLn("  # 全局变量零填充%d位", Var->Ty->Size);
      printLn("  .zero %d", Var->Ty->Size);
    }
  }
}

// .test 函数
void emitText(Obj *Prog) {
  // 为每个函数单独生成代码
  for (Obj *Fn = Prog; Fn; Fn = Fn->Next) {
    if(!Fn->IsFunction || !Fn->IsDefinition)
      continue;
    genFun(Fn);
  }
}

// 代码生成入口函数，包含代码块的基础信息
void codegen(Obj *Prog, FILE *out){
  // 设置目标文件的文件流指针
  OutputFile = out;
  // 计算局部变量的偏移量
  assignLVarOffsets(Prog);
  // 生成数据
  emitData(Prog);
  // 生成代码
  emitText(Prog);
}
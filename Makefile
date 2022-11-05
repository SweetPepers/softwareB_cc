# C编译器参数：使用C11标准，生成debug信息，禁止将未初始化的全局变量放入到common段
CFLAGS=-std=c11 -g -fno-common
# 指定C编译器，来构建项目
CC=gcc
# C源代码文件，表示所有的.c结尾的文件
SRCS=$(wildcard *.c)
# C文件编译生成的未链接的可重定位文件，将所有.c文件替换为同名的.o结尾的文件名
OBJS=$(SRCS:.c=.o)

# cc标签，表示如何构建最终的二进制文件，依赖于所有的.o文件
# $@表示目标文件，此处为cc，$^表示依赖文件，此处为$(OBJS)
cc: $(OBJS)
# 将多个*.o文件编译为cc
	$(CC) $(CFLAGS) -o $@ $^

# 所有的可重定位文件依赖于cc.h的头文件
$(OBJS): cc.h

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

# 测试标签，运行测试
test/%.exe: cc test/%.c
	$(CC) -o- -E -P -C test/$*.c | ./cc -o test/$*.s -
	riscv64-linux-gnu-gcc -static -o $@ test/$*.s -xc test/common

run/%: test/%.exe
	qemu-riscv64 -L $(RISCV)/sysroot test/$*.exe || exit 1

test: $(TESTS)
	for i in $^; do echo $$i; qemu-riscv64 -L $(RISCV)/sysroot ./$$i || exit 1; echo; done

# 清理标签，清理所有非源代码文件
clean:
	rm -rf cc tmp* $(TESTS) test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'
# 伪目标，没有实际的依赖文件
.PHONY: test clean
#!/bin/bash

echo "🚀 开始运行AST构建批量测试..."

# 检查是否存在测试文件
if [ ! -d "Code/sy/Func/src" ]; then
    echo "❌ 错误: 找不到测试目录 Code/sy/Func/src"
    exit 1
fi

# 编译测试程序
echo "🔨 编译测试程序..."
cargo build --bin test_ast_batch --release 2>/dev/null || {
    echo "❌ 编译失败，尝试重新编译..."
    cargo build --bin test_ast_batch
    if [ $? -ne 0 ]; then
        echo "❌ 编译失败，请检查代码"
        exit 1
    fi
}

echo "✅ 编译成功"

# 运行测试
echo "🧪 运行AST构建测试..."
./target/release/test_ast_batch

echo ""
echo "📋 测试完成！"
echo "📄 详细报告请查看 test_ast_report.txt"

# 显示测试结果统计
if [ -f "test_ast_report.txt" ]; then
    echo ""
    echo "📊 测试结果摘要:"
    echo "=================="
    grep -E "(总测试数|通过测试|失败测试|成功率)" test_ast_report.txt
fi

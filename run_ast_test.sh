#!/bin/bash

echo "🚀 开始运行AST构建批量测试..."

# 检查是否存在测试文件
if [ ! -d "Code/sy/Func/src" ]; then
    echo "❌ 错误: 找不到测试目录 Code/sy/Func/src"
    exit 1
fi

# 检查测试文件是否存在
if [ ! -f "examples/test_ast_batch.rs" ]; then
    echo "❌ 错误: 找不到测试文件 examples/test_ast_batch.rs"
    exit 1
fi

# 运行示例程序
echo "🧪 运行AST构建测试..."
if cargo run --example test_ast_batch; then
    echo "✅ 测试运行成功"
else
    echo "❌ 测试运行失败"
    exit 1
fi

echo ""
echo "📋 测试完成！"
echo "📄 详细报告请查看 test_ast_report.txt"

# 显示测试结果统计
if [ -f "test_ast_report.txt" ]; then
    echo ""
    echo "📊 测试结果摘要:"
    echo "=================="
    grep -E "(总测试数|通过测试|失败测试|成功率)" test_ast_report.txt
else
    echo "⚠️  警告: 未找到测试报告文件 test_ast_report.txt"
fi

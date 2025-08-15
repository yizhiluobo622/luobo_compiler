pub mod ast_to_SoNir; 
pub mod SoN_optimization;

use SoN_optimization::{OptimizationPass, OptimizationResult, OptimizationStats};
use ast_to_SoNir::son_ir::SonIr;

/// 生成Sea of Nodes IR的DOT图
pub mod dot_graph {
    use crate::ast_to_cfg::ast_to_SoNir::son_ir::{SonIr, SonNode, SonNodeId, SonEdge, OpCode, EdgeType};
    use std::fmt::Write;

    /// 生成Graphviz DOT格式的图
    pub fn generate_dot(son_ir: &SonIr) -> Result<String, String> {
        let mut output = String::new();
        
        writeln!(output, "digraph SeaOfNodes {{").map_err(|e| e.to_string())?;
        writeln!(output, "  rankdir=BT;").map_err(|e| e.to_string())?;
        writeln!(output, "  node [shape=box, style=filled];").map_err(|e| e.to_string())?;
        
        // 添加节点
        for (node_id, node) in son_ir.get_all_nodes() {
            let label = get_node_label(*node_id, node);
            let color = get_node_color(node);
            writeln!(output, "  {} [label=\"{}\", fillcolor=\"{}\"];", 
                node_id, label, color).map_err(|e| e.to_string())?;
        }
        
        // 添加边（显示use-def关系：从使用到定义）
        for edge in son_ir.get_all_edges() {
            let (style, color) = match edge.edge_type {
                EdgeType::Control => ("solid", "red"),
                EdgeType::Data => ("solid", "black"),
                EdgeType::Virtual => ("dotted", "gray"), // 虚拟边显示为虚线
            };
            
            // 虚拟边已经有专门的类型，不需要额外检测
            let final_style = style;
            let final_color = color;
            
            // 标注操作数位置（左/右）
            let label = get_edge_label(son_ir, edge);
            
            writeln!(output, "  {} -> {} [style={}, color={}{}];", 
                edge.to, edge.from, final_style, final_color, label).map_err(|e| e.to_string())?;
        }
        
        writeln!(output, "}}").map_err(|e| e.to_string())?;
        Ok(output)
    }

    /// 获取节点标签
    fn get_node_label(node_id: SonNodeId, node: &SonNode) -> String {
        match &node.kind.opcode {
            OpCode::Start => format!("Start_{}", node_id),
            OpCode::Return => format!("Return_{}", node_id),
            OpCode::Constant => {
                match &node.kind.data {
                    crate::ast_to_cfg::ast_to_SoNir::son_ir::NodeData::Constant { value, .. } => {
                        format!("Const_{:?}_{}", value, node_id)
                    }
                    _ => format!("Const_{}", node_id)
                }
            }
            OpCode::Local => {
                match &node.kind.data {
                    crate::ast_to_cfg::ast_to_SoNir::son_ir::NodeData::Local { name, .. } => {
                        format!("Local_{}_{}", name, node_id)
                    }
                    _ => format!("Local_{}", node_id)
                }
            }
            _ => format!("{:?}_{}", node.kind.opcode, node_id)
        }
    }

    /// 获取节点颜色
    fn get_node_color(node: &SonNode) -> &'static str {
        match node.kind.opcode {
            OpCode::Start => "lightblue",
            OpCode::Return => "lightcoral",
            OpCode::Constant => "lightgreen",
            OpCode::Local => "lightyellow",
            _ => "lightgray"
        }
    }
    
    /// 获取边的标签（标注操作数位置）
    fn get_edge_label(son_ir: &SonIr, edge: &SonEdge) -> String {
        // 只对数据边标注操作数位置
        if edge.edge_type != EdgeType::Data {
            return String::new();
        }
        
        // 获取目标节点（运算节点）
        if let Some(target_node) = son_ir.get_node(edge.to) {
            // 检查是否是二元运算节点
            if matches!(target_node.kind.opcode, 
                OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide) {
                
                // 找到这个输入在inputs数组中的位置
                if let Some(position) = target_node.inputs.iter().position(|&id| id == edge.from) {
                    match position {
                        0 => ", label=\"left\"",
                        1 => ", label=\"right\"",
                        _ => "",
                    }.to_string()
                } else {
                    String::new()
                }
            } else if matches!(target_node.kind.opcode, OpCode::Minus) {
                // 一元运算节点
                if let Some(position) = target_node.inputs.iter().position(|&id| id == edge.from) {
                    if position == 0 {
                        ", label=\"operand\"".to_string()
                    } else {
                        String::new()
                    }
                } else {
                    String::new()
                }
            } else if target_node.kind.opcode == OpCode::Return {
                // Return节点的第二个输入是返回值
                if let Some(position) = target_node.inputs.iter().position(|&id| id == edge.from) {
                    match position {
                        0 => ", label=\"control\"",
                        1 => ", label=\"value\"",
                        _ => "",
                    }.to_string()
                } else {
                    String::new()
                }
            } else {
                String::new()
            }
        } else {
            String::new()
        }
    }
}

/// 便利函数：创建并运行常量传播优化
pub fn run_constant_propagation(son_ir: &mut ast_to_SoNir::son_ir::SonIr) -> Result<SoN_optimization::OptimizationResult, String> {
    let mut pass = SoN_optimization::constant_propagation::ConstantPropagationPass::new();
    pass.run(son_ir)
}

/// 便利函数：创建并运行所有优化Pass
pub fn run_all_optimizations(son_ir: &mut ast_to_SoNir::son_ir::SonIr) -> Result<Vec<SoN_optimization::OptimizationResult>, String> {
    let mut results = Vec::new();
    
    // 运行常量传播优化
    let constant_prop_result = run_constant_propagation(son_ir)?;
    results.push(constant_prop_result);
    
    // 这里可以添加更多优化Pass
    // 例如：死代码消除、循环优化等
    
    Ok(results)
}

/// 便利函数：获取优化统计摘要
pub fn get_optimization_summary(son_ir: &mut ast_to_SoNir::son_ir::SonIr) -> Result<String, String> {
    let mut summary = String::new();
    summary.push_str("=== 优化统计摘要 ===\n");
    
    // 运行常量传播优化并获取统计
    let mut pass = SoN_optimization::constant_propagation::ConstantPropagationPass::new();
    let _result = pass.run(son_ir)?;
    let stats = pass.get_stats();
    
    summary.push_str(&format!("常量折叠次数: {}\n", stats.constant_foldings));
    summary.push_str(&format!("常量传播次数: {}\n", stats.constant_propagations));
    summary.push_str(&format!("死代码消除次数: {}\n", stats.dead_code_eliminations));
    summary.push_str(&format!("优化轮次: {}\n", stats.optimization_rounds));
    
    Ok(summary)
}

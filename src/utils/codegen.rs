use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct CodeGenerator {
    pub target_architecture: String,
    pub register_map: HashMap<String, String>,
    pub instruction_map: HashMap<String, String>,
}

impl CodeGenerator {
    pub fn new(target: &str) -> Self {
        let mut register_map = HashMap::new();
        let mut instruction_map = HashMap::new();
        
        // 初始化寄存器映射
        match target {
            "x86_64" => {
                register_map.insert("rax".to_string(), "rax".to_string());
                register_map.insert("rbx".to_string(), "rbx".to_string());
                register_map.insert("rcx".to_string(), "rcx".to_string());
                register_map.insert("rdx".to_string(), "rdx".to_string());
                register_map.insert("rsi".to_string(), "rsi".to_string());
                register_map.insert("rdi".to_string(), "rdi".to_string());
                register_map.insert("r8".to_string(), "r8".to_string());
                register_map.insert("r9".to_string(), "r9".to_string());
                register_map.insert("r10".to_string(), "r10".to_string());
                register_map.insert("r11".to_string(), "r11".to_string());
                register_map.insert("r12".to_string(), "r12".to_string());
                register_map.insert("r13".to_string(), "r13".to_string());
                register_map.insert("r14".to_string(), "r14".to_string());
                register_map.insert("r15".to_string(), "r15".to_string());
                
                // 初始化指令映射
                instruction_map.insert("add".to_string(), "add".to_string());
                instruction_map.insert("sub".to_string(), "sub".to_string());
                instruction_map.insert("mul".to_string(), "imul".to_string());
                instruction_map.insert("div".to_string(), "idiv".to_string());
                instruction_map.insert("mov".to_string(), "mov".to_string());
                instruction_map.insert("jmp".to_string(), "jmp".to_string());
                instruction_map.insert("je".to_string(), "je".to_string());
                instruction_map.insert("jne".to_string(), "jne".to_string());
                instruction_map.insert("jl".to_string(), "jl".to_string());
                instruction_map.insert("jle".to_string(), "jle".to_string());
                instruction_map.insert("jg".to_string(), "jg".to_string());
                instruction_map.insert("jge".to_string(), "jge".to_string());
                instruction_map.insert("call".to_string(), "call".to_string());
                instruction_map.insert("ret".to_string(), "ret".to_string());
                instruction_map.insert("push".to_string(), "push".to_string());
                instruction_map.insert("pop".to_string(), "pop".to_string());
            }
            "aarch64" => {
                // ARM64寄存器映射
                register_map.insert("x0".to_string(), "x0".to_string());
                register_map.insert("x1".to_string(), "x1".to_string());
                register_map.insert("x2".to_string(), "x2".to_string());
                register_map.insert("x3".to_string(), "x3".to_string());
                register_map.insert("x4".to_string(), "x4".to_string());
                register_map.insert("x5".to_string(), "x5".to_string());
                register_map.insert("x6".to_string(), "x6".to_string());
                register_map.insert("x7".to_string(), "x7".to_string());
                register_map.insert("x8".to_string(), "x8".to_string());
                register_map.insert("x9".to_string(), "x9".to_string());
                register_map.insert("x10".to_string(), "x10".to_string());
                register_map.insert("x11".to_string(), "x11".to_string());
                register_map.insert("x12".to_string(), "x12".to_string());
                register_map.insert("x13".to_string(), "x13".to_string());
                register_map.insert("x14".to_string(), "x14".to_string());
                register_map.insert("x15".to_string(), "x15".to_string());
                
                // ARM64指令映射
                instruction_map.insert("add".to_string(), "add".to_string());
                instruction_map.insert("sub".to_string(), "sub".to_string());
                instruction_map.insert("mul".to_string(), "mul".to_string());
                instruction_map.insert("div".to_string(), "sdiv".to_string());
                instruction_map.insert("mov".to_string(), "mov".to_string());
                instruction_map.insert("jmp".to_string(), "b".to_string());
                instruction_map.insert("je".to_string(), "beq".to_string());
                instruction_map.insert("jne".to_string(), "bne".to_string());
                instruction_map.insert("jl".to_string(), "blt".to_string());
                instruction_map.insert("jle".to_string(), "ble".to_string());
                instruction_map.insert("jg".to_string(), "bgt".to_string());
                instruction_map.insert("jge".to_string(), "bge".to_string());
                instruction_map.insert("call".to_string(), "bl".to_string());
                instruction_map.insert("ret".to_string(), "ret".to_string());
            }
            _ => {
                // 默认x86_64
                register_map.insert("rax".to_string(), "rax".to_string());
                instruction_map.insert("add".to_string(), "add".to_string());
            }
        }
        
        Self {
            target_architecture: target.to_string(),
            register_map,
            instruction_map,
        }
    }
    
    pub fn generate_assembly(&self, instructions: &[String]) -> String {
        let mut assembly = String::new();
        
        // 添加汇编头部
        assembly.push_str(&self.generate_header());
        
        // 生成指令
        for instruction in instructions {
            assembly.push_str(&self.translate_instruction(instruction));
            assembly.push('\n');
        }
        
        // 添加汇编尾部
        assembly.push_str(&self.generate_footer());
        
        assembly
    }
    
    fn generate_header(&self) -> String {
        match self.target_architecture.as_str() {
            "x86_64" => {
                r#"
section .text
global _start

_start:
"#.to_string()
            }
            "aarch64" => {
                r#"
.text
.global _start

_start:
"#.to_string()
            }
            _ => {
                r#"
section .text
global _start

_start:
"#.to_string()
            }
        }
    }
    
    fn generate_footer(&self) -> String {
        match self.target_architecture.as_str() {
            "x86_64" => {
                r#"
    mov rax, 60
    mov rdi, 0
    syscall
"#.to_string()
            }
            "aarch64" => {
                r#"
    mov x0, #0
    mov x8, #93
    svc #0
"#.to_string()
            }
            _ => {
                r#"
    mov rax, 60
    mov rdi, 0
    syscall
"#.to_string()
            }
        }
    }
    
    fn translate_instruction(&self, instruction: &str) -> String {
        // 简化实现：翻译指令到目标架构
        let parts: Vec<&str> = instruction.split_whitespace().collect();
        if parts.is_empty() {
            return String::new();
        }
        
        let opcode = parts[0];
        let opcode_string = opcode.to_string();
        let translated_opcode = self.instruction_map.get(opcode).unwrap_or(&opcode_string);
        
        match translated_opcode.as_str() {
            "add" | "sub" | "mul" | "div" | "mov" => {
                if parts.len() >= 3 {
                    format!("    {} {}, {}", translated_opcode, parts[1], parts[2])
                } else {
                    format!("    {}", translated_opcode)
                }
            }
            "jmp" | "je" | "jne" | "jl" | "jle" | "jg" | "jge" => {
                if parts.len() >= 2 {
                    format!("    {} {}", translated_opcode, parts[1])
                } else {
                    format!("    {}", translated_opcode)
                }
            }
            "call" | "ret" | "push" | "pop" => {
                if parts.len() >= 2 {
                    format!("    {} {}", translated_opcode, parts[1])
                } else {
                    format!("    {}", translated_opcode)
                }
            }
            _ => {
                format!("    {}", instruction)
            }
        }
    }
    
    pub fn allocate_register(&self, virtual_reg: &str) -> String {
        self.register_map.get(virtual_reg)
            .unwrap_or(&format!("r{}", virtual_reg))
            .clone()
    }
    
    pub fn translate_register(&self, reg: &str) -> String {
        self.register_map.get(reg)
            .unwrap_or(&reg.to_string())
            .clone()
    }
}

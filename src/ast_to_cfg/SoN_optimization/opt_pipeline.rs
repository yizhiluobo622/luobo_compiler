// Optimization pipeline for SoN IR
// This module manages the execution order and dependencies of optimization passes

use crate::ast_to_cfg::ast_to_SoNir::son_ir::SonIr;
use crate::ast_to_cfg::SoN_optimization::{
    //constant_propagation::ConstantPropagation,
    cse::CommonSubexpressionElimination,
    node_dce::NodeDCE,
    global_dce::GlobalDCE,
};

pub struct OptimizationPipeline {
    // First round: basic optimizations
    //constant_propagation: ConstantPropagation,
    cse: CommonSubexpressionElimination,
    node_dce: NodeDCE,
    global_dce: GlobalDCE,
    
    // Configuration
    config: OptimizationConfig,
}

pub struct OptimizationConfig {
    pub enable_round1: bool,      // Basic optimizations
    pub enable_round2: bool,      // Advanced optimizations (pattern matching, peephole, inlining)
    pub max_iterations: usize,    // Maximum iterations for each round
    pub enable_debug: bool,       // Enable debug output
}

impl Default for OptimizationConfig {
    fn default() -> Self {
        Self {
            enable_round1: true,
            enable_round2: false,  // Disabled by default until we implement pattern matching
            max_iterations: 10,    // Increased from 3 to 10 for better constant propagation
            enable_debug: false,
        }
    }
}

impl OptimizationPipeline {
    pub fn new() -> Self {
        Self {
            //constant_propagation: ConstantPropagation::new(),
            cse: CommonSubexpressionElimination::new(),
            node_dce: NodeDCE::new(),
            global_dce: GlobalDCE::new(),
            config: OptimizationConfig::default(),
        }
    }
    
    pub fn with_config(config: OptimizationConfig) -> Self {
        Self {
            //constant_propagation: ConstantPropagation::new(),
            cse: CommonSubexpressionElimination::new(),
            node_dce: NodeDCE::new(),
            global_dce: GlobalDCE::new(),
            config,
        }
    }
    
    /// Run the complete optimization pipeline on a SoN IR
    pub fn run(&mut self, son_ir: &mut SonIr) {
        if self.config.enable_round1 {
            self.run_round1_basic_optimizations(son_ir);
        }
        
        if self.config.enable_round2 {
            self.run_round2_advanced_optimizations(son_ir);
        }
    }
    
    /// Round 1: Basic optimizations (constant propagation, CSE, DCE)
    fn run_round1_basic_optimizations(&mut self, son_ir: &mut SonIr) {
        if self.config.enable_debug {
            println!("Starting Round 1: Basic Optimizations");
        }
        
        for iteration in 0..self.config.max_iterations {
            if self.config.enable_debug {
                println!("  Iteration {}", iteration + 1);
            }
            
            // Run basic optimizations in order
            //self.constant_propagation.run(son_ir);
            self.cse.run(son_ir);
            self.node_dce.run(son_ir);
            self.global_dce.run(son_ir);
            
            // TODO: Check if any changes were made to determine if we should continue
            // For now, we'll always run max_iterations times
        }
    }
    
    /// Round 2: Advanced optimizations (pattern matching, peephole, local inlining)
    fn run_round2_advanced_optimizations(&mut self, son_ir: &mut SonIr) {
        if self.config.enable_debug {
            println!("Starting Round 2: Advanced Optimizations");
        }
        
        // TODO: Implement when we have pattern matching, peephole, and inlining
        // For now, this is a placeholder
        if self.config.enable_debug {
            println!("  Pattern matching: Not implemented yet");
            println!("  Peephole optimization: Not implemented yet");
            println!("  Local inlining: Not implemented yet");
        }
    }
    
    /// Enable Round 2 optimizations (call this when pattern matching is ready)
    pub fn enable_advanced_optimizations(&mut self) {
        self.config.enable_round2 = true;
    }
    
    /// Disable Round 2 optimizations
    pub fn disable_advanced_optimizations(&mut self) {
        self.config.enable_round2 = false;
    }
    
    /// Set maximum iterations for optimization rounds
    pub fn set_max_iterations(&mut self, max_iterations: usize) {
        self.config.max_iterations = max_iterations;
    }
    
    /// Enable debug output
    pub fn enable_debug(&mut self) {
        self.config.enable_debug = true;
    }
}

// SoN optimization module
// This module contains various optimization passes for SoN IR

pub mod constant_propagation;
pub mod cse;
pub mod node_dce;
pub mod global_dce;
pub mod opt_pipeline;
pub mod analysis;




// Future advanced optimizations (to be implemented)
// pub mod pattern_matching;    // Pattern matching using egraph
// pub mod peephole;           // Peephole optimization
// pub mod local_inlining;     // Local function inlining

// Re-export main optimization pipeline and types
pub use opt_pipeline::{OptimizationPipeline, OptimizationConfig};

pub use constant_propagation::ConstantPropagation;
pub use cse::CommonSubexpressionElimination;
pub use node_dce::NodeDCE;
pub use global_dce::GlobalDCE;

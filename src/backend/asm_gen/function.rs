use koopa::ir::FunctionData;

use crate::backend::{
  env::Environment,
  frame::layout_frame,
  mir::{MachineBlock, MachineFunction},
};

use super::lower::lower_value_data;

pub(super) fn lower_function(func: &FunctionData, env: &mut Environment) -> MachineFunction {
  let frame_layout = layout_frame(func);
  env.set_frame_layout(frame_layout.clone());

  let func_name = func.name().trim_start_matches('@').to_string();
  let mut blocks = Vec::new();

  for (bb, node) in func.layout().bbs() {
    let bb_name = func.dfg().bb(*bb).name();
    let label = match bb_name {
      Some(name) => name.trim_start_matches(&['@', '%'][..]).to_string(),
      None => unreachable!("Basic block without name"),
    };
    let mut block = MachineBlock {
      label,
      insts: Vec::new(),
    };

    for &inst in node.insts().keys() {
      let value_data = func.dfg().value(inst);
      env.set_inst(inst);
      block.insts.extend(lower_value_data(value_data, env));
      env.clear_inst();
    }

    blocks.push(block);
  }

  let vreg_info = env.take_vreg_info();
  env.clear_frame_layout();

  MachineFunction {
    name: func_name,
    blocks,
    frame: frame_layout,
    vreg_info,
  }
}

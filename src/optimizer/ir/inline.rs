use std::collections::{HashMap, HashSet};

use koopa::ir::builder_traits::{LocalInstBuilder, ValueBuilder};
use koopa::ir::entities::ValueData;
use koopa::ir::{
  BasicBlock, BinaryOp, Function, FunctionData, Program, TypeKind, Value, ValueKind,
};

const MAX_INLINE_INST: usize = 32;

pub fn run_inline(program: &mut Program) -> bool {
  let mut changed = false;
  let funcs: Vec<Function> = program.func_layout().iter().copied().collect();
  for func in funcs {
    let sites = {
      let func_data = program.func(func);
      collect_inline_sites(program, func, func_data)
    };
    if sites.is_empty() {
      continue;
    }
    let func_data = program.func_mut(func);
    if inline_sites_in_func(func_data, sites) {
      changed = true;
    }
  }
  changed
}

struct InlineSite {
  call_inst: Value,
  callee: InlineCallee,
}

struct InlineCallee {
  params: Vec<Value>,
  insts: Vec<Value>,
  values: HashMap<Value, ValueData>,
  ret: Option<Value>,
}

fn collect_inline_sites(
  program: &Program,
  func: Function,
  data: &FunctionData,
) -> Vec<InlineSite> {
  let mut sites = Vec::new();
  let bbs: Vec<BasicBlock> = data.layout().bbs().keys().copied().collect();
  for bb in bbs {
    let Some(node) = data.layout().bbs().node(&bb) else { continue };
    for &inst in node.insts().keys() {
      let ValueKind::Call(call) = data.dfg().value(inst).kind() else { continue };
      let callee = call.callee();
      let Some(callee_info) = build_inline_callee(program, func, callee) else { continue };
      sites.push(InlineSite {
        call_inst: inst,
        callee: callee_info,
      });
    }
  }
  sites
}

fn build_inline_callee(
  program: &Program,
  caller: Function,
  callee: Function,
) -> Option<InlineCallee> {
  if caller == callee {
    return None;
  }
  let callee_data = program.func(callee);
  let entry = callee_data.layout().entry_bb()?;
  if callee_data.layout().bbs().len() != 1 {
    return None;
  }
  if !callee_data.dfg().bb(entry).params().is_empty() {
    return None;
  }
  let ret_ty = match callee_data.ty().kind() {
    TypeKind::Function(_, ret) => ret.clone(),
    _ => return None,
  };
  if !ret_ty.is_unit() && !ret_ty.is_i32() {
    return None;
  }
  let insts: Vec<Value> = match callee_data.layout().bbs().node(&entry) {
    Some(node) => node.insts().keys().copied().collect(),
    None => return None,
  };
  if insts.is_empty() {
    return None;
  }
  let last_inst = *insts.last().expect("insts is not empty");
  let ValueKind::Return(ret) = callee_data.dfg().value(last_inst).kind() else {
    return None;
  };
  if insts[..insts.len().saturating_sub(1)]
    .iter()
    .any(|v| matches!(callee_data.dfg().value(*v).kind(), ValueKind::Return(_)))
  {
    return None;
  }
  let ret_value = ret.value();
  if ret_ty.is_unit() {
    if ret_value.is_some() {
      return None;
    }
  } else {
    let Some(rv) = ret_value else { return None };
    if !callee_data.dfg().value(rv).ty().is_i32() {
      return None;
    }
  }

  let mut values = HashMap::new();
  let mut defined: HashSet<Value> = callee_data.params().iter().copied().collect();
  let mut inst_count = 0;

  for &inst in &insts {
    let inst_data = callee_data.dfg().value(inst);
    match inst_data.kind() {
      ValueKind::Alloc(_)
      | ValueKind::Load(_)
      | ValueKind::Store(_)
      | ValueKind::GetPtr(_)
      | ValueKind::GetElemPtr(_)
      | ValueKind::Binary(_) => {
        inst_count += 1;
        if inst_count > MAX_INLINE_INST {
          return None;
        }
      }
      ValueKind::Return(_) => {}
      _ => return None,
    }

    for operand in inst_data.kind().value_uses() {
      if operand.is_global() {
        continue;
      }
      let operand_data = callee_data.dfg().value(operand);
      match operand_data.kind() {
        ValueKind::Integer(_)
        | ValueKind::ZeroInit(_)
        | ValueKind::Undef(_)
        | ValueKind::FuncArgRef(_) => {}
        ValueKind::Aggregate(_) | ValueKind::BlockArgRef(_) => return None,
        _ => {
          if !defined.contains(&operand) {
            return None;
          }
        }
      }
      if !insert_value_data(callee_data, &mut values, operand) {
        return None;
      }
    }
    if !insert_value_data(callee_data, &mut values, inst) {
      return None;
    }
    defined.insert(inst);
  }

  Some(InlineCallee {
    params: callee_data.params().to_vec(),
    insts,
    values,
    ret: ret_value,
  })
}

fn inline_sites_in_func(data: &mut FunctionData, sites: Vec<InlineSite>) -> bool {
  let mut changed = false;
  for site in sites {
    if inline_site(data, site) {
      changed = true;
    }
  }
  changed
}

fn inline_site(data: &mut FunctionData, site: InlineSite) -> bool {
  let call_inst = site.call_inst;
  let Some(bb) = data.layout().parent_bb(call_inst) else {
    return false;
  };
  let ValueKind::Call(call) = data.dfg().value(call_inst).kind() else {
    return false;
  };
  if site.callee.ret.is_none() && !data.dfg().value(call_inst).used_by().is_empty() {
    return false;
  }
  let args = call.args().to_vec();
  if args.len() != site.callee.params.len() {
    return false;
  }

  let mut value_map: HashMap<Value, Value> = HashMap::new();
  for (param, arg) in site.callee.params.iter().zip(args.iter()) {
    value_map.insert(*param, *arg);
  }

  let mut new_insts = Vec::new();
  for &inst in &site.callee.insts {
    let Some(inst_data) = site.callee.values.get(&inst) else {
      return false;
    };
    if matches!(inst_data.kind(), ValueKind::Return(_)) {
      continue;
    }
    let new_inst = match inst_data.kind() {
      ValueKind::Alloc(_) => {
        let base = match inst_data.ty().kind() {
          TypeKind::Pointer(base) => base.clone(),
          _ => return false,
        };
        data.dfg_mut().new_value().alloc(base)
      }
      ValueKind::Load(load) => {
        let src = match map_value(data, &site.callee, &mut value_map, load.src()) {
          Some(v) => v,
          None => return false,
        };
        data.dfg_mut().new_value().load(src)
      }
      ValueKind::Store(store) => {
        let value = match map_value(data, &site.callee, &mut value_map, store.value()) {
          Some(v) => v,
          None => return false,
        };
        let dest = match map_value(data, &site.callee, &mut value_map, store.dest()) {
          Some(v) => v,
          None => return false,
        };
        data.dfg_mut().new_value().store(value, dest)
      }
      ValueKind::GetPtr(ptr) => {
        let src = match map_value(data, &site.callee, &mut value_map, ptr.src()) {
          Some(v) => v,
          None => return false,
        };
        let index = match map_value(data, &site.callee, &mut value_map, ptr.index()) {
          Some(v) => v,
          None => return false,
        };
        data.dfg_mut().new_value().get_ptr(src, index)
      }
      ValueKind::GetElemPtr(ptr) => {
        let src = match map_value(data, &site.callee, &mut value_map, ptr.src()) {
          Some(v) => v,
          None => return false,
        };
        let index = match map_value(data, &site.callee, &mut value_map, ptr.index()) {
          Some(v) => v,
          None => return false,
        };
        data.dfg_mut().new_value().get_elem_ptr(src, index)
      }
      ValueKind::Binary(bin) => {
        let lhs = match map_value(data, &site.callee, &mut value_map, bin.lhs()) {
          Some(v) => v,
          None => return false,
        };
        let rhs = match map_value(data, &site.callee, &mut value_map, bin.rhs()) {
          Some(v) => v,
          None => return false,
        };
        data.dfg_mut().new_value().binary(bin.op(), lhs, rhs)
      }
      _ => return false,
    };
    value_map.insert(inst, new_inst);
    new_insts.push(new_inst);
  }

  let ret_val = match site.callee.ret {
    Some(rv) => match map_value(data, &site.callee, &mut value_map, rv) {
      Some(v) => Some(v),
      None => return false,
    },
    None => None,
  };

  {
    let mut inst_cur = data.layout_mut().bb_mut(bb).insts_mut().cursor_front_mut();
    while let Some(cur) = inst_cur.key().copied() {
      if cur == call_inst {
        break;
      }
      inst_cur.move_next();
    }
    if inst_cur.key().copied() != Some(call_inst) {
      return false;
    }
    for inst in &new_insts {
      if inst_cur.insert_key_before(*inst).is_err() {
        return false;
      }
    }
    if ret_val.is_none() {
      inst_cur.remove_current();
    }
  }

  if let Some(ret_val) = ret_val {
    let zero = data.dfg_mut().new_value().integer(0);
    data
      .dfg_mut()
      .replace_value_with(call_inst)
      .binary(BinaryOp::Add, ret_val, zero);
  } else if data.dfg().value(call_inst).used_by().is_empty() {
    data.dfg_mut().remove_value(call_inst);
  }

  true
}

fn map_value(
  data: &mut FunctionData,
  callee: &InlineCallee,
  value_map: &mut HashMap<Value, Value>,
  value: Value,
) -> Option<Value> {
  if value.is_global() {
    return Some(value);
  }
  if let Some(mapped) = value_map.get(&value) {
    return Some(*mapped);
  }
  let value_data = callee.values.get(&value)?;
  let new_value = match value_data.kind() {
    ValueKind::Integer(intv) => data.dfg_mut().new_value().integer(intv.value()),
    ValueKind::ZeroInit(_) => data.dfg_mut().new_value().zero_init(value_data.ty().clone()),
    ValueKind::Undef(_) => data.dfg_mut().new_value().undef(value_data.ty().clone()),
    ValueKind::FuncArgRef(_) => return None,
    ValueKind::Aggregate(_) | ValueKind::BlockArgRef(_) => return None,
    _ => return None,
  };
  value_map.insert(value, new_value);
  Some(new_value)
}

fn insert_value_data(
  data: &FunctionData,
  values: &mut HashMap<Value, ValueData>,
  value: Value,
) -> bool {
  if value.is_global() || values.contains_key(&value) {
    return true;
  }
  let value_data = data.dfg().value(value);
  match value_data.kind() {
    ValueKind::Aggregate(_) | ValueKind::BlockArgRef(_) => return false,
    _ => {}
  }
  values.insert(value, value_data.clone());
  true
}

use koopa::ir::{Type, TypeKind, Value};

use crate::frontend::{ast::{ConstExp, ConstInitVal, Exp, InitVal}, env::Environment, ir_gen::GenerateIR, util::{Eval, Fold}};

pub trait ExprGenerator {
  fn generate(&self, env: &mut Environment) -> Value;
  fn evaluate(&self, env: &mut Environment) -> i32;
}

impl ExprGenerator for Exp {
  fn generate(&self, env: &mut Environment) -> Value {
    // 你现有的 Exp::generate_on 实现
    Exp::generate_on(self, env)
  }

  fn evaluate(&self, env: &mut Environment) -> i32 {
    let folded = self.fold(env);
    match folded {
      Exp::Number(n) => n,
      _ => panic!("Cannot evaluate expression: {:?}", folded),
    }
  }
}

impl ExprGenerator for ConstExp {
  fn generate(&self, env: &mut Environment) -> Value {
    match self {
      ConstExp::Exp(exp) => exp.generate_on(env),
    }
  }

  fn evaluate(&self, env: &mut Environment) -> i32 {
    self.eval(env)
  }
}

/// 抽象初始化项（InitVal / ConstInitVal）
pub trait InitLike: Sized {
  type Expr: ExprGenerator;

  /// 如果是表达式型初始化值，返回 Some(&expr)
  fn as_exp(&self) -> Option<&Self::Expr>;

  /// 如果是 InitVals，返回 Some(&[Self]) —— 注意返回 slice 引用
  fn as_initvals(&self) -> Option<&[Self]>;
}

// 为 InitVal 实现 InitLike
impl InitLike for InitVal {
  type Expr = Exp;

  fn as_exp(&self) -> Option<&Self::Expr> {
    match self {
      InitVal::Exp(e) => Some(e),
      _ => None,
    }
  }

  fn as_initvals(&self) -> Option<&[Self]> {
    match self {
      InitVal::InitVals(bv) => Some(&**bv),
      _ => None,
    }
  }
}

// 为 ConstInitVal 实现 InitLike
impl InitLike for ConstInitVal {
  type Expr = ConstExp;

  fn as_exp(&self) -> Option<&Self::Expr> {
    match self {
      ConstInitVal::ConstExp(e) => Some(e),
      _ => None,
    }
  }

  fn as_initvals(&self) -> Option<&[Self]> {
    match self {
      ConstInitVal::ConstInitVals(bv) => Some(&**bv),
      _ => None,
    }
  }
}


struct InitIter<'a, T> {
  items: &'a [T],
  pos: usize,
}

impl<'a, T> InitIter<'a, T> {
  fn new(items: &'a [T]) -> Self {
    Self { items, pos: 0 }
  }

  fn peek(&self) -> Option<&'a T> {
    self.items.get(self.pos)
  }

  // 返回当前项的引用并前移（消费一项）
  fn next(&mut self) -> Option<&'a T> {
    let r = self.items.get(self.pos);
    if r.is_some() { self.pos += 1; }
    r
  }

  fn is_empty(&self) -> bool {
    self.pos >= self.items.len()
  }
}

/// 关键递归函数：从 init_iter 中“消费”尽可能多的项来完整初始化当前类型（可能是数组或标量）
/// - ty_base: 当前级别的元素类型（如果调用者是数组，此处为 base_ty）
/// - len: 当前数组长度（调用者保证这是数组级）
fn fill_array_from_iter<'a, T>(
  env: &mut Environment,
  base_ty: Type,
  len: i32,
  ptr: Value,
  iter: &mut InitIter<'a, T>,
)
where
  T: InitLike + Clone,
  T::Expr: ExprGenerator,
{
  for i in 0..len {
    let index = env.ctx.local_integer(i as i32);
    let elem_ptr = env.ctx.get_elem_ptr(ptr, index);
    env.ctx.add_inst(elem_ptr);

    if iter.is_empty() {
      generate_local_zero_vals(env, base_ty.clone(), elem_ptr);
      continue;
    }

    match iter.peek().unwrap().as_initvals() {
      Some(_) => {
        // 如果下一项是嵌套大括号，作为该元素的完整初始化器消费它
        let nested = iter.next().unwrap(); // consume
        // nested 类型是 &T
        generate_local_init_vals(env, base_ty.clone(), elem_ptr, nested);
      }
      None => {
        // 下一项不是大括号，则应为表达式
        match base_ty.kind() {
          TypeKind::Int32 => {
            let vref = iter.next().unwrap(); // consume an Exp
            if let Some(exp) = vref.as_exp() {
              let val = exp.generate(env);
              let store_inst = env.ctx.local_store(val, elem_ptr);
              env.ctx.add_inst(store_inst);
            } else {
              panic!("Expected expression to initialize scalar element");
            }
          }
          TypeKind::Array(inner_base, inner_len) => {
            // 递归填充子数组；注意递归内部会继续从 iter 中消费连续的表达式或嵌套列表
            fill_array_from_iter(env, inner_base.clone(), *inner_len as i32, elem_ptr, iter);
          }
          _ => panic!("Unsupported nested type in fill_array_from_iter_generic: {:?}", base_ty),
        }
      }
    }
  }
}

/// 递归：根据 iter 消费项来填充当前层 len 个元素，返回每个元素对应的 Value 向量
fn fill_global_array_from_iter<'a, T>(
  env: &mut Environment,
  base_ty: Type,
  len: i32,
  iter: &mut InitIter<'a, T>,
) -> Vec<Value>
where
  T: InitLike + Clone,
  T::Expr: ExprGenerator,
{
  let mut elems: Vec<Value> = Vec::with_capacity(len as usize);

  for _i in 0..len {
    // 没有更多显式初始化 -> 直接生成零初始化（包括递归零）
    if iter.is_empty() {
      elems.push(env.ctx.global_zero_init(base_ty.clone()));
      continue;
    }

    // 如果下一项是一个嵌套大括号，则把它当作完整的元素初始化器（consume）
    if iter.peek().and_then(|x| x.as_initvals()).is_some() {
      let nested = iter.next().unwrap(); // consume nested initvals
      let v = generate_global_init_vals(env, base_ty.clone(), nested);
      elems.push(v);
      continue;
    }

    // 否则下一项应是表达式（扁平序列）
    match base_ty.kind() {
      TypeKind::Int32 => {
        let vref = iter.next().unwrap(); // consume one expression
        if let Some(exp) = vref.as_exp() {
          let val = exp.evaluate(env);
          elems.push(env.ctx.global_integer(val));
        } else {
          panic!("Expected expression to initialize scalar element");
        }
      }

      TypeKind::Array(inner_base, inner_len) => {
        // 对于子数组类型：递归从同一个 iter 中消费足够数量的项来填满该子数组
        let sub_elems = fill_global_array_from_iter(env, inner_base.clone(), *inner_len as i32, iter);
        let agg = env.ctx.global_aggregate(sub_elems);
        elems.push(agg);
      }

      other => panic!("Unsupported nested type in global initializer: {}", other),
    }
  }

  elems
}

pub fn generate_global_init_vals<T>(
  env: &mut Environment,
  ty: Type,
  initval: &T,
) -> Value
where
  T: InitLike + Clone,         // Clone 用于将单个 Exp 当作单元素列表的临时情况
  T::Expr: ExprGenerator,
{
  // 情形：直接是一个表达式
  if let Some(exp) = initval.as_exp() {
    match ty.kind() {
      TypeKind::Int32 => {
        let v = exp.evaluate(env);
        return env.ctx.global_integer(v);
      }
      TypeKind::Array(_, _) => {
        // 把单个表达式当作 "{ expr }"
        let tmp: Vec<T> = vec![initval.clone()];
        let mut iter = InitIter::new(tmp.as_slice());
        if let TypeKind::Array(base_ty, len) = ty.kind() {
          let elems = fill_global_array_from_iter(env, base_ty.clone(), *len as i32, &mut iter);
          return env.ctx.global_aggregate(elems);
        } else {
          unreachable!();
        }
      }
      _ => panic!("Unsupported type for global initialization: {:?}", ty),
    }
  }

  // 情形：InitVals（大括号）
  if let Some(vals) = initval.as_initvals() {
    let mut iter = InitIter::new(vals);
    match ty.kind() {
      TypeKind::Int32 => {
        // 标量用大括号：若为空 -> zero；否则用第一个表达式的值
        if iter.is_empty() {
          return env.ctx.global_zero_init(ty);
        } else {
          let vref = iter.next().unwrap();
          if let Some(exp) = vref.as_exp() {
            let v = exp.evaluate(env);
            return env.ctx.global_integer(v);
          } else {
            panic!("Invalid initializer for scalar: expected expression inside braces");
          }
        }
      }
      TypeKind::Array(base_ty, len) => {
        let elems = fill_global_array_from_iter(env, base_ty.clone(), *len as i32, &mut iter);
        // 额外剩余项：你可以选择 panic! 报错或者忽略（这里忽略）
        // if !iter.is_empty() { panic!("Too many initializers for array") }
        return env.ctx.global_aggregate(elems);
      }
      _ => panic!("Unsupported type for global initialization: {:?}", ty),
    }
  }

  panic!("Unsupported initval variant for global init");
}

pub fn generate_local_zero_vals(env: &mut Environment, ty: Type, ptr: Value) {
  match ty.kind() {
    TypeKind::Int32 => {
      let zero = env.ctx.local_integer(0);
      let store_inst = env.ctx.local_store(zero, ptr);
      env.ctx.add_inst(store_inst);
    }
    TypeKind::Array(base_ty, len) => {
      for i in 0..*len {
        let index = env.ctx.local_integer(i as i32);
        let elem_ptr = env.ctx.get_elem_ptr(ptr, index);
        env.ctx.add_inst(elem_ptr);
        generate_local_zero_vals(env, base_ty.clone(), elem_ptr);
      }
    }
    _ => panic!("Unsupported type for local zero initialization: {:?}", ty)
  }
}

pub fn generate_local_init_vals<T>(env: &mut Environment, ty: Type,  ptr: Value, initval: &T)
where 
  T: InitLike + Clone,
  T::Expr: ExprGenerator,
{
  if let Some(exp) = initval.as_exp() {
    match ty.kind() {
      TypeKind::Int32 => {
        let val = exp.generate(env);
        let store_inst = env.ctx.local_store(val, ptr);
        env.ctx.add_inst(store_inst);
      }
      TypeKind::Array(base_ty, len) => {
        let single_as_vec = vec![initval.clone()]; // 需要 InitVal 可克隆。如果没有 Clone，可构造临时 InitVal::InitVals(vec![initval.clone()])
        let mut iter = InitIter::new(single_as_vec.as_slice());
        fill_array_from_iter(env, base_ty.clone(), *len as i32, ptr, &mut iter);
      }
      _ => panic!("Unsupported type for local initialization: {:?}", ty),
    }
  } else if let Some(vals) = initval.as_initvals() {
    match ty.kind() {
      TypeKind::Int32 => {
        panic!("Expected expression for local variable initialization, got InitVals");
      }
      TypeKind::Array(base_ty, len) => {
        let mut iter = InitIter::new(vals);
        fill_array_from_iter(env, base_ty.clone(), *len as i32, ptr, &mut iter);
        if !iter.is_empty() {
          panic!("Too many initializers for array of length: expected {}, got {}", len, vals.len());
        }
      }
      _ => panic!("Unsupported type for local array initialization: {:?}", ty),
    }
  } else {
    panic!("Unsupported initval variant");
  }
}

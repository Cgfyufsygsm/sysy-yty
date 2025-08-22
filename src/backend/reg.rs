use once_cell::sync::Lazy;
use std::{fmt, sync::RwLock};

pub struct RegPool {
  free: Vec<&'static str>,
}

impl RegPool {
  fn new() -> Self {
    RegPool {
      free: vec!["t0", "t1", "t2", "t3", "t4", "t5", "t6"]
    }
  }

  fn alloc(&mut self) -> Option<&'static str> {
    self.free.pop()
  }

  fn release(&mut self, reg: &'static str) {
    if !self.free.contains(&reg) {
      self.free.push(reg);
    }
  }
}

/// 全局寄存器池
pub static REG_POOL: Lazy<RwLock<RegPool>> = Lazy::new(|| {
  RwLock::new(RegPool::new())
});

pub struct RegGuard {
  reg: Option<&'static str>,
}

impl RegGuard {
  pub fn new() -> Option<Self> {
    let reg = REG_POOL.write().unwrap().alloc();
    reg.map(|r| Self { reg: Some(r) })
  }

  pub fn name(&self) -> &'static str {
    self.reg.expect("RegGuard holds no register")
  }
}

impl Drop for RegGuard {
  fn drop(&mut self) {
    if let Some(r) = self.reg.take() {
      REG_POOL.write().unwrap().release(r);
    }
  }
}

impl fmt::Display for RegGuard {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.name())
  }
}
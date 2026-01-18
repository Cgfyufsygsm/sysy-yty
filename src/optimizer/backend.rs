pub fn optimize(asm: &mut String) {
  let mut changed = true;
  while changed {
    changed = false;
    if peephole_sw_lw(asm) {
      changed = true;
    }
    if peephole_jump_to_next_label(asm) {
      changed = true;
    }
    if peephole_remove_redundant_mv(asm) {
      changed = true;
    }
  }
}

#[derive(Clone, Debug)]
struct MemInst {
  reg: String,
  offset: i32,
  base: String,
  comment: String,
}

fn peephole_sw_lw(asm: &mut String) -> bool {
  let lines: Vec<&str> = asm.lines().collect();
  let had_trailing_newline = asm.ends_with('\n');
  let mut out: Vec<String> = Vec::with_capacity(lines.len());
  let mut changed = false;

  let mut i = 0usize;
  while i < lines.len() {
    let line = lines[i];
    let sw = parse_mem_inst(line, "sw");
    if let Some(sw_inst) = sw {
      let mut j = i + 1;
      let mut skipped: Vec<&str> = Vec::new();
      while j < lines.len() && is_blank_or_comment(lines[j]) {
        skipped.push(lines[j]);
        j += 1;
      }
      if j < lines.len() {
        let next_line = lines[j];
        if let Some(lw_inst) = parse_mem_inst(next_line, "lw") {
          if sw_inst.base == "sp"
            && lw_inst.base == "sp"
            && sw_inst.offset == lw_inst.offset
          {
            out.push(line.to_string());
            for s in skipped {
              out.push(s.to_string());
            }
            if sw_inst.reg != lw_inst.reg {
              let mut mv_line = format!("  mv    {}, {}", lw_inst.reg, sw_inst.reg);
              if !lw_inst.comment.is_empty() {
                mv_line.push(' ');
                mv_line.push_str(&lw_inst.comment);
              }
              out.push(mv_line);
            }
            changed = true;
            i = j + 1;
            continue;
          }
        }
      }
    }

    out.push(line.to_string());
    i += 1;
  }

  let mut rebuilt = out.join("\n");
  if had_trailing_newline {
    rebuilt.push('\n');
  }
  *asm = rebuilt;
  changed
}

fn peephole_jump_to_next_label(asm: &mut String) -> bool {
  let lines: Vec<&str> = asm.lines().collect();
  let had_trailing_newline = asm.ends_with('\n');
  let mut out: Vec<String> = Vec::with_capacity(lines.len());
  let mut changed = false;

  let mut i = 0usize;
  while i < lines.len() {
    let line = lines[i];
    if let Some(target) = parse_jump(line) {
      let mut j = i + 1;
      let mut skipped: Vec<&str> = Vec::new();
      while j < lines.len() && is_blank_or_comment(lines[j]) {
        skipped.push(lines[j]);
        j += 1;
      }
      if j < lines.len() && is_label(lines[j], &target) {
        for s in skipped {
          out.push(s.to_string());
        }
        changed = true;
        i = j;
        continue;
      }
    }
    out.push(line.to_string());
    i += 1;
  }

  let mut rebuilt = out.join("\n");
  if had_trailing_newline {
    rebuilt.push('\n');
  }
  *asm = rebuilt;
  changed
}

fn peephole_remove_redundant_mv(asm: &mut String) -> bool {
  let lines: Vec<&str> = asm.lines().collect();
  let had_trailing_newline = asm.ends_with('\n');
  let mut out: Vec<String> = Vec::with_capacity(lines.len());
  let mut changed = false;

  for line in lines {
    if let Some((dst, src, comment)) = parse_mv(line) {
      if dst == src {
        if !comment.is_empty() {
          out.push(comment.to_string());
        }
        changed = true;
        continue;
      }
    }
    out.push(line.to_string());
  }

  let mut rebuilt = out.join("\n");
  if had_trailing_newline {
    rebuilt.push('\n');
  }
  *asm = rebuilt;
  changed
}

fn parse_mem_inst(line: &str, op: &str) -> Option<MemInst> {
  let trimmed = line.trim();
  let (code, comment) = split_comment(trimmed);
  let mut parts = code.split_whitespace();
  let op_token = parts.next()?;
  if op_token != op {
    return None;
  }
  let rest = code[op_token.len()..].trim();
  let mut args = rest.split(',');
  let reg = args.next()?.trim().to_string();
  let mem = args.next()?.trim();
  if args.next().is_some() {
    return None;
  }
  let (offset, base) = parse_mem_operand(mem)?;
  Some(MemInst {
    reg,
    offset,
    base,
    comment: comment.to_string(),
  })
}

fn parse_mem_operand(operand: &str) -> Option<(i32, String)> {
  let l = operand.find('(')?;
  let r = operand.find(')')?;
  if r <= l {
    return None;
  }
  let offset_str = operand[..l].trim();
  let base = operand[l + 1..r].trim().to_string();
  let offset = offset_str.parse::<i32>().ok()?;
  Some((offset, base))
}

fn split_comment(line: &str) -> (&str, &str) {
  if let Some(idx) = line.find('#') {
    let (code, comment) = line.split_at(idx);
    (code.trim_end(), comment.trim_start())
  } else {
    (line, "")
  }
}

fn is_blank_or_comment(line: &str) -> bool {
  let (code, _comment) = split_comment(line);
  code.trim().is_empty()
}

fn is_label(line: &str, target: &str) -> bool {
  let (code, _comment) = split_comment(line);
  let trimmed = code.trim();
  if !trimmed.ends_with(':') {
    return false;
  }
  let name = trimmed.trim_end_matches(':').trim();
  name == target
}

fn parse_jump(line: &str) -> Option<String> {
  let (code, _comment) = split_comment(line);
  let mut parts = code.split_whitespace();
  let op = parts.next()?;
  if op != "j" {
    return None;
  }
  let target = parts.next()?.trim();
  if parts.next().is_some() {
    return None;
  }
  if target.is_empty() {
    return None;
  }
  Some(target.to_string())
}

fn parse_mv(line: &str) -> Option<(String, String, String)> {
  let trimmed = line.trim();
  let (code, comment) = split_comment(trimmed);
  let mut parts = code.split_whitespace();
  let op = parts.next()?;
  if op != "mv" {
    return None;
  }
  let rest = code[op.len()..].trim();
  let mut args = rest.split(',');
  let dst = args.next()?.trim().to_string();
  let src = args.next()?.trim().to_string();
  if args.next().is_some() {
    return None;
  }
  Some((dst, src, comment.to_string()))
}

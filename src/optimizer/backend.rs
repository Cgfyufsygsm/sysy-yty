pub fn optimize(asm: &mut String) {
  peephole_sw_lw(asm);
}

#[derive(Clone, Debug)]
struct MemInst {
  reg: String,
  offset: i32,
  base: String,
  comment: String,
}

fn peephole_sw_lw(asm: &mut String) {
  let mut lines: Vec<&str> = asm.lines().collect();
  let had_trailing_newline = asm.ends_with('\n');
  let mut out: Vec<String> = Vec::with_capacity(lines.len());

  let mut i = 0usize;
  while i < lines.len() {
    let line = lines[i];
    let sw = parse_mem_inst(line, "sw");
    if let Some(sw_inst) = sw {
      if i + 1 < lines.len() {
        let next_line = lines[i + 1];
        if let Some(lw_inst) = parse_mem_inst(next_line, "lw") {
          if sw_inst.base == "sp"
            && lw_inst.base == "sp"
            && sw_inst.offset == lw_inst.offset
          {
            out.push(line.to_string());
            if sw_inst.reg != lw_inst.reg {
              let mut mv_line = format!("  mv    {}, {}", lw_inst.reg, sw_inst.reg);
              if !lw_inst.comment.is_empty() {
                mv_line.push(' ');
                mv_line.push_str(&lw_inst.comment);
              }
              out.push(mv_line);
            }
            i += 2;
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

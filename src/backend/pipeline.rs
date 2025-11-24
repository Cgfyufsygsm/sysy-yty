use std::fmt::Write;

/// Options controlling backend optimization passes.
///
/// They are deliberately kept simple so new passes can be toggled on without
/// reworking the code generation API.
#[derive(Debug, Clone, Default)]
pub struct BackendOptions {
    /// Enable peephole optimization over the generated assembly.
    pub enable_peephole: bool,
    /// Enable graph-coloring based register allocation.
    pub enable_graph_coloring: bool,
}

/// Lightweight assembly representation split into data and text sections.
///
/// A vector based representation makes it easier to insert or rewrite
/// instructions in future optimization passes without reparsing raw strings.
#[derive(Debug, Clone, Default)]
pub struct AsmProgram {
    pub data: Vec<String>,
    pub text: Vec<String>,
}

impl AsmProgram {
    pub fn new() -> Self {
        Self {
            data: vec![],
            text: vec![],
        }
    }

    pub fn from_sections(data: String, text: String) -> Self {
        let mut program = Self::new();
        program.extend_data(&data);
        program.extend_text(&text);
        program
    }

    pub fn extend_data(&mut self, content: &str) {
        self.data
            .extend(content.lines().map(|line| line.to_string()));
    }

    pub fn extend_text(&mut self, content: &str) {
        self.text
            .extend(content.lines().map(|line| line.to_string()));
    }

    pub fn render(&self) -> String {
        let mut buf = String::new();
        for line in &self.data {
            writeln!(buf, "{}", line).unwrap();
        }
        if !self.text.is_empty() && !self.data.is_empty() {
            buf.push('\n');
        }
        for line in &self.text {
            writeln!(buf, "{}", line).unwrap();
        }
        buf
    }
}

/// Trait for assembly level optimization passes.
pub trait AsmPass {
    fn name(&self) -> &str;
    fn run(&mut self, program: &mut AsmProgram);
}

/// Perform simple peephole rewrites to shrink and simplify the instruction
/// stream.
#[derive(Default)]
pub struct PeepholePass;

impl PeepholePass {
    pub fn new() -> Self {
        Self
    }

    fn is_trivial_line(line: &str) -> bool {
        let trimmed = line.trim();
        trimmed.is_empty() || trimmed.ends_with(':') || trimmed.starts_with('.')
    }

    fn simplify_single(line: &str) -> Option<String> {
        let trimmed = line.trim();
        if Self::is_trivial_line(trimmed) {
            return Some(line.to_string());
        }

        // mv rd, rd => remove
        if let Some(rest) = trimmed.strip_prefix("mv") {
            let mut parts = rest.split(',').map(|s| s.trim());
            if let (Some(rd), Some(rs)) = (parts.next(), parts.next()) {
                if rd == rs {
                    return None;
                }
            }
        }

        // addi rd, rs, 0 => mv rd, rs
        if let Some(rest) = trimmed.strip_prefix("addi") {
            let mut parts = rest.split(',').map(|s| s.trim());
            if let (Some(rd), Some(rs), Some(imm)) = (parts.next(), parts.next(), parts.next()) {
                if imm == "0" {
                    return Some(format!("  mv    {}, {}", rd, rs));
                }
            }
        }

        // li rd, 0 => mv rd, zero
        if let Some(rest) = trimmed.strip_prefix("li") {
            let mut parts = rest.split(',').map(|s| s.trim());
            if let (Some(rd), Some(imm)) = (parts.next(), parts.next()) {
                if imm == "0" {
                    return Some(format!("  mv    {}, zero", rd));
                }
            }
        }

        Some(line.to_string())
    }
}

impl AsmPass for PeepholePass {
    fn name(&self) -> &str {
        "peephole"
    }

    fn run(&mut self, program: &mut AsmProgram) {
        let mut optimized = Vec::with_capacity(program.text.len());

        let mut i = 0;
        while i < program.text.len() {
            let line = &program.text[i];
            if let Some(next) = program.text.get(i + 1) {
                // Combine a leading immediate load followed by a pure move.
                let trimmed = line.trim();
                let next_trimmed = next.trim();
                if trimmed.starts_with("li") && next_trimmed.starts_with("mv") {
                    let mut parts_li = trimmed[2..].split(',').map(|s| s.trim());
                    let mut parts_mv = next_trimmed[2..].split(',').map(|s| s.trim());
                    if let (Some(li_rd), Some(imm), Some(mv_rd), Some(mv_rs)) = (
                        parts_li.next(),
                        parts_li.next(),
                        parts_mv.next(),
                        parts_mv.next(),
                    ) {
                        if li_rd == mv_rs {
                            optimized.push(format!("  li    {}, {}", mv_rd, imm));
                            i += 2;
                            continue;
                        }
                    }
                }
            }

            if let Some(simplified) = Self::simplify_single(line) {
                optimized.push(simplified);
            }
            i += 1;
        }

        program.text = optimized;
    }
}

/// Apply a very small graph-coloring allocator over temporary registers (t0-t6)
/// using a backwards liveness walk to build the interference graph.
#[derive(Default)]
pub struct GraphColoringRegAlloc;

impl GraphColoringRegAlloc {
    pub fn new() -> Self {
        Self
    }

    fn is_colorable(reg: &str) -> bool {
        reg.starts_with('t')
    }

    fn parse_operand_regs(op: &str) -> Vec<String> {
        if let Some(start) = op.find('(') {
            if let Some(end) = op[start + 1..].find(')') {
                let reg = &op[start + 1..start + 1 + end];
                if Self::is_register(reg) {
                    return vec![reg.to_string()];
                }
            }
        }
        if Self::is_register(op) {
            vec![op.to_string()]
        } else {
            vec![]
        }
    }

    fn is_register(token: &str) -> bool {
        matches!(
            token,
            "zero"
                | "ra"
                | "sp"
                | "gp"
                | "tp"
                | "t0"
                | "t1"
                | "t2"
                | "t3"
                | "t4"
                | "t5"
                | "t6"
                | "s0"
                | "s1"
                | "s2"
                | "s3"
                | "s4"
                | "s5"
                | "s6"
                | "s7"
                | "s8"
                | "s9"
                | "s10"
                | "s11"
                | "a0"
                | "a1"
                | "a2"
                | "a3"
                | "a4"
                | "a5"
                | "a6"
                | "a7"
        )
    }

    fn parse_line_defs_uses(line: &str) -> (Option<String>, Vec<String>) {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('.') || trimmed.ends_with(':') {
            return (None, vec![]);
        }

        let mut iter = trimmed.split_whitespace();
        let op = match iter.next() {
            Some(op) => op,
            None => return (None, vec![]),
        };
        let operand_str = iter.collect::<Vec<_>>().join(" ");
        let operands: Vec<&str> = operand_str
            .split(',')
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .collect();

        match op {
            "add" | "sub" | "mul" | "div" | "rem" | "xor" | "or" | "and" | "slt" | "sltu"
            | "srli" | "slli" | "addi" => {
                let def = operands.get(0).map(|s| s.to_string());
                let uses = operands
                    .iter()
                    .skip(1)
                    .flat_map(|op| Self::parse_operand_regs(op))
                    .collect();
                (def, uses)
            }
            "lw" | "li" | "mv" | "neg" => {
                let def = operands.get(0).map(|s| s.to_string());
                let uses = operands
                    .iter()
                    .skip(1)
                    .flat_map(|op| Self::parse_operand_regs(op))
                    .collect();
                (def, uses)
            }
            "sw" => {
                let uses = operands
                    .iter()
                    .flat_map(|op| Self::parse_operand_regs(op))
                    .collect();
                (None, uses)
            }
            "beq" | "bne" | "blt" | "bge" | "bltu" | "bgeu" => {
                let uses = operands
                    .iter()
                    .take(2)
                    .flat_map(|op| Self::parse_operand_regs(op))
                    .collect();
                (None, uses)
            }
            _ => (None, vec![]),
        }
    }

    fn build_interference(
        lines: &[String],
    ) -> std::collections::HashMap<String, std::collections::HashSet<String>> {
        use std::collections::{HashMap, HashSet};

        let mut graph: HashMap<String, HashSet<String>> = HashMap::new();
        let mut live: HashSet<String> = HashSet::new();

        for line in lines.iter().rev() {
            let (def, uses) = Self::parse_line_defs_uses(line);
            let def = def.filter(|d| Self::is_colorable(d));
            let uses: Vec<String> = uses.into_iter().filter(|u| Self::is_colorable(u)).collect();

            // Add uses to liveness before processing def
            for u in &uses {
                live.insert(u.clone());
            }

            if let Some(d) = def {
                let live_now: Vec<String> = live.iter().cloned().collect();
                for other in live_now {
                    if other != d {
                        graph.entry(d.clone()).or_default().insert(other.clone());
                        graph.entry(other).or_default().insert(d.clone());
                    }
                }
                live.remove(&d);
                live.insert(d);
            }
        }

        graph
    }

    fn color_graph(
        &self,
        graph: &std::collections::HashMap<String, std::collections::HashSet<String>>,
    ) -> std::collections::HashMap<String, String> {
        use std::collections::HashMap;

        let mut mapping: HashMap<String, String> = HashMap::new();
        let mut nodes: Vec<_> = graph.keys().cloned().collect();
        nodes.sort_by_key(|n| std::cmp::Reverse(graph.get(n).map(|s| s.len()).unwrap_or(0)));

        let colors = ["t0", "t1", "t2", "t3", "t4", "t5", "t6"];

        for node in nodes {
            let mut used = std::collections::HashSet::new();
            if let Some(neighbors) = graph.get(&node) {
                for n in neighbors {
                    if let Some(color) = mapping.get(n) {
                        used.insert(color.clone());
                    }
                }
            }
            let assigned = colors
                .iter()
                .find(|c| !used.contains(&c.to_string()))
                .map(|c| c.to_string())
                .unwrap_or_else(|| node.clone());
            mapping.insert(node.clone(), assigned);
        }

        mapping
    }

    fn rewrite_line(line: &str, mapping: &std::collections::HashMap<String, String>) -> String {
        let mut result = String::new();
        let mut token = String::new();

        let flush_token = |tok: &mut String, out: &mut String| {
            if tok.is_empty() {
                return;
            }
            if let Some(rep) = mapping.get(tok) {
                out.push_str(rep);
            } else {
                out.push_str(tok);
            }
            tok.clear();
        };

        for ch in line.chars() {
            if ch.is_alphanumeric() {
                token.push(ch);
            } else {
                flush_token(&mut token, &mut result);
                result.push(ch);
            }
        }
        flush_token(&mut token, &mut result);
        result
    }
}

impl AsmPass for GraphColoringRegAlloc {
    fn name(&self) -> &str {
        "graph-coloring-regalloc"
    }

    fn run(&mut self, program: &mut AsmProgram) {
        if program.text.is_empty() {
            return;
        }

        // Run per function (identified by .globl markers) to avoid cross-function
        // interference.
        let mut start = 0;
        let mut new_text = Vec::with_capacity(program.text.len());
        for (idx, line) in program.text.iter().enumerate() {
            if line.trim_start().starts_with(".globl") && idx != start {
                let chunk = &program.text[start..idx];
                let graph = Self::build_interference(chunk);
                let mapping = self.color_graph(&graph);
                new_text.extend(chunk.iter().map(|l| Self::rewrite_line(l, &mapping)));
                start = idx;
            }
        }
        let chunk = &program.text[start..];
        let graph = Self::build_interference(chunk);
        let mapping = self.color_graph(&graph);
        new_text.extend(chunk.iter().map(|l| Self::rewrite_line(l, &mapping)));

        program.text = new_text;
    }
}

/// Pass manager that runs registered assembly passes sequentially.
#[derive(Default)]
pub struct AsmPassManager {
    passes: Vec<Box<dyn AsmPass>>,
}

impl AsmPassManager {
    pub fn new() -> Self {
        Self { passes: vec![] }
    }

    pub fn add_pass<P: AsmPass + 'static>(&mut self, pass: P) {
        self.passes.push(Box::new(pass));
    }

    pub fn build_from_options(options: &BackendOptions) -> Self {
        let mut manager = Self::new();
        if options.enable_peephole {
            manager.add_pass(PeepholePass::new());
        }
        if options.enable_graph_coloring {
            manager.add_pass(GraphColoringRegAlloc::new());
        }
        manager
    }

    pub fn run_passes(&mut self, program: &mut AsmProgram) {
        for pass in self.passes.iter_mut() {
            pass.run(program);
        }
    }
}

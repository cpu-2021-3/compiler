use std::{mem::swap, convert::TryInto, iter::Inspect, ops::Index};

use fnv::{FnvHashSet, FnvHashMap};

use crate::{riscv::asm::{Instruction, IOp}, knormal::{UnaryOp, BinaryOp, CondOp}, span::{Spanned, Span}, id::generate_id};

use super::{specific::{Function, Expr, RawExpr, RawInstr, Instr}, asm::{Program, Register, ROp, self, BOp, AREG_NUM}};

#[derive(Clone, Debug)]
struct Graph {
    verts: FnvHashSet<String>,
    edges: FnvHashSet<(String, String)>
}

// interference graph
impl Graph {
    // 新しいグラフを生成
    fn new() -> Self {
        Graph {verts: FnvHashSet::default(), edges: FnvHashSet::default()}
    }
    // u と v を辺でつなぐ
    fn add(&mut self, mut u: String, mut v: String) {
        self.verts.insert(u.clone());
        self.verts.insert(v.clone());
        if u == v {
            return;
        }
        if u > v {
            swap(&mut u, &mut v);
        }
        self.edges.insert((u, v));
    }
    // s 中のすべての異なる 2 頂点同士を辺でつなぐ
    fn clique(&mut self, s: &FnvHashSet<String>) {
        for x in s {
            for y in s {
                self.add(x.clone(), y.clone());
            }
        }
    }
    // 別のグラフと合体する
    fn merge(&mut self, o: Graph) {
        for vert in o.verts {
            self.verts.insert(vert);
        }
        for edge in o.edges {
            self.edges.insert(edge);
        }
    }
    // グラフの隣接リストを得る
    fn edge_list(&self) -> FnvHashMap<String, FnvHashSet<String>> {
        let mut res: FnvHashMap<String, FnvHashSet<String>> = FnvHashMap::default();
        for v in &self.verts {
            res.insert(v.clone(), FnvHashSet::default());
        }
        for (u, v) in &self.edges {
            res.get_mut(u).unwrap().insert(v.clone());
            res.get_mut(v).unwrap().insert(u.clone());
        }
        res
    }
}


// 活性解析の結果を格納するデータ構造
#[derive(Clone)]
struct Data {
    // ある命令以前から生きているレジスタの集合
    pub prop: FnvHashSet<String>, 
    // ある命令以後にある依存関係のグラフ
    pub graph: Graph
}

impl Data {
    // prop 中のすべての 2 頂点を辺で結んだうえで Data を返す
    fn clique_prop(prop: FnvHashSet<String>, mut graph: Graph) -> Self {
        graph.clique(&prop);
        Self {prop, graph}
    }

    fn new() -> Self {
        Self {prop: FnvHashSet::default(), graph: Graph::new() }
    }
}

// ret <- instr 文の後ろからの Data をもとに、ret <- instr 文の前の Data を計算する
fn update_prop(prop: &mut FnvHashSet<String>, ret: &String, instr: &RawInstr) {
    prop.remove(ret);
    match instr {
        RawInstr::Unit | RawInstr::Int(_) | RawInstr::Float(_) | RawInstr::DataTag(_) => {},
        RawInstr::Var(id) => {
            prop.insert(id.clone());
        },
        RawInstr::UnOp { op: _, id } => {
            prop.insert(id.clone());
        },
        RawInstr::BiOp { id_left, op: _, id_right } => {
            prop.insert(id_left.clone());
            prop.insert(id_right.clone());
        },
        RawInstr::BiImm { id_left, op: _, imm: _ } => {
            prop.insert(id_left.clone());
        },
        RawInstr::FCondOp { id_left, op: _, id_right } => {
            prop.insert(id_left.clone());
            prop.insert(id_right.clone());
        },
        RawInstr::If { id_left, op, id_right, exp_then, exp_else } => {
            panic!("internal compiler error");
        },
        RawInstr::IfZero { id, exp_then, exp_else } => {
            panic!("internal compiler error");
        },
        RawInstr::CallCls { cls, args } => {
            prop.insert(cls.clone());
            args.into_iter().for_each(|arg|{prop.insert(arg.clone());});
        },
        RawInstr::CallDir { tag: _, args } => {
            args.into_iter().for_each(|arg|{prop.insert(arg.clone());});
        },
        RawInstr::NewTuple(elms) => {
            elms.into_iter().for_each(|elm|{prop.insert(elm.clone());});
        },
        RawInstr::NewClosure { tag: _, free_vars } => {
            free_vars.into_iter().for_each(|free_var|{prop.insert(free_var.clone());});
        },
        RawInstr::Read { address, offset: _ } => {
            prop.insert(address.clone());
        },
        RawInstr::Write { address, offset: _, value } => {
            prop.insert(address.clone());
            prop.insert(value.clone());
        },
    };
}

// 式 expr 以前の Data を計算する
// ret: expr の結果を格納する変数名 
// suc: expr より後から来る Data
fn liveness_info (expr: &Expr, ret: String, suc: &Data) -> Data {
    match &expr.item {
        RawExpr::LetIn { id, instr_id, instr_suc } => {
            match &instr_id.item {
                RawInstr::If { id_left, op: _, id_right, exp_then, exp_else } => {
                    let data = liveness_info(instr_suc, ret, suc);
                    let then_data = liveness_info(exp_then, id.clone(), &data);
                    let else_data = liveness_info(exp_else, id.clone(), &data);
                    let mut prop = then_data.prop;
                    prop.extend(else_data.prop);
                    prop.insert(id_left.clone());
                    prop.insert(id_right.clone());
                    let mut graph = then_data.graph;
                    graph.merge(else_data.graph);
                    Data::clique_prop(prop, graph)
                },
                RawInstr::IfZero { id: id_zero, exp_then, exp_else } => {
                    let data = liveness_info(instr_suc, ret, suc);
                    let then_data = liveness_info(exp_then, id.clone(), &data);
                    let else_data = liveness_info(exp_else, id.clone(), &data);
                    let mut prop = then_data.prop;
                    prop.extend(else_data.prop);
                    prop.insert(id_zero.clone());
                    let mut graph = then_data.graph;
                    graph.merge(else_data.graph);
                    Data::clique_prop(prop, graph)
                },
                otherwise => {
                    let data = liveness_info(instr_suc, ret, suc);
                    let mut prop = data.prop;
                    update_prop(&mut prop, id, otherwise);
                    let graph = data.graph;
                    Data::clique_prop(prop, graph)
                }
            }
        },
        RawExpr::Is(instr) => {
            match &instr.item {
                RawInstr::If { id_left, op: _, id_right, exp_then, exp_else } => {
                    let then_data = liveness_info(exp_then, ret.clone(), suc);
                    let else_data = liveness_info(exp_else, ret, suc);
                    let mut prop = then_data.prop;
                    prop.extend(else_data.prop);
                    prop.insert(id_left.clone());
                    prop.insert(id_right.clone());
                    let mut graph = then_data.graph;
                    graph.merge(else_data.graph);
                    Data::clique_prop(prop, graph)
                },
                RawInstr::IfZero { id, exp_then, exp_else} => {
                    let then_data = liveness_info(exp_then, ret.clone(), suc);
                    let else_data = liveness_info(exp_else, ret, suc);
                    let mut prop = then_data.prop;
                    prop.extend(else_data.prop);
                    prop.insert(id.clone());
                    let mut graph = then_data.graph;
                    graph.merge(else_data.graph);
                    Data::clique_prop(prop, graph)
                },
                otherwise => {
                    let data = suc.clone();
                    let mut prop = data.prop;
                    update_prop(&mut prop, &ret, otherwise);
                    let graph = data.graph;
                    Data::clique_prop(prop, graph)
                }
            }
        },
    }
}

// expr 上の interference graph を構築する
fn build_graph(expr: &Expr) -> Graph {
    let data = liveness_info(expr, "dummy!".to_string(), &Data::new());
    data.graph
}

// graph 上の各頂点にレジスタ番号 (0 以上 limit 未満) を割り当てる
fn map_register(graph: Graph, limit: usize) -> FnvHashMap<String, usize> {
    let mut color_list: FnvHashMap<String, usize> = FnvHashMap::default();
    let mut edge_list = graph.edge_list();
    let mut vertice_stack = vec![];
    // 頂点のスタックを構築
    loop {
        let mut vertice = None;
        if edge_list.is_empty() {
            break;
        }
        for (v, edges) in &edge_list {
            if edges.len() <= limit {
                vertice = Some(v.clone());
                break;
            }
        }
        if vertice == None {
            for (v, _edges) in &edge_list {
                vertice = Some(v.clone());
                break;
            }
        }
        let vertice = vertice.unwrap();
        for other in edge_list.remove(&vertice).unwrap() {
            edge_list.get_mut(&other).unwrap().remove(&vertice);
        }
        vertice_stack.push(vertice);
    }
    // スタックの上から頂点を色付け (番号の割り当て) をしていく
    let edge_list = graph.edge_list();
    loop {
        match vertice_stack.pop() {
            Some(vertice) => {
                let mut used_colors: FnvHashSet<usize> = FnvHashSet::default();
                for other in edge_list.get(&vertice).unwrap() {
                    if let Some(color) = color_list.get(other) {
                        used_colors.insert(*color);
                    }
                }
                let mut new_color = 0usize;
                while used_colors.contains(&new_color) {
                    new_color += 1;
                }
                if new_color >= limit.try_into().unwrap() {
                    log::error!("regalloc error: too few registers for allocation");
                    panic!("internal compiler error");
                }
                color_list.insert(vertice, new_color);
            },
            None => {
                break;
            }
        }
    }
    color_list
}

fn reg_of(id: String, color_list: &FnvHashMap<String, usize>) -> Register {
    if let Some(num) = color_list.get(&id) {
        Register::S(*num)
    }
    else {
        Register::X
    }
}

fn push_instructions(instr: Instr, rd: Register, program: &mut Program, is_tail: bool, color_list: &FnvHashMap<String, usize>) {
    let instr_span = instr.span;
    fn push(program: &mut Program, instruction: Instruction, instr_span: Span) {
        program.push(Spanned::new(instruction, instr_span))
    }
    match instr.item {
        RawInstr::Unit => {},
        RawInstr::Int(i) => {
            push(program, Instruction::SetImm {
                rd,
                imm: i,
            }, instr_span)
        },
        RawInstr::Float(f) => {
            push(program, Instruction::SetImm {
                rd,
                imm: f.to_bits() as i32,
            }, instr_span)
        },
        RawInstr::Var(id) => push(program, Instruction::Move {
            rd,
            rs: reg_of(id, color_list)
        }, instr_span),
        RawInstr::DataTag(tag) => push(program, Instruction::LoadTag {
            rd,
            tag,
        }, instr_span),
        RawInstr::UnOp { op, id } => {
            let instruction = match op {
                UnaryOp::Neg => {
                    Instruction::ROp {
                        rd,
                        op: ROp::Sub,
                        rs1: Register::Zero,
                        rs2: reg_of(id, color_list),
                    }
                },
                UnaryOp::FNeg => {
                    Instruction::ROp {
                        rd,
                        op: ROp::FSub,
                        rs1: Register::Zero,
                        rs2: reg_of(id, color_list),
                    }
                },
            };
            push(program, instruction, instr_span);
        },
        RawInstr::BiOp { id_left, op, id_right } => {
            let op = match op {
                BinaryOp::Add => ROp::Add,
                BinaryOp::Sub => ROp::Sub,
                BinaryOp::Mul => ROp::Mul,
                BinaryOp::Div => ROp::Div,
                BinaryOp::FAdd => ROp::FAdd,
                BinaryOp::FSub => ROp::FSub,
                BinaryOp::FMul => ROp::FMul,
                BinaryOp::FDiv => ROp::FDiv,
                BinaryOp::LShift => ROp::LShift,
                BinaryOp::RShift => ROp::RShift,
            };
            push(program, Instruction::ROp {
                rd,
                op,
                rs1: reg_of(id_left, color_list),
                rs2: reg_of(id_right, color_list),
            }, instr_span)
        },
        RawInstr::BiImm { id_left, op, imm } => {
            let instruction = match op {
                BinaryOp::Add => Instruction::IOp {
                    rd,
                    op: IOp::Add,
                    rs1: reg_of(id_left, color_list),
                    imm,
                },
                BinaryOp::Sub => Instruction::IOp {
                    rd,
                    op: IOp::Add,
                    rs1: reg_of(id_left, color_list),
                    imm: -imm,
                },
                BinaryOp::LShift => Instruction::IOp{
                    rd,
                    op: IOp::LShift,
                    rs1: reg_of(id_left, color_list),
                    imm,
                },
                BinaryOp::RShift => Instruction::IOp{
                    rd,
                    op: IOp::RShift,
                    rs1: reg_of(id_left, color_list),
                    imm,
                },
                _ => {
                    log::error!("I-type instructions for {op} is not supported");
                    panic!("internal compiler error");
                }
            };
            push(program, instruction, instr_span)
        },
        RawInstr::FCondOp { id_left, op, id_right } => {
            let op = match op {
                CondOp::Eq => ROp::FEq,
                CondOp::LEq => ROp::FLEq,
            };
            push(program, Instruction::ROp {
                rd,
                op,
                rs1: reg_of(id_left, color_list),
                rs2: reg_of(id_right, color_list),
            }, instr_span);
        },
        RawInstr::CallCls { cls, args } => {
            let arg_num = args.len();
            if arg_num >= asm::AREG_NUM {
                log::error!("too many arguments to fit in register");
                panic!("internal compiler error");
            }
            // 引数を引数用レジスタに移動
            for (index, arg) in args.into_iter().enumerate() {
                push(program, Instruction::Move {
                    rd: Register::A(index),
                    rs: reg_of(arg, color_list),
                }, instr_span);
            }
            // クロージャーを引数用レジスタに移動
            let reg_cls = reg_of(cls, color_list);
            push(program, Instruction::Move {rd: Register::A(arg_num), rs: reg_cls}, instr_span);
            // 関数のアドレスをクロージャーから取り出してコール
            push(program, Instruction::Load {rd: Register::X, imm: 0, rs1: reg_cls }, instr_span);
            push(program, Instruction::Call(Register::X, is_tail), instr_span);
            // (tail でなければ) 関数の戻り値を rd に格納
            if !is_tail {
                push(program, Instruction::Move {rd, rs: Register::A(0)}, instr_span);
            }
            return;
        },
        RawInstr::CallDir { tag, args } => {
            let arg_num = args.len();
            if arg_num > asm::AREG_NUM {
                log::error!("too many arguments to fit in register");
                panic!("internal compiler error");
            }
            // 引数を引数用レジスタに移動
            for (index, arg) in args.into_iter().enumerate() {
                push(program, Instruction::Move {
                    rd: Register::A(index),
                    rs: reg_of(arg, color_list),
                }, instr_span);
            }
            // 関数をコール
            push(program, Instruction::CallTag(tag, is_tail), instr_span);
            // (tail でなければ) 関数の戻り値を rd に格納
            if !is_tail {
                push(program, Instruction::Move {rd, rs: Register::A(0)}, instr_span);
            }
            return;
        },
        RawInstr::NewTuple(elms) => {
            // タプル用のメモリに要素をストア
            let mut tuple_bytes = 0;
            for elm in elms {
                push(program, Instruction::Store { rs2: reg_of(elm, color_list), imm: tuple_bytes, rs1: Register::Hp }, instr_span);
                tuple_bytes += 4;
            }
            // ヒープにタプル用のメモリを確保
            push(program, Instruction::Move {rd, rs: Register::Hp}, instr_span);
            // ヒープポインタを移動
            push(program, Instruction::IOp { rd: Register::Hp, op: IOp::Add, rs1: Register::Hp, imm: tuple_bytes }, instr_span);
        },
        RawInstr::NewClosure { tag, free_vars } => {
            // クロージャーのメモリ先頭に関数ポインタをストア
            push(program, Instruction::LoadTag { rd: Register::X, tag }, instr_span);
            push(program, Instruction::Store { rs2: Register::X, imm: 0, rs1: Register::Hp }, instr_span);
            let mut closure_bytes = 4;
            // クロージャーのメモリに自由変数をストア
            for free_var in free_vars {
                push(program, Instruction::Store { rs2: reg_of(free_var, color_list), imm: closure_bytes, rs1: Register::Hp }, instr_span);
                closure_bytes += 4;
            }
            // ヒープにクロージャー用のメモリを確保
            push(program, Instruction::Move {rd, rs: Register::Hp}, instr_span);
            // ヒープポインタを移動
            push(program, Instruction::IOp { rd: Register::Hp, op: IOp::Add, rs1: Register::Hp, imm: closure_bytes }, instr_span);
        },
        RawInstr::Read { address, offset } => {
            push(program, Instruction::Load{ rd, imm: offset, rs1: reg_of(address, color_list) }, instr_span);
        },
        RawInstr::Write { address, offset, value } => {
            push(program, Instruction::Store { rs2: reg_of(value, color_list), imm: offset, rs1: reg_of(address, color_list) }, instr_span);
        },
        RawInstr::If { id_left, op, id_right, exp_then, exp_else } => {
            // 条件が成立した場合ブランチする
            let tag_then = generate_id("if");
            let tag_cont = generate_id("fi");
            let op = match op {
                CondOp::Eq => BOp::Eq,
                CondOp::LEq => BOp::LEq,
            };
            push(program, Instruction::Branch { op, rs1: reg_of(id_left, color_list), rs2: reg_of(id_right, color_list), tag: tag_then.clone() }, instr_span);
            // 条件が成立しなかった場合
            push_expr(*exp_else, rd, program, is_tail, color_list);
            // (tail でなければ) 後続の処理にジャンプ
            if !is_tail {
                push(program, Instruction::Jump{ tag: tag_cont.clone() }, instr_span)
            }
            // 条件が成立した場合
            push(program, Instruction::Tag(tag_then), instr_span);
            push_expr(*exp_then, rd, program, is_tail, color_list);
            if !is_tail {
                push(program, Instruction::Tag(tag_cont), instr_span);
            }
            return;
        },
        RawInstr::IfZero { id, exp_then, exp_else } => {
            // 条件が成立した場合ブランチする
            let tag_then = generate_id("if");
            let tag_cont = generate_id("fi");
            push(program, Instruction::Branch { op: BOp::Eq, rs1: reg_of(id, color_list), rs2: Register::Zero, tag: tag_then.clone() }, instr_span);
            // 条件が成立しなかった場合
            push_expr(*exp_else, rd, program, is_tail, color_list);
            // (tail でなければ) 後続の処理にジャンプ
            if !is_tail {
                push(program, Instruction::Jump{ tag: tag_cont.clone() }, instr_span)
            }
            // 条件が成立した場合
            push(program, Instruction::Tag(tag_then), instr_span);
            push_expr(*exp_then, rd, program, is_tail, color_list);
            if !is_tail {
                push(program, Instruction::Tag(tag_cont), instr_span);
            }
            return;
        },
    }
    if is_tail {
        push(program, Instruction::Return, instr_span);
    }
}

fn push_expr(expr: Expr, rd: Register, program: &mut Program, is_tail: bool, color_list: &FnvHashMap<String, usize>) {
    match expr.item {
        RawExpr::LetIn { id, instr_id, instr_suc } => {
            push_instructions(instr_id, reg_of(id, color_list), program, false, color_list);
            push_expr(*instr_suc, rd, program, is_tail, color_list);
        },
        RawExpr::Is(instr) => {
            push_instructions(instr, rd, program, is_tail, color_list)
        },
    }
}

fn convert_function(function: Function, color_list: &FnvHashMap<String, usize>) -> super::asm::Function {
    let mut program = Program::new();
    let span = function.body.span;
    let arg_count = function.args.len();
    // A レジスタから S レジスタに引数を移す
    for (index, arg) in function.args.into_iter().enumerate() {
        program.push(Spanned::new(Instruction::Move{ rd: reg_of(arg, color_list), rs: Register::A(index) }, span));
    }
    // クロージャー自身を S レジスタに渡す
    if arg_count >= AREG_NUM {
        log::error!("too many arguments to fit in register");
        panic!("internal compiler error");
    }
    if !function.free_vars.is_empty() {
        program.push(Spanned::new(Instruction::Move{ rd: reg_of(function.tag.clone(), color_list), rs: Register::A(arg_count) }, span));
    }
    // クロージャー内の自由変数を S レジスタに展開する
    let mut offset = 4;
    for free_var in function.free_vars {
        program.push(Spanned::new(Instruction::Load{ rd: reg_of(free_var, color_list), imm: offset, rs1: Register::A(arg_count) }, span));
        offset += 4;
    }
    // 関数本体を展開する
    let is_tail = function.tag != "min_caml_start";
    push_expr(function.body, Register::A(0), &mut program, is_tail, color_list);
    super::asm::Function::new(function.tag, program)
}

pub fn do_register_allocation(functions: Vec<Function>) -> Vec<super::asm::Function> {
    let mut res = vec![];
    for function in functions {
        let graph = build_graph(&function.body);
        let color_list = map_register(graph, 50);
        let function = convert_function(function, &color_list);
        let function = if function.tag == "min_caml_start" {
            function 
        }
        else {
            function.add_prologue_epilogue()
        };
        res.push(function);
    }
    res
}
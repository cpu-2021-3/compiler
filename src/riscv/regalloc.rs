use std::{collections::HashSet, mem::swap};

use super::specific::{Function, Expr, RawExpr, RawInstr};

#[derive(Clone, Debug)]
struct Graph {
    edges: HashSet<(String, String)>
}

// interference graph
impl Graph {
    // 新しいグラフを生成
    fn new() -> Self {
        Graph {edges: HashSet::new()}
    }
    // u と v を辺でつなぐ
    fn add(&mut self, mut u: String, mut v: String) {
        if u == v {
            return;
        }
        if u > v {
            swap(&mut u, &mut v);
        }
        self.edges.insert((u, v));
    }
    // s 中のすべての異なる 2 頂点同士を辺でつなぐ
    fn clique(&mut self, s: &HashSet<String>) {
        for x in s {
            for y in s {
                self.add(x.clone(), y.clone());
            }
        }
    }
    // 別のグラフと合体する
    fn merge(&mut self, o: Graph) {
        for edge in o.edges {
            self.edges.insert(edge);
        }
    }
}


// 活性解析の結果を格納するデータ構造
#[derive(Clone)]
struct Data {
    // ある命令以前から生きているレジスタの集合
    pub prop: HashSet<String>, 
    // ある命令以後にある依存関係のグラフ
    pub graph: Graph
}

impl Data {
    // prop 中のすべての 2 頂点を辺で結んだうえで Data を返す
    fn clique_prop(prop: HashSet<String>, mut graph: Graph) -> Self {
        graph.clique(&prop);
        Self {prop, graph}
    }

    fn new() -> Self {
        Self {prop: HashSet::new(), graph: Graph::new() }
    }
}

// ret <- instr 文の後ろからの Data をもとに、ret <- instr 文の前の Data を計算する
fn update_prop(prop: &mut HashSet<String>, ret: &String, instr: &RawInstr) {
    prop.remove(ret);
    match instr {
        RawInstr::Unit | RawInstr::Int(_) | RawInstr::Float(_) | RawInstr::DataTag(_) => {},
        RawInstr::Var(id) => {
            prop.insert(id.clone());
        },
        RawInstr::UnOp { op, id } => {
            prop.insert(id.clone());
        },
        RawInstr::BiOp { id_left, op, id_right } => {
            prop.insert(id_left.clone());
            prop.insert(id_right.clone());
        },
        RawInstr::BiImm { id_left, op, imm } => {
            prop.insert(id_left.clone());
        },
        RawInstr::FCondOp { id_left, op, id_right } => {
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
        RawInstr::CallDir { tag, args } => {
            args.into_iter().for_each(|arg|{prop.insert(arg.clone());});
        },
        RawInstr::NewTuple(elms) => {
            elms.into_iter().for_each(|elm|{prop.insert(elm.clone());});
        },
        RawInstr::NewClosure { tag, free_vars } => {
            free_vars.into_iter().for_each(|free_var|{prop.insert(free_var.clone());});
        },
        RawInstr::Read { address, offset } => {
            prop.insert(address.clone());
        },
        RawInstr::Write { address, offset, value } => {
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
                RawInstr::If { id_left, op, id_right, exp_then, exp_else } => {
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
                RawInstr::If { id_left, op, id_right, exp_then, exp_else } => {
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
fn build_graph(expr: &Expr) {
    println!("{}", expr.item);
    let data = liveness_info(expr, "dummy!".to_string(), &Data::new());
    println!("{:#?}", data.graph);
}

pub fn do_register_allocation(functions: Vec<Function>) {
    for function in functions {
        // A レジスタから S レジスタに引数を移す
        // クロージャー内の自由変数を S レジスタに展開する
        build_graph(&function.body);
    }
}
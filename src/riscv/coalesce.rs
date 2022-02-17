use fnv::{FnvHashSet, FnvHashMap};

use crate::{riscv::asm::{Instruction, Register, ROp, IOp}, span::Spanned};

use super::asm::{Function, Program};

// 各命令について、直前の命令が存在するかどうか　各タグについて、どこから飛んでくるのか　を計算
fn flow_of_program(program: &Program) -> (Vec<bool>, FnvHashMap<String, FnvHashSet<usize>>) {
    let program_len = program.len();
    let mut prev_exists = vec![false; program_len + 1];
    let mut prev_of_tags = FnvHashMap::default();

    for (index, instruction) in program.iter().enumerate() {
        match &instruction.item {
            Instruction::SetImm { rd: _, imm: _ } |
            Instruction::Move { rd: _, rs: _ } |
            Instruction::ROp { rd: _, op: _, rs1: _, rs2: _ } |
            Instruction::IOp { rd: _, op: _, rs1: _, imm: _ } | 
            Instruction::Call(_, false) |
            Instruction::CallTag(_, false) |
            Instruction::LoadTag { rd: _, tag: _ } |
            Instruction::Load { rd: _, imm: _, rs1: _ } |
            Instruction::Store { rs2: _, imm: _, rs1: _ } |
            Instruction::Tag(_) =>
            {
                // 通常の命令やタグ
                prev_exists[index + 1] = true;
            },
            Instruction::Branch { op: _, rs1: _, rs2: _, tag } => {
                // ブランチ (一つ先にもジャンプ先にも飛びうる)
                prev_exists[index + 1] = true;
                prev_of_tags.entry(tag.clone()).or_insert(FnvHashSet::default()).insert(index);
            },
            Instruction::Jump { tag } => {
                // ジャンプ (ジャンプ先のみ)
                prev_of_tags.entry(tag.clone()).or_insert(FnvHashSet::default()).insert(index);
            },
            Instruction::Call(_, true) |
            Instruction::CallTag(_, true) |
            Instruction::Return => {}, // 次の命令は無い
        }
    }

    (prev_exists, prev_of_tags)
}

// プログラム中の各命令について、その命令より後に生きているレジスタの集合を計算
fn liveness_of_program(program: &Program) -> Vec<FnvHashSet<Register>> {
    let program_len = program.len();
    let (prev_exists, mut prev_of_tags) = flow_of_program(program);
    // live_afters[i] := i 番目の命令直後に生きているレジスタの集合
    let mut live_afters = vec![FnvHashSet::<Register>::default(); program_len];

    // 配る DP を後ろから行う
    for (index, instruction) in program.iter().enumerate().rev() {
        // 直前の命令集合と、この命令より前に生きているレジスタ集合を計算
        let (prevs, live_before) = match &instruction.item {
            Instruction::Tag(tag) => {
                // このタグの直前の命令を計算
                let mut prevs = prev_of_tags.entry(tag.clone()).or_insert(FnvHashSet::default()).clone();
                if prev_exists[index] {
                    prevs.insert(index - 1);
                }
                (prevs.into_iter().collect(), live_afters[index].clone())
            },
            Instruction::SetImm { rd, imm: _ } => {
                let prevs = if prev_exists[index] { vec![index - 1] } else { vec![] };
                let mut live_before = live_afters[index].clone();
                live_before.remove(rd);
                (prevs, live_before)
            },
            Instruction::Move { rd, rs } => {
                let prevs = if prev_exists[index] { vec![index - 1] } else { vec![] };
                let mut live_before = live_afters[index].clone();
                live_before.remove(rd);
                live_before.insert(*rs);
                (prevs, live_before)
            },
            Instruction::ROp { rd, op: _, rs1, rs2 } => {
                let prevs = if prev_exists[index] { vec![index - 1] } else { vec![] };
                let mut live_before = live_afters[index].clone();
                live_before.remove(rd);
                live_before.insert(*rs1);
                live_before.insert(*rs2);
                (prevs, live_before)
            },
            Instruction::IOp { rd, op: _, rs1, imm: _ } => {
                let prevs = if prev_exists[index] { vec![index - 1] } else { vec![] };
                let mut live_before = live_afters[index].clone();
                live_before.remove(rd);
                live_before.insert(*rs1);
                (prevs, live_before)
            },
            Instruction::Branch { op: _, rs1, rs2, tag: _ } => {
                let prevs = if prev_exists[index] { vec![index - 1] } else { vec![] };
                let mut live_before = live_afters[index].clone();
                live_before.insert(*rs1);
                live_before.insert(*rs2);
                (prevs, live_before)
            },
            Instruction::Jump { tag: _ } => {
                let prevs = if prev_exists[index] { vec![index - 1] } else { vec![] };
                let live_before = live_afters[index].clone();
                (prevs, live_before)
            },
            Instruction::Call(reg, _) => {
                let prevs = if prev_exists[index] { vec![index - 1] } else { vec![] };
                let mut live_before = live_afters[index].clone();
                live_before.insert(*reg);
                (prevs, live_before)
            },
            Instruction::CallTag(_, _) => {
                let prevs = if prev_exists[index] { vec![index - 1] } else { vec![] };
                let live_before = live_afters[index].clone();
                (prevs, live_before)
            },
            Instruction::LoadTag { rd, tag: _ } => {
                let prevs = if prev_exists[index] { vec![index - 1] } else { vec![] };
                let mut live_before = live_afters[index].clone();
                live_before.remove(rd);
                (prevs, live_before)
            },
            Instruction::Load { rd, imm: _, rs1 } => {
                let prevs = if prev_exists[index] { vec![index - 1] } else { vec![] };
                let mut live_before = live_afters[index].clone();
                live_before.remove(rd);
                live_before.insert(*rs1);
                (prevs, live_before)
            },
            Instruction::Store { rs2, imm: _, rs1 } => {
                let prevs = if prev_exists[index] { vec![index - 1] } else { vec![] };
                let mut live_before = live_afters[index].clone();
                live_before.insert(*rs1);
                live_before.insert(*rs2);
                (prevs, live_before)
            },
            Instruction::Return => {
                let prevs = if prev_exists[index] { vec![index - 1] } else { vec![] };
                let live_before = live_afters[index].clone();
                (prevs, live_before)
            },
        };
        // この命令より前に来うる各命令について
        for prev in prevs.iter() {
            // 生きているレジスタ集合を伝播
            live_afters[*prev].extend(live_before.clone());
        }
    }
    live_afters
}

impl Function {
    // レジスタの合体を行い、mv がされやすくなるようにする
    pub fn coalesce(self) -> Self {
        let program = self.program;
        let program_len = program.len();

        let (prev_exists, mut prev_of_tags) = flow_of_program(&program);

        // 前から順番に見ていき、一致環境を更新
        // env_lists[i] := i 番目の命令の後に適用できる置き換えレジスタ表
        let mut env_lists = vec![FnvHashMap::<Register, Register>::default(); program_len];
        let mut new_program = vec![];
        // 表から reg に関する情報を取り除く
        fn remove_reg_from_list (env_list: &FnvHashMap<Register, Register>, reg: &Register) -> FnvHashMap<Register, Register> {
            env_list.clone().into_iter().filter(|(rk, rv)| {
                *rk != *reg && *rv != *reg
            }).collect()
        }
        // 表から callee-save ではないレジスタの情報を取り除く
        fn remove_tmps_from_list (env_list: &FnvHashMap<Register, Register>) -> FnvHashMap<Register, Register> {
            env_list.clone().into_iter().filter(|(rk, rv)| {
                rk.is_stable() && rv.is_stable()
            }).collect()
        }
        for (index, instruction) in program.into_iter().enumerate() {
            // 前の命令が存在する場合
            if prev_exists[index] {
                // 置き換え表を持ってくる
                env_lists[index] = env_lists[index - 1].clone();
            }
            let new_instruction = match instruction.item {
                Instruction::Tag(tag) => {
                    // このタグの直前の命令を計算
                    let prevs = prev_of_tags.entry(tag.clone()).or_insert(FnvHashSet::default());
                    if prev_exists[index] {
                        prevs.insert(index - 1);
                    }
                    // 直前の命令集合が空でなければ
                    if let Some(first_prev) = prevs.iter().next() {
                        let mut new_list = FnvHashMap::default();
                        // 置き換え表を計算
                        for (k, v) in env_lists.get(*first_prev).unwrap() {
                            new_list.insert(k.clone(), v.clone());
                        }
                        for prev in prevs.iter() {
                            let mut remove_keys = vec![];
                            for (k, v1) in new_list.iter() {
                                if let Some(v2) = env_lists.get(*prev).unwrap().get(k) {
                                    if v1 != v2 {
                                        remove_keys.push(k.clone());
                                    }
                                }
                                else {
                                    remove_keys.push(k.clone());
                                }
                            }
                            for remove_key in remove_keys {
                                new_list.remove(&remove_key);
                            }
                        }
                        env_lists[index] = new_list;
                    } 
                    Instruction::Tag(tag)
                },
                Instruction::Move { rd, mut rs } => {
                    // rs を置き換える
                    if let Some(reg) = env_lists[index].get(&rs) {
                        rs = reg.clone();
                    }
                    // 置き換え表を更新
                    if rd != rs && rd != Register::Zero {
                        // いままでの rd に関する情報は削除
                        env_lists[index] = remove_reg_from_list(&env_lists[index], &rd);
                        // rd -> rs を登録
                        env_lists[index].insert(rd, rs);
                    }
                    Instruction::Move {rd, rs}
                },
                Instruction::SetImm { rd, imm } => {
                    // rd にかかわる情報を削除する
                    env_lists[index] = remove_reg_from_list(&env_lists[index], &rd);
                    Instruction::SetImm {rd, imm}
                },
                Instruction::ROp { rd, op, mut rs1, mut rs2 } => {
                    // rs1, rs2 を置き換える
                    if let Some(reg) = env_lists[index].get(&rs1) {
                        rs1 = *reg;
                    }
                    if let Some(reg) = env_lists[index].get(&rs2) {
                        rs2 = *reg;
                    }
                    // rd にかかわる情報を削除する
                    env_lists[index] = remove_reg_from_list(&env_lists[index], &rd);
                    Instruction::ROp {rd, op, rs1, rs2}
                },
                Instruction::IOp { rd, op, mut rs1, imm } => {
                    // rs1 を置き換える
                    if let Some(reg) = env_lists[index].get(&rs1) {
                        rs1 = *reg;
                    }
                    // rd にかかわる情報を削除する
                    env_lists[index] = remove_reg_from_list(&env_lists[index], &rd);
                    Instruction::IOp {rd, op, rs1, imm}
                },
                Instruction::LoadTag { rd, tag } => {
                    // rd にかかわる情報を削除する
                    env_lists[index] = remove_reg_from_list(&env_lists[index], &rd);
                    Instruction::LoadTag {rd, tag}
                },
                Instruction::Load { rd, imm, mut rs1 } => {
                    // rs1 を置き換える
                    if let Some(reg) = env_lists[index].get(&rs1) {
                        rs1 = *reg;
                    }
                    // rd にかかわる情報を削除する
                    env_lists[index] = remove_reg_from_list(&env_lists[index], &rd);
                    Instruction::Load {rd, imm, rs1}
                },
                Instruction::Branch { op, mut rs1, mut rs2, tag } => {
                    // rs1, rs2 を置き換える
                    if let Some(reg) = env_lists[index].get(&rs1) {
                        rs1 = *reg;
                    }
                    if let Some(reg) = env_lists[index].get(&rs2) {
                        rs2 = *reg;
                    }
                    Instruction::Branch{ op, rs1, rs2, tag}
                },
                Instruction::Jump { tag } => {
                    Instruction::Jump {tag}
                },
                Instruction::Call(mut rs, flag) => {
                    // reg を置き換える
                    if let Some(reg) = env_lists[index].get(&rs) {
                        rs = *reg;
                    }
                    // 関数呼び出し前後で変化するレジスタを削除する
                    env_lists[index] = remove_tmps_from_list(&env_lists[index]);
                    Instruction::Call(rs, flag)
                },
                Instruction::CallTag(tag, flag) => {
                    // 関数呼び出し前後で変化するレジスタを削除する
                    env_lists[index] = remove_tmps_from_list(&env_lists[index]);
                    Instruction::CallTag(tag, flag)
                },
                Instruction::Store { mut rs2, imm, mut rs1 } => {
                    // rs1, rs2 を置き換える
                    if let Some(reg) = env_lists[index].get(&rs1) {
                        rs1 = *reg;
                    }
                    if let Some(reg) = env_lists[index].get(&rs2) {
                        rs2 = *reg;
                    }
                    Instruction::Store{rs2, imm, rs1}
                },
                Instruction::Return => Instruction::Return,
            };
            new_program.push(Spanned::new(new_instruction, instruction.span));
        }
        Self::new(self.tag, new_program)
    }
    
    // 不必要な命令 (生きていないレジスタへの代入) を削除する
    pub fn remove_dead_code(self) -> Self {
        let program = self.program;
        let live_afters = liveness_of_program(&program);

        let mut new_program = vec![];

        for (index, instruction) in program.into_iter().enumerate() {
            if let Instruction::Move {rd, rs} = &instruction.item {
                // rd と rs が同じ場合は無駄な命令なのでパス
                if *rd == *rs {
                    continue;
                }
            }
            match &instruction.item {
                Instruction::SetImm { rd, imm: _ } |
                Instruction::Move { rd, rs: _ } |
                Instruction::ROp { rd, op: _, rs1: _, rs2: _ } |
                Instruction::LoadTag { rd, tag: _ } |
                Instruction::Load { rd, imm: _, rs1: _ } |
                Instruction::IOp { rd, op: _, rs1: _, imm: _ } => {
                    // 代入しようとしているレジスタが callee-save か X か であり、この命令より後で生きていない場合
                    if !live_afters[index].contains(&rd) {
                        if rd.is_stable() || *rd == Register::X {
                            // 無駄な命令なのでパス
                            continue;
                        }
                    }
                },
                _ => {}
            }
            // 無駄な代入以外の命令は採用
            new_program.push(instruction);
        }

        Self::new(self.tag, new_program)
    }

    pub fn do_constant_folding(self) -> Self {
        let program = self.program;
        let program_len = program.len();
        let (prev_exists, mut prev_of_tags) = flow_of_program(&program);
        
        let mut const_lists = vec![FnvHashMap::<Register, i32>::default(); program_len];
        let mut new_program = vec![];

        // 表から callee-save ではないレジスタの情報を取り除く
        fn remove_tmps_from_list (const_list: &FnvHashMap<Register, i32>) -> FnvHashMap<Register, i32> {
            const_list.clone().into_iter().filter(|(reg, _)| {
                reg.is_stable()
            }).collect()
        }
        for (index, instruction) in program.into_iter().enumerate() {
            // 前の命令が存在する場合
            if prev_exists[index] {
                // 定数表を持ってくる
                const_lists[index] = const_lists[index - 1].clone();
            }
            let new_instruction = match instruction.item {
                Instruction::Tag(tag) => {
                    // このタグの直前の命令を計算
                    let prevs = prev_of_tags.entry(tag.clone()).or_insert(FnvHashSet::default());
                    if prev_exists[index] {
                        prevs.insert(index - 1);
                    }
                    // 直前の命令集合が空でなければ
                    if let Some(first_prev) = prevs.iter().next() {
                        let mut new_list = FnvHashMap::default();
                        // 置き換え表を計算
                        for (k, v) in const_lists.get(*first_prev).unwrap() {
                            new_list.insert(k.clone(), v.clone());
                        }
                        for prev in prevs.iter() {
                            let mut remove_keys = vec![];
                            for (k, v1) in new_list.iter() {
                                if let Some(v2) = const_lists.get(*prev).unwrap().get(k) {
                                    if v1 != v2 {
                                        remove_keys.push(k.clone());
                                    }
                                }
                                else {
                                    remove_keys.push(k.clone());
                                }
                            }
                            for remove_key in remove_keys {
                                new_list.remove(&remove_key);
                            }
                        }
                        const_lists[index] = new_list;
                    } 
                    Instruction::Tag(tag)
                },
                Instruction::SetImm { rd, imm } => {
                    const_lists[index].insert(rd, imm);
                    Instruction::SetImm {rd, imm}
                },
                Instruction::Move { rd, rs } => {
                    if let Some(imm) = const_lists[index].get(&rs).cloned() {
                        const_lists[index].insert(rd, imm);
                    }
                    else {
                        const_lists[index].remove(&rd);
                    }
                    Instruction::Move {rd, rs}
                },
                Instruction::ROp { rd, op, rs1, rs2 } => {
                    let mut rd_imm = None;
                    if let Some(imm1) = const_lists[index].get(&rs1).cloned() {
                        if let Some(imm2) = const_lists[index].get(&rs2).cloned() {
                            let imm = match op {
                                ROp::Add => Some(imm1 + imm2),
                                ROp::Sub => Some(imm1 - imm2),
                                ROp::Mul => Some(imm1 * imm2),
                                ROp::Div => Some(imm1 / imm2),
                                ROp::LShift => Some(imm1 << imm2),
                                ROp::RShift => Some(imm1 >> imm2),
                                _ => {None}
                            };
                            if let Some(imm) = imm {
                                const_lists[index].insert(rd, imm);
                                rd_imm = Some(imm);
                            }
                        }
                    }
                    match rd_imm {
                        Some(imm) => Instruction::SetImm { rd, imm },
                        None => {
                            const_lists[index].remove(&rd);
                            Instruction::ROp {rd, op, rs1, rs2}
                        }
                    }
                },
                Instruction::IOp { rd, op, rs1, imm: imm2 } => {
                    let mut rd_imm = None;
                    if let Some(imm1) = const_lists[index].get(&rs1).cloned() {
                        let imm = match op {
                            IOp::Add => imm1 + imm2,
                            IOp::LShift => imm1 << imm2,
                            IOp::RShift => imm1 >> imm2,
                        };
                        const_lists[index].insert(rd, imm);
                        rd_imm = Some(imm);
                    }
                    match rd_imm {
                        Some(imm) => Instruction::SetImm { rd, imm },
                        None => {
                            const_lists[index].remove(&rd);
                            Instruction::IOp {rd, op, rs1, imm: imm2}
                        }
                    }
                },
                Instruction::Branch { op, rs1, rs2, tag } => {
                    Instruction::Branch {op, rs1, rs2, tag}
                },
                Instruction::Jump { tag } => {
                    Instruction::Jump {tag}
                },
                Instruction::Call(reg, is_tail) => {
                    const_lists[index] = remove_tmps_from_list(&const_lists[index]);
                    Instruction::Call(reg, is_tail)
                },
                Instruction::CallTag(tag, is_tail) => {
                    const_lists[index] = remove_tmps_from_list(&const_lists[index]);
                    Instruction::CallTag(tag, is_tail)
                },
                Instruction::LoadTag { rd, tag } => {
                    const_lists[index].remove(&rd);
                    Instruction::LoadTag {rd, tag}
                },
                Instruction::Load { rd, imm, rs1 } => {
                    const_lists[index].remove(&rd);
                    Instruction::Load {rd, imm, rs1}
                },
                Instruction::Store { rs2, imm, rs1 } => {
                    Instruction::Store {rs2, imm, rs1}
                },
                Instruction::Return => {
                    Instruction::Return
                },
            };
            new_program.push(Spanned::new(new_instruction, instruction.span));
        }
        Self::new(self.tag, new_program)
    }
}
use crate::api::*;
use crate::gap::*;
use crate::graph::*;
use crate::*;
/*
<
        G: GroupHelper<R> + Clone + PartialEq + Eq + Hash,
        R: RegisterHelper<G> + Clone + PartialEq + Eq + Hash,
        K: KindHelper<G, R> + Clone + PartialEq + Eq + Hash,
>

*/
use std::collections::VecDeque;

pub trait Generator<K, G> {
    fn generate(&self, g: &mut G);
}

pub trait GeneratorFunctions<K, G: GroupHelper<R>, R: RegisterHelper<G>> {
    fn prelude(&mut self);
    fn epilogue(&mut self);
    fn swap(&mut self, left: &Value<G, R>, right: &Value<G, R>);
    fn mov(&mut self, from: &Value<G, R>, to: &Value<G, R>);
    fn block(&mut self, id: BlockId);
    fn goto(&mut self, id: BlockId);
    fn instr(
        &mut self,
        kind: &K,
        output: Option<Value<G, R>>,
        inputs: &VecDeque<Value<G, R>>,
        temporary: &VecDeque<Value<G, R>>,
        succ: &VecDeque<BlockId>,
    );
}

impl<
        G: GroupHelper<R> + Clone + PartialEq + Eq + Hash,
        R: RegisterHelper<G> + Clone + PartialEq + Eq + Hash,
        K: KindHelper<G, R> + Clone + PartialEq + Eq + Hash,
        GF: GeneratorFunctions<K, G, R>,
    > Generator<K, GF> for Graph<K, G, R>
{
    fn generate(&self, g: &mut GF) {
        g.prelude();

        for (id, instr) in self.instructions.iter().enumerate() {
            let instr: Instruction<K, G, R> = instr.clone().unwrap();

            match instr.kind {
                Phi(_) => loop {},
                _ => (),
            }

            let block = self.get_block(&instr.block);
            if id == block.start().to_usize() {
                g.block(block.id.clone());
            }

            let is_gap = match instr.kind {
                Gap => true,
                _ => false,
            };
            if is_gap || self.gaps.contains_key(&id) {
                self.generate_gap(g, &InstrId(id));
            }

            if !is_gap {
                let output = match instr.output {
                    Some(ref out) => {
                        let group = instr.kind.result_kind().unwrap().group();
                        self.get_value(
                            out,
                            if instr.kind.clobbers(&group) {
                                instr.id.next()
                            } else {
                                instr.id.clone()
                            },
                        )
                    }
                    None => None,
                };

                let inputs: VecDeque<Value<G, R>> = instr
                    .inputs
                    .iter()
                    .map(|inp| {
                        self.get_value(&self.get_output(inp), instr.clone().id)
                            .expect("input")
                    })
                    .collect();

                let temporary: VecDeque<Value<G, R>> = instr
                    .temporary
                    .iter()
                    .map(|tmp| self.get_value(tmp, instr.clone().id).expect("temporary"))
                    .collect();

                match instr.kind {
                    Phi(_) => (),
                    ToPhi(_) => {
                        assert!(inputs.len() == 1);
                        let out = output.expect("ToPhi output");

                        if out != inputs[0] {
                            g.mov(&inputs[0], &out);
                        }
                    }
                    Gap => (),
                    User(ref k) => g.instr(k, output, &inputs, &temporary, &block.successors),
                }

                if instr.id == block.end().prev() {
                    match block.successors.len() {
                        0 => g.epilogue(),
                        1 => {
                            if block.successors[0].to_usize() != block.id.to_usize() + 1 {
                                // Goto to non-consequent successor
                                g.goto(block.successors[0].clone())
                            }
                        }
                        2 => (), // Should be handled in instruction
                        _ => panic!("Too much successors"),
                    }
                }
            }
        }
    }
}

pub trait GeneratorHelper<K, GF> {
    fn generate_gap(&self, g: &mut GF, id: &InstrId);
}

impl<
        G: GroupHelper<R> + Clone + PartialEq + Eq + Hash,
        R: RegisterHelper<G> + Clone + PartialEq + Eq + Hash,
        K: KindHelper<G, R> + Clone + PartialEq + Eq + Hash,
        GF: GeneratorFunctions<K, G, R>,
    > GeneratorHelper<K, GF> for Graph<K, G, R>
{
    fn generate_gap(&self, g: &mut GF, id: &InstrId) {
        match self.gaps.find(&id.to_usize()) {
            Some(state) => {
                for action in state.actions.iter() {
                    let from = self.get_interval(&action.from).value.clone();
                    let to = self.get_interval(&action.to).value.clone();
                    match action.kind {
                        Swap => g.swap(&from, &to),
                        Move => g.mov(&from, &to),
                    }
                }
            }
            None => {}
        }
    }
}

use crate::api::*;
use crate::graph::*;
use crate::*;

pub trait Liveness {
    fn liveness_analysis(&mut self);
}

use std::collections::VecDeque;

trait LivenessHelper {
    fn build_local(&mut self, blocks: &VecDeque<BlockId>);

    // Build live_in, live_out
    fn build_global(&mut self, blocks: &VecDeque<BlockId>);
}

impl<
        G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
        R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
        K: KindHelper<G, R> + Eq + Clone + PartialEq + Hash,
    > Liveness for Graph<K, G, R>
{
    fn liveness_analysis(&mut self) {
        let blocks = self.get_block_list();
        self.build_local(&blocks);
        self.build_global(&blocks);
    }
}

impl<
        G: GroupHelper<R> + Clone + PartialEq + Eq + Hash,
        R: RegisterHelper<G> + Clone + PartialEq + Eq + Hash,
        K: KindHelper<G, R> + Clone + PartialEq + Eq + Hash,
    > LivenessHelper for Graph<K, G, R>
{
    fn build_local(&mut self, blocks: &VecDeque<BlockId>) {
        for block in blocks.iter() {
            let instructions = self.get_block(block).instructions.clone();

            for instr in instructions.iter() {
                let output = self.get_instr(instr).output.clone();
                let inputs = self.get_instr(instr).inputs.clone();

                match output {
                    Some(output) => self
                        .get_mut_block(block)
                        .live_kill
                        .insert(output.to_usize()),
                    None => true,
                };

                for input_instr in inputs.iter() {
                    let input = self.get_output(input_instr);
                    if !self.get_block(block).live_kill.contains(&input.to_usize()) {
                        self.get_mut_block(block).live_gen.insert(input.to_usize());
                    }
                }
            }
        }
    }
    fn build_global(&mut self, blocks: &VecDeque<BlockId>) {
        let mut change = true;
        while change {
            change = false;

            for block in blocks.iter().rev() {
                let successors = self.get_block(&block).successors.clone();
                let mut tmp = BitvSet::new();

                for succ in successors.iter() {
                    tmp.union_with(&self.get_block(succ).live_in);
                }

                if self.get_block(&block).live_out != tmp {
                    self.get_mut_block(&block).live_out = tmp;
                    change = true;
                }

                let mut old = self.get_block(&block).live_out.clone();

                old.difference_with(&self.get_block(&block).live_kill);
                old.union_with(&self.get_block(&block).live_gen);
                if old != self.get_block(&block).live_in {
                    self.get_mut_block(&block).live_in = old;
                    change = true;
                }
            }
        }
    }
}

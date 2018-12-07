use crate::api::*;
use crate::graph::*;
use crate::*;
use std::collections::VecDeque;

pub struct MapResult {
    pub block: BlockId,
    pub score: usize,
}

pub trait Flatten {
    fn flatten(&mut self) {}
}

trait FlattenHelper {
    fn flatten_get_ends(&mut self) -> SmallIntMap<VecDeque<BlockId>> {
        SmallIntMap::new()
    }

    fn flatten_assign_indexes(&mut self) {}
    fn flatten_reindex_blocks(&mut self, _list: &VecDeque<BlockId>) -> VecDeque<BlockId> {
        VecDeque::new()
    }
    fn flatten_reindex_instructions(&mut self, _list: &VecDeque<BlockId>) {}
}

use std::hash::Hash;

impl<
        G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
        R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
        K: KindHelper<G, R> + Eq + Clone + PartialEq + Hash,
    > FlattenHelper for Graph<K, G, R>
{
    fn flatten_get_ends(&mut self) -> SmallIntMap<VecDeque<BlockId>> {
        let root = self.clone().root.expect("Root block");
        let mut queue = VecDeque::new();
        queue.push_back(root.clone());
        let mut visited = BitvSet::new();
        let mut ends: SmallIntMap<VecDeque<BlockId>> = SmallIntMap::new();

        while queue.len() > 0 {
            let cur = queue.pop_front().unwrap();
            visited.insert(cur.to_usize());
            for succ in self.get_block(&cur).successors.iter() {
                if visited.contains(&succ.to_usize()) {
                    if ends.contains_key(&succ.to_usize()) {
                        ends.find_mut(&succ.to_usize())
                            .unwrap()
                            .push_back(cur.clone());
                    } else {
                        let mut v = VecDeque::new();
                        v.push_back(cur.clone());
                        ends.insert(succ.to_usize(), v);
                    }
                } else {
                    queue.push_back(succ.clone());
                }
            }
        }

        ends
    }

    fn flatten_assign_indexes(&mut self) {
        let ends = self.flatten_get_ends();
        let mut loop_index = 1;
        for (start, ends) in ends.iter().enumerate() {
            let ends = ends.clone().unwrap();
            let start_id = BlockId(start.clone());
            let mut visited = BitvSet::new();

            let mut queue = VecDeque::new();
            let expected_depth = self.get_block(&start_id).loop_depth.clone();
            assert!(self.get_block(&start_id).incoming_forward_branches == 2);
            for end in ends.iter() {
                queue.push_back(end.clone());
            }
            while queue.len() > 0 {
                let cur = queue.remove(0).unwrap();
                let block = self.get_mut_block(&cur);

                if !visited.insert(cur.to_usize()) {
                    loop {
                        println!("looping")
                    }
                }

                if block.loop_depth == expected_depth {
                    block.loop_index = loop_index;
                    block.loop_depth += 1;
                }

                if cur.to_usize() != start {
                    for pred in block.predecessors.iter() {
                        queue.push_back(pred.clone())
                    }
                }
            }
            loop_index += 1;
        }
    }

    fn flatten_reindex_blocks(&mut self, list: &VecDeque<BlockId>) -> VecDeque<BlockId> {
        let mut block_id = 0;
        let mut queue = VecDeque::new();
        let mut result = VecDeque::new();
        let mut mapping = SmallIntMap::new();

        for id in list.iter() {
            let mut block = self.blocks.clone().pop(&id.to_usize()).expect("block");
            if block.id == self.clone().root.expect("root block") {
                self.root = Some(BlockId(block_id));
            }

            mapping.insert(block.id.to_usize(), BlockId(block_id));

            block.id = BlockId(block_id);
            block_id += 1;
            for instr_id in block.instructions.iter() {
                self.get_mut_instr(instr_id).block = block.id.clone();
            }

            result.push_back(block.id.clone());
            queue.push_back(block);
        }

        self.blocks.clear();

        while queue.len() > 0 {
            let mut block = queue.pop_back().unwrap();
            //TODO: Rewrite
            /*block.successors = block.successors.iter().map(|succ| {
                *mapping.find(&succ.to_usize()).expect("successor")
            });*/
            for (idx, succ) in block.clone().successors.iter().clone().enumerate() {
                block.successors[idx] = mapping.find(&succ.to_usize()).expect("successor").clone();
            }
            for (idx, pred) in block.clone().predecessors.clone().iter().enumerate() {
                block.successors[idx] =
                    mapping.find(&pred.to_usize()).expect("predecessor").clone();
            }
            //TODO: Rewrite
            /*block.predecessors = block.predecessors.iter().map(|pred| {
                *mapping.find(&pred.to_usize()).expect("predecessor")
            });*/
            self.blocks.insert(block.id.to_usize(), block);
        }
        return result;
    }

    fn flatten_reindex_instructions(&mut self, list: &VecDeque<BlockId>) {
        self.instr_id = 0;
        let mut queue = VecDeque::new();
        let mut map = SmallIntMap::new();

        for block in list.iter() {
            let list = self.get_block(block).instructions.clone();
            let mut new_list = VecDeque::new();

            let start_gap = self.create_gap(block);
            new_list.push_back(start_gap.clone().id);
            queue.push_back(start_gap);

            for (i, id) in list.iter().enumerate() {
                let mut instr = self.instructions.pop(&id.to_usize()).unwrap();

                let id = self.instr_id();
                map.insert(instr.id.to_usize(), id.clone());
                instr.id = id;

                new_list.push_back(instr.clone().id);
                queue.push_back(instr);

                if i != list.len() {
                    let gap = self.create_gap(block);
                    new_list.push_back(gap.id.clone());
                    queue.push_back(gap);
                }
            }
            if list.len() != 0 {
                let end_gap = self.create_gap(block);
                new_list.push_back(end_gap.id.clone());
                queue.push_back(end_gap);
            }

            self.get_mut_block(block).instructions = new_list;
        }

        let mut i = 0;
        while i < self.phis.len() {
            let mut phi = self
                .instructions
                .pop(&self.phis[i].to_usize())
                .expect("Phi");
            let id = self.instr_id();
            map.insert(phi.id.to_usize(), id.clone());
            phi.id = id;
            queue.push_back(phi);
            i += 1;
        }

        self.instructions.clear();

        while queue.len() > 0 {
            let mut instr = queue.pop_back().unwrap();

            for (idx, input) in instr.clone().inputs.clone().iter().enumerate() {
                match map.find(&idx) {
                    Some(r) => instr.inputs[idx] = r.clone(),
                    None => instr.inputs[idx] = input.clone(),
                }
            }

            self.instructions.insert(instr.id.to_usize(), instr);
        }
    }
}

impl<
        G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
        R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
        K: KindHelper<G, R> + Eq + Clone + PartialEq + Hash,
    > Flatten for Graph<K, G, R>
{
    fn flatten(&mut self) {
        self.flatten_assign_indexes();
        let root = self.clone().root.expect("Root block");
        let mut queue = VecDeque::new();
        queue.push_back(root.clone());
        let mut list = VecDeque::new();
        let mut visited = BitvSet::new();

        while queue.len() > 0 {
            let cur = queue.pop_front().unwrap();
            visited.insert(cur.to_usize());
            if !visited.contains(&cur.to_usize()) {
                loop {
                    println!("loooooping");
                }
            }
            list.push_back(cur.clone());

            let successors = self.get_block(&cur).successors.clone();
            for (idx, succ_id) in successors.iter().enumerate() {
                let succ = self.get_mut_block(succ_id);
                if succ.incoming_forward_branches == 0 {
                    loop {}
                }
                succ.incoming_forward_branches -= 1;
                if succ.incoming_forward_branches == 0 {
                    queue.remove(idx);
                }
            }
            list = self.flatten_reindex_blocks(&list);
            self.flatten_reindex_blocks(&list);
        }
    }
}

use crate::graph::*;
use std::hash::Hash;

pub struct BlockBuilder<
    'a,
    K: Eq + Clone + PartialEq + Hash,
    G: Eq + Clone + PartialEq + Hash,
    R: Eq + Clone + PartialEq + Hash,
> {
    pub graph: &'a mut Graph<K, G, R>,
    pub block: BlockId,
}

use std::collections::VecDeque;

pub trait GroupHelper<R>: Clone + Eq + PartialEq + Hash {
    fn groups() -> VecDeque<Self> {
        VecDeque::new()
    }

    fn registers(&self) -> VecDeque<R> {
        VecDeque::new()
    }

    fn to_usize(&self) -> usize {
        0
    }

    fn from_usize(i: usize) -> Self {
        unimplemented!()
    }
}

pub trait RegisterHelper<Group>: Clone + Eq + PartialEq {
    fn group(&self) -> Group {
        unimplemented!()
    }
    fn to_usize(&self) -> usize {
        0xfffff
    }
    fn from_usize(g: &Group, i: usize) -> Self {
        unimplemented!()
    }
}

pub trait GroupAutoHelper<R: Eq + Clone + PartialEq + Hash>: Eq + Clone + PartialEq + Hash {
    fn use_any(&self) -> UseKind<Self, R> {
        UseKind::Invalid
    }

    fn use_reg(&self) -> UseKind<Self, R> {
        UseKind::Invalid
    }
}

pub trait RegisterAutoHelper<G: Eq + Clone + PartialEq + Hash>:
    Eq + Clone + PartialEq + Hash
{
    fn use_fixed(&self) -> UseKind<G, Self> {
        UseKind::Invalid
    }
}

pub trait KindHelper<
    G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
    R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
>: Eq + Clone + PartialEq + Hash
{
    fn clobbers(&self, group: &G) -> bool {
        false
    }

    fn temporary(&self) -> VecDeque<G> {
        VecDeque::new()
    }

    fn use_kind(&self, i: usize) -> UseKind<G, R> {
        UseKind::Invalid
    }

    fn result_kind(&self) -> Option<UseKind<G, R>> {
        None
    }
}

pub trait GraphAPI<
    K: KindHelper<G, R> + Eq + Clone + PartialEq + Hash,
    G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
    R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
>
{
    fn empty_block(&mut self) -> BlockId;
    fn block(&mut self, body: &fn(b: &mut BlockBuilder<K, G, R>)) -> BlockId;
    fn phi(&mut self, group: G) -> InstrId;
    fn with_block(&mut self, id: BlockId, body: &fn(b: &mut BlockBuilder<K, G, R>));
    fn new_instr(&mut self, kind: K, args: VecDeque<InstrId>) -> InstrId;
    fn set_root(&mut self, id: BlockId);
}

impl<
        G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
        R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
    > GroupAutoHelper<R> for G
{
    fn use_any(&self) -> UseKind<G, R> {
        UseAny(self.clone())
    }
    fn use_reg(&self) -> UseKind<G, R> {
        UseRegister(self.clone())
    }
}

impl<
        G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
        R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
    > RegisterAutoHelper<G> for R
{
    fn use_fixed(&self) -> UseKind<G, R> {
        UseFixed(self.clone())
    }
}

impl<
        G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
        R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
        K: KindHelper<G, R> + Eq + Clone + PartialEq + Hash,
    > GraphAPI<K, G, R> for Graph<K, G, R>
{
    /// Create empty block
    fn empty_block(&mut self) -> BlockId {
        let block = Block::new(self);
        let id = block.id.clone();
        self.blocks.insert(id.to_usize(), block);
        return id;
    }

    /// Create empty block and initialize it in the block
    fn block(&mut self, body: &fn(b: &mut BlockBuilder<K, G, R>)) -> BlockId {
        let block = Block::new(self);
        let id = block.id.clone();
        self.blocks.insert(id.to_usize(), block);

        // Execute body
        self.with_block(id.clone(), body);

        return id;
    }

    /// Create phi value
    fn phi(&mut self, group: G) -> InstrId {
        let res = Instruction::new(self, Phi(group), VecDeque::new());
        // Prevent adding phi to block
        self.get_mut_instr(&res).added = true;
        self.phis.push_back(res);
        return res;
    }

    /// Perform operations on block
    fn with_block(&mut self, id: BlockId, body: &fn(b: &mut BlockBuilder<K, G, R>)) {
        let mut b = BlockBuilder {
            graph: self,
            block: id,
        };
        body(&mut b);
    }

    /// Create new instruction outside the block
    fn new_instr(&mut self, kind: K, args: VecDeque<InstrId>) -> InstrId {
        return Instruction::new(self, User(kind), args);
    }

    /// Set graph's root block
    fn set_root(&mut self, id: BlockId) {
        self.root = Some(id);
    }
}

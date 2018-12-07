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

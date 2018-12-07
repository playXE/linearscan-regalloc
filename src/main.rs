extern crate re;

use re::allocator::*;
use re::api::*;
use re::flatten::*;
use re::gap::*;
use re::graph::*;
use re::liveness::*;

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Kind {
    Increment,
    Sum,
    MultAdd,
    BranchIfBigger,
    JustUse,
    FixedUse,
    Nop,
    Print,
    Iconst(i32),
    ToDouble,
    Return,
    le,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Group {
    Normal,
    Double,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Register {
    rax,
    rbx,
    rcx,
    rdx,
    r9,
    r10,
    r11,
    r13,
}

use std::collections::VecDeque;

use self::Register::*;

impl GroupHelper<Register> for Group {
    fn groups() -> VecDeque<Self> {
        VecDeque::from(vec![Group::Normal, Group::Double])
    }

    fn registers(&self) -> VecDeque<Register> {
        match *self {
            Group::Normal => VecDeque::from(vec![rax, rbx, rcx, rdx, r9, r10, r11, r13]),
            _ => unimplemented!(),
        }
    }

    fn to_usize(&self) -> usize {
        self.clone() as usize
    }

    fn from_usize(u: usize) -> Self {
        match u {
            0 => Group::Normal,
            1 => Group::Double,
            _ => unreachable!(),
        }
    }
}

fn main() {
    println!("Hello, world!");
}

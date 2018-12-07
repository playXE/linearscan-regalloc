pub use self::GapActionKind::*;
pub use self::InstrKind::*;
pub use self::UseKind::*;
pub use self::Value::*;
use crate::api::*;
use crate::BitvSet;
use crate::SmallIntMap;
use std::hash::Hash;
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub usize);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstrId(pub usize);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntervalId(pub usize);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackId(pub usize);

pub trait GraphId {
    fn to_usize(&self) -> usize;
}

macro_rules! impl_id {
    ($t:tt) => {
        impl $crate::graph::GraphId for $t {
            fn to_usize(&self) -> usize {
                self.0
            }
        }
    };
}

impl_id!(InstrId);
impl_id!(StackId);
impl_id!(BlockId);
impl_id!(IntervalId);
use std::collections::VecDeque;

#[derive(Clone)]
pub struct Graph<
    K: Eq + Clone + PartialEq + Hash,
    G: Eq + Clone + PartialEq + Hash,
    R: Eq + Clone + PartialEq + Hash,
> {
    pub root: Option<BlockId>,
    pub block_id: usize,
    pub instr_id: usize,
    pub interval_id: usize,
    pub intervals: SmallIntMap<Interval<G, R>>,
    pub blocks: SmallIntMap<Block>,
    pub instructions: SmallIntMap<Instruction<K, G, R>>,
    pub phis: VecDeque<InstrId>,
    pub gaps: SmallIntMap<GapState>,
    pub prepared: bool,
    pub physical: SmallIntMap<SmallIntMap<IntervalId>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub id: BlockId,
    pub instructions: VecDeque<InstrId>,
    pub successors: VecDeque<BlockId>,
    pub predecessors: VecDeque<BlockId>,
    pub loop_index: usize,
    pub loop_depth: usize,
    pub incoming_forward_branches: usize,

    pub live_gen: BitvSet,
    pub live_kill: BitvSet,
    pub live_in: BitvSet,
    pub live_out: BitvSet,
    pub ended: bool,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Instruction<
    K: Eq + Clone + PartialEq + Hash,
    G: Eq + Clone + PartialEq + Hash,
    R: Eq + Clone + PartialEq + Hash,
> {
    pub id: InstrId,
    pub block: BlockId,
    pub kind: InstrKind<K, G>,
    pub output: Option<IntervalId>,
    pub inputs: VecDeque<InstrId>,
    pub temporary: VecDeque<IntervalId>,
    pub added: bool,
    _marker: ::std::marker::PhantomData<R>,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum InstrKind<K: Eq + Clone + PartialEq + Hash, G: Eq + Clone + PartialEq + Hash> {
    User(K),
    Gap,
    Phi(G),
    ToPhi(G),
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Interval<G: Eq + Clone + PartialEq + Hash, R: Eq + Clone + PartialEq + Hash> {
    pub id: IntervalId,
    pub value: Value<G, R>,
    pub hint: Option<IntervalId>,
    pub ranges: VecDeque<LiveRange>,
    pub parent: Option<IntervalId>,
    pub uses: VecDeque<Use<G, R>>,
    pub children: VecDeque<IntervalId>,
    pub fixed: bool,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum Value<G, R> {
    VirtualVal(G),
    RegisterVal(R),
    StackVal(G, StackId),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Use<G: Eq + Clone + PartialEq, R: Eq + Clone + PartialEq> {
    kind: UseKind<G, R>,
    pos: InstrId,
}

#[derive(Eq, Clone, PartialEq, Hash)]
pub enum UseKind<G: Eq + Clone + PartialEq, R: PartialEq + Eq + Clone> {
    UseAny(G),
    UseRegister(G),
    UseFixed(R),
    Invalid,
}

#[derive(Eq, Clone, PartialEq, Hash)]
pub struct LiveRange {
    start: InstrId,
    end: InstrId,
}

#[derive(Hash, Clone, Eq, PartialEq)]
pub struct GapState {
    pub actions: VecDeque<GapAction>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum GapActionKind {
    Move,
    Swap,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct GapAction {
    pub kind: GapActionKind,
    pub from: IntervalId,
    pub to: IntervalId,
}

impl<
        G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
        R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
        K: KindHelper<G, R> + Eq + Clone + PartialEq + Hash,
    > Graph<K, G, R>
{
    pub fn new() -> Graph<K, G, R> {
        Graph {
            root: None,
            block_id: 0,
            instr_id: 0,
            interval_id: 0,
            intervals: SmallIntMap::new(),
            blocks: SmallIntMap::new(),
            gaps: SmallIntMap::new(),
            phis: VecDeque::new(),
            instructions: SmallIntMap::new(),
            physical: SmallIntMap::new(),
            prepared: false,
        }
    }

    pub fn create_gap(&mut self, block: &BlockId) -> Instruction<K, G, R> {
        let id = self.instr_id();
        Instruction {
            id,
            block: block.clone(),
            kind: Gap,
            output: None,
            inputs: VecDeque::new(),
            temporary: VecDeque::new(),
            added: true,
            _marker: ::std::marker::PhantomData,
        }
    }

    #[inline(always)]
    pub fn instr_id(&mut self) -> InstrId {
        let r = self.instr_id;
        self.instr_id += 1;
        return InstrId(r);
    }

    pub fn get_mut_block<'r>(&'r mut self, id: &BlockId) -> &'r mut Block {
        self.blocks.find_mut(&id.to_usize()).unwrap()
    }

    pub fn get_block<'r>(&'r self, id: &BlockId) -> &'r Block {
        self.blocks.find(&id.to_usize()).unwrap()
    }

    pub fn get_block_list(&self) -> VecDeque<BlockId> {
        let mut blocks = VecDeque::new();

        for block in self.blocks.iter() {
            if block.is_some() {
                blocks.push_back(block.clone().unwrap().id);
            }
        }

        blocks
    }
    pub fn get_mut_instr<'r>(&'r mut self, id: &InstrId) -> &'r mut Instruction<K, G, R> {
        self.instructions.find_mut(&id.to_usize()).unwrap()
    }

    pub fn get_instr<'r>(&'r self, id: &InstrId) -> &'r Instruction<K, G, R> {
        self.instructions.find(&id.to_usize()).unwrap()
    }

    pub fn get_output(&self, id: &InstrId) -> IntervalId {
        self.instructions
            .get(&id.to_usize())
            .clone()
            .output
            .expect("Instruciton output")
    }

    pub fn get_mut_interval<'r>(&'r mut self, id: &IntervalId) -> &'r mut Interval<G, R> {
        self.intervals.find_mut(&id.to_usize()).unwrap()
    }

    pub fn get_interval<'r>(&'r self, id: &IntervalId) -> &'r Interval<G, R> {
        self.intervals.get(&id.to_usize())
    }

    pub fn get_mut_gap<'r>(&'r mut self, id: &InstrId) -> &'r mut GapState {
        if !self.gaps.contains_key(&id.to_usize()) {
            self.gaps.insert(
                id.to_usize(),
                GapState {
                    actions: VecDeque::new(),
                },
            );
        }
        self.gaps.find_mut(&id.to_usize()).unwrap()
    }

    pub fn interval_id(&mut self) -> IntervalId {
        let r = self.interval_id;
        self.interval_id += 1;
        return IntervalId(r);
    }
}

impl<
        G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
        R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
        K: KindHelper<G, R> + Eq + Clone + PartialEq + Hash,
    > Instruction<K, G, R>
{
    /// Create instruction without output interval
    pub fn new_empty(
        graph: &mut Graph<K, G, R>,
        kind: InstrKind<K, G>,
        args: VecDeque<InstrId>,
    ) -> InstrId {
        let id = graph.instr_id();

        let mut temporary = VecDeque::new();
        for group in kind.temporary().iter() {
            temporary.push_back(Interval::new(graph, group.clone()));
        }

        let r = Instruction {
            id: id.clone(),
            block: BlockId(0), // NOTE: this will be overwritten soon
            kind: kind,
            output: None,
            inputs: args.clone(),
            temporary: temporary,
            added: false,
            _marker: ::std::marker::PhantomData,
        };
        graph.instructions.insert(r.id.to_usize(), r);
        return id;
    }

    /// Create instruction with output
    pub fn new(
        graph: &mut Graph<K, G, R>,
        kind: InstrKind<K, G>,
        args: VecDeque<InstrId>,
    ) -> InstrId {
        let output = match kind.result_kind() {
            Some(k) => Some(Interval::new(graph, k.group())),
            None => None,
        };

        let instr = Instruction::new_empty(graph, kind, args);
        graph.get_mut_instr(&instr).output = output;
        return instr;
    }
}

impl<
        G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
        R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
    > Interval<G, R>
{
    /// Create new virtual interval
    pub fn new<K: KindHelper<G, R>>(graph: &mut Graph<K, G, R>, group: G) -> IntervalId {
        let r = Interval {
            id: graph.interval_id(),
            value: VirtualVal(group),
            hint: None,
            ranges: VecDeque::new(),
            parent: None,
            uses: VecDeque::new(),
            children: VecDeque::new(),
            fixed: false,
        };
        let id = r.clone().id;
        graph.intervals.insert(r.clone().id.to_usize(), r);
        return id;
    }

    /// Add range to interval's live range list.
    /// NOTE: Ranges are ordered by start position
    pub fn add_range(&mut self, start: InstrId, end: InstrId) {
        assert!(self.ranges.len() == 0 || self.ranges.back().unwrap().start >= end);

        // Extend last range
        if self.ranges.len() > 0 && self.ranges[0].start == end {
            self.ranges[0].start = start;
        } else {
            // Insert new range
            self.ranges.push_front(LiveRange {
                start: start,
                end: end,
            });
        }
    }

    /// Return mutable first range
    pub fn first_range<'r>(&'r mut self) -> &'r mut LiveRange {
        assert!(self.ranges.len() != 0);
        return &mut self.ranges[0];
    }

    /// Return interval's start position
    pub fn start(&self) -> InstrId {
        assert!(self.ranges.len() != 0);
        return self.ranges[0].clone().start;
    }

    /// Return interval's end position
    pub fn end(&self) -> InstrId {
        assert!(self.ranges.len() != 0);
        return self.ranges.back().clone().unwrap().end.clone();
    }

    /// Return true if one of the ranges contains `pos`
    pub fn covers(&self, pos: InstrId) -> bool {
        for range in self.ranges.iter() {
            if range.covers(pos.clone()) {
                return true;
            }
        }

        return false;
    }

    /// Add use to the interval's use list.
    /// NOTE: uses are ordered by increasing `pos`
    pub fn add_use(&mut self, kind: UseKind<G, R>, pos: InstrId) {
        assert!(
            self.uses.len() == 0
                || self.uses[0].pos > pos
                || self.uses[0].kind.group() == kind.group()
        );
        self.uses.push_front(Use {
            kind: kind,
            pos: pos,
        });
    }

    /// Return next UseFixed(...) after `after` position.
    pub fn next_fixed_use(&self, after: InstrId) -> Option<Use<G, R>> {
        for u in self.uses.iter() {
            match u.kind {
                UseFixed(_) if u.pos >= after => {
                    return Some(u.clone());
                }
                _ => (),
            }
        }
        return None;
    }

    /// Return next UseFixed(...) or UseRegister after `after` position.
    pub fn next_use(&self, after: InstrId) -> Option<Use<G, R>> {
        for u in self.uses.iter() {
            if u.pos >= after && !u.kind.is_any() {
                return Some(u.clone());
            }
        }
        return None;
    }

    /// Return last UseFixed(...) or UseRegister before `before` position
    pub fn last_use(&self, before: InstrId) -> Option<Use<G, R>> {
        for u in self.uses.iter() {
            if u.pos <= before && !u.kind.is_any() {
                return Some(u.clone());
            }
        }
        return None;
    }
}

impl<
        G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
        R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
        K: KindHelper<G, R> + Eq + Clone + PartialEq + Hash,
    > KindHelper<G, R> for InstrKind<K, G>
{
    /// Return true if instruction is clobbering registers
    fn clobbers(&self, group: &G) -> bool {
        match self {
            &User(ref k) => k.clobbers(group),
            &Gap => false,
            &ToPhi(_) => false,
            &Phi(_) => false,
        }
    }

    /// Return count of instruction's temporary operands
    fn temporary(&self) -> VecDeque<G> {
        match self {
            &User(ref k) => k.temporary(),
            &Gap => VecDeque::new(),
            &Phi(_) => VecDeque::new(),
            &ToPhi(_) => VecDeque::new(),
        }
    }

    /// Return use kind of instruction's `i`th input
    fn use_kind(&self, i: usize) -> UseKind<G, R> {
        match self {
            &User(ref k) => k.use_kind(i),
            &Gap => panic!("Gap can't have any input"),
            &Phi(ref g) => UseAny(g.clone()),
            &ToPhi(ref g) => UseAny(g.clone()),
        }
    }

    /// Return result kind of instruction or None, if instruction has no result
    fn result_kind(&self) -> Option<UseKind<G, R>> {
        match self {
            &User(ref k) => k.result_kind(),
            &Gap => None,
            &Phi(ref g) => Some(UseAny(g.clone())),
            &ToPhi(ref g) => Some(UseAny(g.clone())),
        }
    }
}

impl LiveRange {
    /// Return true if range contains position
    pub fn covers(&self, pos: InstrId) -> bool {
        return self.start <= pos && pos < self.end;
    }

    /// Return first intersection position of two ranges
    pub fn get_intersection(&self, other: &LiveRange) -> Option<InstrId> {
        if self.covers(other.clone().start) {
            return Some(other.clone().start);
        } else if other.start < self.start && self.start < other.end {
            return Some(self.start.clone());
        }
        return None;
    }
}

impl<
        G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
        R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
    > UseKind<G, R>
{
    pub fn is_fixed(&self) -> bool {
        match self {
            &UseFixed(_) => true,
            _ => false,
        }
    }

    pub fn is_any(&self) -> bool {
        match self {
            &UseAny(_) => true,
            _ => false,
        }
    }

    pub fn group(&self) -> G {
        match self {
            &UseRegister(ref g) => g.clone(),
            &UseAny(ref g) => g.clone(),
            &UseFixed(ref r) => r.group(),
            &Invalid => unreachable!(),
        }
    }
}

impl GapState {
    pub fn add_move(&mut self, from: &IntervalId, to: &IntervalId) {
        self.actions.push_back(GapAction {
            kind: Move,
            from: from.clone(),
            to: to.clone(),
        })
    }
}

impl Block {
    pub fn start(&self) -> InstrId {
        assert!(self.instructions.len() != 0);
        return self.instructions[0].clone();
    }
}

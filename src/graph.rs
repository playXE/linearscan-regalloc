pub use self::GapActionKind::*;
pub use self::InstrKind::*;
pub use self::UseKind::*;
pub use self::Value::*;
use crate::api::*;
use crate::gap::*;
use crate::BitvSet;
use crate::SmallIntMap;
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub usize);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub struct InstrId(pub usize);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub struct IntervalId(pub usize);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackId(pub usize);

impl InstrId {
    pub fn next(&self) -> InstrId {
        InstrId(self.0 + 1)
    }

    pub fn prev(&self) -> InstrId {
        InstrId(self.0 - 1)
    }
}

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

static mut RET: Option<IntervalId> = None;

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
    pub kind: UseKind<G, R>,
    pub pos: InstrId,
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
    pub start: InstrId,
    pub end: InstrId,
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

impl Block {
    /// Create new empty block
    pub fn new<
        G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
        R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
        K: KindHelper<G, R> + Eq + Clone + PartialEq + Hash,
    >(
        graph: &mut Graph<K, G, R>,
    ) -> Block {
        Block {
            id: graph.block_id(),
            instructions: VecDeque::new(),
            successors: VecDeque::new(),
            predecessors: VecDeque::new(),
            loop_index: 0,
            loop_depth: 0,
            incoming_forward_branches: 0,
            live_gen: BitvSet::new(),
            live_kill: BitvSet::new(),
            live_in: BitvSet::new(),
            live_out: BitvSet::new(),
            ended: false,
        }
    }

    pub fn add_successor<'r>(&'r mut self, succ: BlockId) -> &'r mut Block {
        assert!(self.successors.len() <= 2);
        self.successors.push_back(succ);
        return self;
    }

    pub fn add_predecessor(&mut self, pred: BlockId) {
        assert!(self.predecessors.len() <= 2);
        self.predecessors.push_back(pred);
        // NOTE: we'll decrease them later in flatten.rs
        self.incoming_forward_branches += 1;
    }
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

    fn block_id(&mut self) -> BlockId {
        let r = self.block_id;
        self.block_id += 1;
        return BlockId(r);
    }

    pub fn block_boundary(&self, pos: InstrId) -> bool {
        let block = self.get_block(&self.get_instr(&pos).block);
        return block.start() == pos || block.end() == pos;
    }

    /// Find optimal split position between two instructions
    pub fn optimal_split_pos(&self, group: &G, start: InstrId, end: InstrId) -> InstrId {
        // Fast and unfortunate case
        if start == end {
            return end;
        }

        let mut best_pos = end.clone();
        let mut best_depth = ::std::usize::MAX;
        for block in self.blocks.iter() {
            let block = block.clone().unwrap();
            if best_depth >= block.loop_depth {
                let block_to = block.end();

                // Choose the most shallow block
                if start < block_to && block_to <= end {
                    best_pos = block_to;
                    best_depth = block.loop_depth;
                }
            }
        }

        // Always split at gap
        if !self.is_gap(&best_pos) && !self.clobbers(group, &best_pos) {
            assert!(best_pos.to_usize() >= start.next().to_usize());
            best_pos = best_pos.prev();
        }
        assert!(start < best_pos && best_pos <= end);
        return best_pos;
    }

    pub fn get_intersection(&self, a: &IntervalId, b: &IntervalId) -> Option<InstrId> {
        let int_a = self.get_interval(a);
        let int_b = self.get_interval(b);

        for a in int_a.ranges.iter() {
            for b in int_b.ranges.iter() {
                match a.get_intersection(b) {
                    Some(pos) => return Some(pos),
                    _ => (),
                }
            }
        }

        return None;
    }

    pub fn split_at(&mut self, id: &IntervalId, pos: InstrId) -> IntervalId {
        assert!(self.get_interval(id).start() < pos);

        let group = self.get_interval(id).value.group();
        assert!(self.is_gap(&pos) || self.clobbers(&group, &pos));
        let child = Interval::new(self, group.clone());
        let parent = match &self.get_interval(id).parent {
            Some(parent) => parent.clone(),
            None => id.clone(),
        };
        let mut split_parent = parent.clone();
        if !self.get_interval(&split_parent).covers(pos.clone()) {
            for child in self.get_interval(&split_parent).children.iter() {
                if self.get_interval(child).covers(pos.clone()) {
                    split_parent = child.clone();
                }
            }
        }

        let split_at_call = self.clobbers(&group, &pos);
        if split_at_call || self.block_boundary(pos.clone()) {
            self.get_mut_gap(&pos).add_move(&split_parent, &child);
        }

        let mut child_ranges = VecDeque::new();

        let parent_ranges: VecDeque<Option<LiveRange>> = self
            .get_interval(&split_parent)
            .ranges
            .iter()
            .map(|range| {
                if range.end <= pos {
                    Some(range.clone())
                } else if range.start < pos {
                    child_ranges.push_back(LiveRange {
                        start: pos.clone(),
                        end: range.end.clone(),
                    });
                    Some(LiveRange {
                        start: range.start.clone(),
                        end: pos.clone(),
                    })
                } else {
                    child_ranges.push_back(range.clone());
                    None
                }
            })
            .collect();

        let parent_ranges = {
            let mut temp = VecDeque::new();
            for v in parent_ranges.iter() {
                if v.is_some() {
                    temp.push_back(v.clone().unwrap());
                }
            }
            temp
        };

        assert!(child_ranges.len() != 0);
        assert!(parent_ranges.len() != 0);
        self.get_mut_interval(&child).ranges = child_ranges;
        self.get_mut_interval(&split_parent).ranges = parent_ranges;
        self.get_mut_interval(&child).hint = Some(split_parent.clone());

        let mut child_uses = VecDeque::new();
        let split_on_call = self.get_instr(&pos).kind.clobbers(&group);
        let mut parent_uses = self.get_interval(&split_parent).uses.clone();

        parent_uses.retain(|u| {
            if split_at_call && u.pos <= pos || !split_on_call && u.pos < pos {
                true
            } else {
                child_uses.push_back(u.clone());
                false
            }
        });

        self.get_mut_interval(&child).uses = child_uses;
        self.get_mut_interval(&split_parent).uses = parent_uses.clone();
        let mut index = 0;
        for idx in self.get_interval(&parent).children.len()..0 {
            let child = &self.get_interval(&parent).children[idx];
            if self.get_interval(child).end() <= pos {
                index = idx + 1;
                break;
            }
        }

        self.get_mut_interval(&parent)
            .children
            .insert(index, child.clone());
        self.get_mut_interval(&child).parent = Some(parent);

        return child;
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

    pub fn iterate_children(&self, id: &IntervalId, f: &Fn(&Interval<G, R>) -> bool) -> bool {
        let p = self.get_interval(id);
        if !f(p) {
            return false;
        }

        for child_id in p.children.iter() {
            let child = self.get_interval(id);
            if !f(child) {
                break;
            };
        }

        true
    }
    //TODO: remove using static mut RET because it's a data race
    pub fn child_at(&self, parent: &IntervalId, pos: InstrId) -> Option<IntervalId> {
        unsafe { RET = None };

        self.iterate_children(parent, &|interval| {
            if interval.start() <= pos && pos < interval.end() {
                unsafe { RET = Some(interval.id.clone()) }
                false
            } else {
                true
            }
        });
        unsafe { RET.clone() }
    }

    pub fn child_with_use_at(&self, parent: &IntervalId, pos: InstrId) -> Option<IntervalId> {
        unsafe { RET = None };

        self.iterate_children(parent, &|interval| {
            if interval.start() <= pos
                && pos <= interval.end()
                && interval.uses.iter().any(|u| u.pos == pos)
            {
                unsafe { RET = Some(interval.id.clone()) }
                false
            } else {
                true
            }
        });

        // No match?
        unsafe { RET.clone() }
    }

    pub fn get_value(&self, i: &IntervalId, pos: InstrId) -> Option<Value<G, R>> {
        let child = self.child_with_use_at(i, pos);
        match child {
            Some(child) => Some(self.get_interval(&child).value.clone()),
            None => None,
        }
    }

    /// Return true if instruction at specified position is Gap
    pub fn is_gap(&self, pos: &InstrId) -> bool {
        match self.get_instr(pos).kind {
            Gap => true,
            _ => false,
        }
    }

    /// Return true if instruction at specified position contains
    /// register-clobbering call.
    pub fn clobbers(&self, group: &G, pos: &InstrId) -> bool {
        return self.get_instr(pos).kind.clobbers(group);
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
        for u in self.uses.iter().rev() {
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

    pub fn end(&self) -> InstrId {
        assert!(self.instructions.len() != 0);
        return self.instructions.back().unwrap().next();
    }
}

impl<G: GroupHelper<R> + Eq + Clone + PartialEq + Hash, R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash> Value<G, R> {
    pub fn is_virtual(&self) -> bool {
        match self {
            &VirtualVal(_) => true,
            _ => false,
        }
    }

    pub fn group(&self) -> G {
        match self {
            &VirtualVal(ref g) => g.clone(),
            &RegisterVal(ref r) => r.group(),
            &StackVal(ref g, _) => g.clone(),
        }
    }
}


impl<'a,
     G: GroupHelper<R> + Eq + Clone + PartialEq + Hash,
     R: RegisterHelper<G> + Eq + Clone + PartialEq + Hash,
     K: KindHelper<G, R> + Eq + Clone + PartialEq + Hash > BlockBuilder<'a, K, G, R> {
  /// add instruction to block
  pub fn add(&mut self, kind: K, args: VecDeque<InstrId>) -> InstrId {
    let instr_id = self.graph.new_instr(kind, args);

    self.add_existing(instr_id);

    return instr_id;
  }

  /// add existing instruction to block
  pub fn add_existing(&mut self, instr_id: InstrId) {
    assert!(!self.graph.get_instr(&instr_id).added);
    self.graph.get_mut_instr(&instr_id).added = true;
    self.graph.get_mut_instr(&instr_id).block = self.block.clone();

    let block = self.graph.get_mut_block(&self.block);
    assert!(!block.ended);
    block.instructions.push_back(instr_id);
  }

  /// add arg to existing instruction in block
  pub fn add_arg(&mut self, id: InstrId, arg: InstrId) {
    assert!(self.graph.get_instr(&id).block == self.block);
    self.graph.get_mut_instr(&id).inputs.push_back(arg);
  }

  /// add phi movement to block
  pub fn to_phi(&mut self, input: InstrId, phi: InstrId) {
    let group = match self.graph.get_instr(&phi).kind {
      Phi(ref group) => group.clone(),
      _ => panic!("Expected Phi argument")
    };
    let out = self.graph.get_instr(&phi).output.expect("Phi output");
    let inp = self.graph.get_instr(&input).output
                  .expect("Phi input output");

    // Insert one hint
    if self.graph.get_interval(&out).hint.is_none() {
      self.graph.get_mut_interval(&out).hint = Some(inp);
    }

    let res = Instruction::new_empty(self.graph, ToPhi(group), VecDeque::from(vec![input]));
    self.graph.get_mut_instr(&res).output = Some(out);
    self.add_existing(res);
    self.graph.get_mut_instr(&phi).inputs.push_back(res);
    assert!(self.graph.get_instr(&phi).inputs.len() <= 2);
  }

  /// end block
  pub fn end(&mut self) {
    let block = self.graph.get_mut_block(&self.block);
    assert!(!block.ended);
    assert!(block.instructions.len() > 0);
    block.ended = true;
  }

  /// add `target_id` to block's successors
  pub fn goto(&mut self, target_id: BlockId) {
    self.graph.get_mut_block(&self.block).add_successor(target_id.clone());
    self.graph.get_mut_block(&target_id).add_predecessor(self.block.clone());
    self.end();
  }

  /// add `left` and `right` to block's successors
  pub fn branch(&mut self, left: BlockId, right: BlockId) {
    self.graph.get_mut_block(&self.block).add_successor(left.clone())
                                         .add_successor(right.clone());
    self.graph.get_mut_block(&left).add_predecessor(self.block.clone());
    self.graph.get_mut_block(&right).add_predecessor(self.block.clone());
    self.end();
  }

  /// mark block as root
  pub fn make_root(&mut self) {
    self.graph.set_root(self.block.clone());
  }
}


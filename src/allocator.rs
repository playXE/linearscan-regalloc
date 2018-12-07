use crate::api::GroupAutoHelper;
use crate::api::*;
use crate::flatten::*;
use crate::gap::*;
use crate::graph::*;
use crate::liveness::Liveness;
use crate::*;
use quick_sort::sort_by;
use std::collections::VecDeque;
#[derive(Clone, Debug)]
pub struct AllocatorResult {
    pub spill_count: VecDeque<usize>,
}

#[derive(Clone, Debug)]
pub struct GroupResult {
    pub spill_count: usize,
}

#[derive(Clone)]
pub struct AllocatorState<
    G: GroupHelper<R> + Clone + PartialEq + Eq + Hash,
    R: RegisterHelper<G> + Clone + PartialEq + Eq + Hash,
> {
    pub group: G,
    pub register_count: usize,
    pub spill_count: usize,
    pub spills: VecDeque<Value<G, R>>,
    pub unhandled: VecDeque<IntervalId>,
    pub active: VecDeque<IntervalId>,
    pub inactive: VecDeque<IntervalId>,
}

impl<
        G: GroupHelper<R> + Clone + PartialEq + Eq + Hash,
        R: RegisterHelper<G> + Clone + PartialEq + Eq + Hash,
    > AllocatorState<G, R>
{
    fn get_spill(&mut self) -> Value<G, R> {
        return if self.spills.len() > 0 {
            self.spills.pop_front().unwrap().clone()
        } else {
            let slot = self.spill_count.clone();
            self.spill_count += 1;
            StackVal(self.group.clone(), StackId(slot))
        };
    }

    fn to_handled(&mut self, value: &Value<G, R>) {
        match value {
            &StackVal(ref group, ref slot) => {
                self.spills.push_back(StackVal(group.clone(), slot.clone()))
            }
            _ => (),
        }
    }
}

pub trait Allocator<
    G: GroupHelper<R> + Clone + PartialEq + Eq + Hash,
    R: RegisterHelper<G> + Clone + PartialEq + Eq + Hash,
    K: KindHelper<G, R> + Clone + PartialEq + Eq + Hash,
>
{
    fn prepare(&mut self);

    // Allocate registers
    fn allocate(&mut self) -> Result<AllocatorResult, String>;
}
use self::SplitConf::*;

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub enum SplitConf {
    Between(InstrId, InstrId),
    At(InstrId),
}
impl<
        G: GroupHelper<R> + Clone + PartialEq + Eq + Hash,
        R: RegisterHelper<G> + Clone + PartialEq + Eq + Hash,
        K: KindHelper<G, R> + Clone + PartialEq + Eq + Hash,
    > Allocator<G, R, K> for Graph<K, G, R>
{
    fn prepare(&mut self) {
        if self.prepared {
            return;
        }

        self.flatten();
        self.liveness_analysis();

        self.prepared = true;
    }

    fn allocate(&mut self) -> Result<AllocatorResult, String> {
        self.prepare();

        // Create physical fixed intervals
        let groups: VecDeque<G> = GroupHelper::groups();
        for group in groups.iter() {
            self.physical.insert(group.to_usize(), SmallIntMap::new());
            let regs = group.registers();
            for reg in regs.iter() {
                let interval = Interval::<G, R>::new::<K>(self, group.clone());
                self.get_mut_interval(&interval).value = RegisterVal(reg.clone());
                self.get_mut_interval(&interval).fixed = true;
                self.physical
                    .find_mut(&group.to_usize())
                    .unwrap()
                    .insert(reg.to_usize(), interval);
            }
        }

        let list = self.get_block_list();

        // Create live ranges
        match self.build_ranges(&list) {
            Ok(_) => {
                let mut results = VecDeque::new();
                // In each register group
                for group in groups.iter() {
                    // Walk intervals!
                    match self.walk_intervals(group) {
                        Ok(res) => {
                            results.push_back(res);
                        }
                        Err(reason) => {
                            return Err(reason);
                        }
                    }
                }

                // Add moves between blocks
                self.resolve_data_flow(&list);

                // Resolve parallel moves
                self.resolve_gaps();

                // Verify correctness of allocation
                self.verify();

                // Map results from each group to a general result
                return Ok(AllocatorResult {
                    spill_count: results.iter().map(|result| result.spill_count).collect(),
                });
            }
            Err(reason) => {
                return Err(reason);
            }
        };
    }
}

use std::iter::Iterator;

pub trait AllocatorHelper<
    G: GroupHelper<R> + Clone + PartialEq + Eq + Hash + GroupAutoHelper<R>,
    R: RegisterHelper<G> + Clone + PartialEq + Eq + Hash + RegisterAutoHelper<G>,
>
{
    fn walk_intervals(&mut self, group: &G) -> Result<GroupResult, String> {
        Err(format!("Unimplemented"))
    }

    // Try allocating free register
    fn allocate_free_reg<'r>(
        &'r mut self,
        current: IntervalId,
        state: &'r mut AllocatorState<G, R>,
    ) -> bool {
        false
    }
    // Allocate blocked register and spill others, or spill interval itself
    fn allocate_blocked_reg<'r>(
        &'r mut self,
        current: IntervalId,
        state: &'r mut AllocatorState<G, R>,
    ) -> Result<(), String> {
        Err(format!("Unimplemented"))
    }
    // Add movements on block edges
    fn resolve_data_flow(&mut self, list: &VecDeque<BlockId>) {}

    // Build live ranges for each interval
    fn build_ranges(&mut self, blocks: &VecDeque<BlockId>) -> Result<(), String> {
        Err(format!("Unimplemented"))
    }

    // Split intervals with fixed uses
    fn split_fixed(&mut self) {}

    //
    // Helpers
    //

    // Sort unhandled list (after insertion)
    fn sort_unhandled<'r>(&'r mut self, state: &'r mut AllocatorState<G, R>) {}

    // Get register hint if present
    fn get_hint(&mut self, current: IntervalId) -> Option<R> {
        None
    }

    // Split interval at some optimal position and add split child to unhandled
    fn split<'r>(
        &'r mut self,
        current: IntervalId,
        conf: SplitConf,
        state: &'r mut AllocatorState<G, R>,
    ) -> IntervalId {
        IntervalId(2555)
    }

    // Split and spill all intervals intersecting with current
    fn split_and_spill<'r>(&'r mut self, current: IntervalId, state: &'r mut AllocatorState<G, R>) {
    }

    // Iterate through all active intervals
    //fn iter_active<'r>(&'r self, state: &'r AllocatorState<G, R>)
    // -> Map<std::collections::vec_deque::Iter<'r,graph::IntervalId>>;
    /*
      // Iterate through all inactive intervals that are intersecting with current
      fn iter_intersecting<'r>(&'r self,
                               current: IntervalId,
                               state: &'r AllocatorState<G, R>)
          -> FilterMap<&IntervalId,
                                 (&IntervalId, &R, InstrId),
                                 VecIterator<IntervalId> >;
    */
    // Verify allocation results
    fn verify(&self) {}
}

impl<
        G: GroupHelper<R> + Clone + PartialEq + Eq + Hash + GroupAutoHelper<R>,
        R: RegisterHelper<G> + Clone + PartialEq + Eq + Hash + RegisterAutoHelper<G>,
        K: KindHelper<G, R> + Clone + PartialEq + Eq + Hash,
    > AllocatorHelper<G, R> for Graph<K, G, R>
{
    fn verify(&self) {}
    fn walk_intervals(&mut self, group: &G) -> Result<GroupResult, String> {
        let reg_count = group.registers().len();
        let mut state = AllocatorState {
            group: group.clone(),
            register_count: reg_count,
            spill_count: 0,
            spills: VecDeque::new(),
            unhandled: VecDeque::new(),
            active: VecDeque::new(),
            inactive: VecDeque::new(),
        };

        for interval in self.intervals.iter() {
            let interval = interval.clone().unwrap();
            if &interval.value.group() == &state.group && interval.ranges.len() > 0 {
                if interval.fixed {
                    state.active.push_back(interval.id);
                } else {
                    state.unhandled.push_back(interval.id);
                }
            }
        }

        self.sort_unhandled(&mut state);

        while state.unhandled.len() > 0 {
            let current = state.unhandled.pop_front().unwrap();
            let position = self.get_interval(&current).start();

            let mut handled = VecDeque::new();
            let mut queue = VecDeque::new();
            state.active.retain(|id| {
                if self.get_interval(&id.clone()).covers(position.clone()) {
                    true
                } else {
                    if position <= self.get_interval(id).end() {
                        queue.push_back(id.clone());
                    }
                    handled.push_back(self.get_interval(id).value.clone());
                    false
                }
            });
            state.active = queue;
            let mut queue = VecDeque::new();
            state.inactive.retain(|id| {
                if self.get_interval(&id.clone()).covers(position.clone()) {
                    queue.push_back(id.clone());
                    handled.push_back(self.get_interval(&id.clone()).value.clone());
                    false
                } else {
                    position < self.get_interval(id).end()
                }
            });

            state.active = queue;
            for v in handled.iter() {
                state.to_handled(v);
            }

            if self.get_interval(&current).value.is_virtual() {
                if !self.allocate_free_reg(current.clone(), &mut state) {
                    match self.allocate_blocked_reg(current.clone(), &mut state) {
                        Ok(_) => (),
                        Err(err) => return Err(err),
                    }
                }
            }

            match self.get_interval(&current).value {
                RegisterVal(_) => state.active.push_back(current),
                _ => (),
            }
        }

        return Ok(GroupResult {
            spill_count: state.spill_count,
        });
    }

    fn allocate_free_reg<'r>(
        &'r mut self,
        current: IntervalId,
        state: &'r mut AllocatorState<G, R>,
    ) -> bool {
        let mut free_pos = vec_from(state.register_count, ::std::u32::MAX as usize);
        let hint = self.get_hint(current.clone());
        let iter = state
            .active
            .iter()
            .map(|id| match self.get_interval(&id.clone()).value {
                RegisterVal(ref reg) => (id, reg),
                _ => panic!("Expected reg in active"),
            });

        for (_, reg) in iter {
            free_pos[reg.to_usize()] = 0;
        }
        let iter_i = state.inactive.iter().filter_map(|id| {
            match self.get_intersection(&id.clone(), &current) {
                Some(pos) => match self.get_interval(id).value {
                    RegisterVal(ref reg) => Some((id, reg, pos)),
                    _ => panic!("Expected reg in active"),
                },
                None => None,
            }
        });

        for (_, reg, pos) in iter_i {
            if free_pos[reg.to_usize()] > pos.to_usize() {
                free_pos[reg.to_usize()] = pos.to_usize();
            }
        }

        let mut reg = 0;
        let mut max_pos = InstrId(0);

        match self.get_interval(&current).next_fixed_use(InstrId(0)) {
            Some(u) => match u.kind {
                UseFixed(r) => {
                    reg = r.to_usize();
                    max_pos = InstrId(free_pos[reg]);
                }
                _ => panic!("Unexpected kind"),
            },
            None => {
                for (i, pos) in free_pos.iter().enumerate() {
                    if pos > &max_pos.to_usize() {
                        max_pos = InstrId(*pos);
                        reg = i;
                    }
                }
            }
        }

        if max_pos.to_usize() == 0 {
            return false;
        }

        let start = self.get_interval(&current).start();
        let end = self.get_interval(&current).end();

        if max_pos >= end {
            return true;
        } else if start.next() >= max_pos {
            return false;
        } else {
            assert!(max_pos < end);

            let mut split_pos = self.optimal_split_pos(&state.group, start, max_pos.clone());
            if split_pos == max_pos.prev() && self.clobbers(&state.group, &max_pos) {
                match self.get_interval(&current).next_use(max_pos.clone()) {
                    Some(ref u) if u.pos == max_pos => {
                        split_pos = max_pos;
                    }
                    _ => return false,
                }
            }

            let child = self.split(current.clone(), At(split_pos), state);
            match self.get_interval(&child).next_use(InstrId(0)) {
                None => {
                    self.get_mut_interval(&child).value = state.get_spill();
                }
                _ => (),
            }

            self.get_mut_interval(&current).value =
                RegisterVal(RegisterHelper::from_usize(&state.group, reg));

            return true;
        }
    }

    fn allocate_blocked_reg<'r>(
        &'r mut self,
        current: IntervalId,
        state: &'r mut AllocatorState<G, R>,
    ) -> Result<(), String> {
        let mut use_pos = vec_from(state.register_count, ::std::u32::MAX as usize);
        let mut block_pos = vec_from(state.register_count, ::std::u32::MAX as usize);

        let start = self.get_interval(&current).start();
        let hint = self.get_hint(current.clone());

        let iter = state
            .active
            .iter()
            .map(|id| match self.get_interval(&id.clone()).value {
                RegisterVal(ref reg) => (id, reg),
                _ => panic!("Expected reg in active"),
            });
        for (id, reg) in iter {
            let interval = self.get_interval(id);
            if !interval.fixed {
                let int_reg = reg.to_usize();
                match interval.next_use(start.clone()) {
                    Some(u) => {
                        if use_pos[int_reg] > u.pos.to_usize() {
                            use_pos[int_reg] = u.pos.to_usize();
                        }
                    }
                    None => (),
                }
            }
        }

        let iter_i = state.inactive.iter().filter_map(|id| {
            match self.get_intersection(&id.clone(), &current) {
                Some(pos) => match self.get_interval(id).value {
                    RegisterVal(ref reg) => Some((id, reg, pos)),
                    _ => panic!("Expected reg in active"),
                },
                None => None,
            }
        });

        for (id, reg, _) in iter_i {
            let interval = self.get_interval(id);
            if !interval.fixed {
                let int_reg = reg.to_usize();
                match interval.next_use(start.clone()) {
                    Some(u) => {
                        if use_pos[int_reg] > u.pos.to_usize() {
                            use_pos[int_reg] = u.pos.to_usize();
                        }
                    }
                    None => (),
                }
            }
        }

        let iter = state
            .active
            .iter()
            .map(|id| match self.get_interval(&id.clone()).value {
                RegisterVal(ref reg) => (id, reg),
                _ => panic!("Expected reg in active"),
            });

        for (id, reg) in iter {
            if self.get_interval(id).fixed {
                let int_reg = reg.to_usize();
                block_pos[int_reg] = 0;
                use_pos[int_reg] = 0;
            }
        }

        let iter_i = state.inactive.iter().filter_map(|id| {
            match self.get_intersection(&id.clone(), &current) {
                Some(pos) => match self.get_interval(id).value {
                    RegisterVal(ref reg) => Some((id, reg, pos)),
                    _ => panic!("Expected reg in active"),
                },
                None => None,
            }
        });

        for (id, reg, pos) in iter_i {
            if self.get_interval(id).fixed {
                let int_reg = reg.to_usize();
                let int_pos = pos.to_usize();
                block_pos[int_reg] = int_pos;
                if use_pos[int_reg] > int_pos {
                    use_pos[int_reg] = int_pos;
                }
            }
        }
        let mut reg = 0;
        let mut max_pos = 0;
        match self.get_interval(&current).next_fixed_use(InstrId(0)) {
            // Intervals with fixed use should have specific register
            Some(u) => match u.kind {
                UseFixed(r) => {
                    reg = r.to_usize();
                    max_pos = use_pos[reg];
                }
                _ => panic!("Unexpected use kind"),
            },

            // Other intervals should prefer register that isn't used for longer time
            None => {
                // Prefer hinted register
                match hint {
                    Some(hint) => {
                        for (i, &pos) in use_pos.iter().enumerate() {
                            if pos > max_pos || hint.to_usize() == i && pos == max_pos {
                                max_pos = pos;
                                reg = i;
                            }
                        }
                    }
                    None => {
                        for (i, &pos) in use_pos.iter().enumerate() {
                            if pos > max_pos {
                                max_pos = pos;
                                reg = i;
                            }
                        }
                    }
                }
            }
        }

        let first_use = self.get_interval(&current).next_use(InstrId(0));
        match first_use {
            Some(u) => {
                if max_pos < u.pos.to_usize() {
                    if u.pos == start {
                        return Err("Incorrect input, allocation impossible".into());
                    }

                    // Spill current itself
                    self.get_mut_interval(&current).value = state.get_spill();

                    // And split before first register use
                    self.split(current, Between(start, u.pos), state);
                } else {
                    // Assign register to current
                    self.get_mut_interval(&current).value =
                        RegisterVal(RegisterHelper::from_usize(&state.group, reg));

                    // If blocked somewhere before end by fixed interval
                    if block_pos[reg] <= self.get_interval(&current).end().to_usize() {
                        // Split before this position
                        self.split(
                            current.clone(),
                            Between(start, InstrId(block_pos[reg])),
                            state,
                        );
                    }

                    // Split and spill, active and intersecting inactive
                    self.split_and_spill(current, state);
                }
            }
            None => {
                // Spill current, it has no uses
                self.get_mut_interval(&current).value = state.get_spill();
            }
        }

        Ok(())
    }

    fn split<'r>(
        &'r mut self,
        current: IntervalId,
        conf: SplitConf,
        state: &'r mut AllocatorState<G, R>,
    ) -> IntervalId {
        let split_pos = match conf {
            Between(start, end) => self.optimal_split_pos(&state.group, start, end),
            At(pos) => pos,
        };

        let res = self.split_at(&current, split_pos);
        state.unhandled.push_back(res.clone());
        self.sort_unhandled(state);
        return res;
    }

    fn sort_unhandled<'r>(&'r mut self, state: &'r mut AllocatorState<G, R>) {
        // TODO(indutny) do sorted inserts and don't call this on every insertion,
        // it is really expensive!

        // Sort intervals in the order of increasing start position
        let (s1, s2) = state.unhandled.as_mut_slices();
        s1.sort_by(|left, right| {
            let lstart = self.get_interval(left).start();
            let rstart = self.get_interval(right).start();

            lstart.cmp(&rstart)
        });
        s2.sort_by(|left, right| {
            let lstart = self.get_interval(left).start();
            let rstart = self.get_interval(right).start();

            lstart.cmp(&rstart)
        })
    }

    fn get_hint(&mut self, current: IntervalId) -> Option<R> {
        match self.get_interval(&current).hint {
            Some(ref id) => match self.get_interval(id).value {
                RegisterVal(ref r) => {
                    assert!(r.group() == self.get_interval(&current).value.group());
                    Some(r.clone())
                }
                _ => None,
            },
            None => None,
        }
    }

    fn resolve_data_flow(&mut self, list: &VecDeque<BlockId>) {
        for block_id in list.iter() {
            let block_end = self.get_block(block_id).end().prev();
            let successors = self.get_block(block_id).successors.clone();
            for succ_id in successors.iter() {
                let succ_start = self.get_block(succ_id).start().clone();
                let live_in = self.get_block(succ_id).live_in.clone();

                for interval in live_in.iter() {
                    let interval_id = IntervalId(*interval);
                    let parent = match self.get_interval(&interval_id).parent {
                        Some(p) => p,
                        None => interval_id,
                    };

                    let from = self
                        .child_at(&parent, block_end.clone())
                        .expect("Interval should exist at pred end");
                    let to = self
                        .child_at(&parent, succ_start.clone())
                        .expect("Interval should exist at succ start");
                    if from != to {
                        let gap_pos = if successors.len() == 2 {
                            succ_start
                        } else {
                            block_end
                        };
                        self.get_mut_gap(&gap_pos).add_move(&from, &to);
                    }
                }
            }
        }
    }

    fn split_fixed(&mut self) {
        let mut list = VecDeque::new();
        for interval in self.intervals.iter() {
            let interval = interval.clone().unwrap();
            if interval.uses.iter().any(|u| u.kind.is_fixed()) {
                list.push_back(interval.id);
            }
        }
        for id in list.iter() {
            let cur = *id;

            let mut uses = self.get_interval(id).uses.clone();
            uses.retain(|u| u.kind.is_fixed());

            let mut i = 0;
            while i < uses.len() - 1 {
                // Split between each pair of uses
                let split_pos =
                    self.optimal_split_pos(&uses[i].kind.group(), uses[i].pos, uses[i + 1].pos);
                self.split_at(&cur, split_pos);

                i += 1;
            }
        }
    }

    fn build_ranges(&mut self, blocks: &VecDeque<BlockId>) -> Result<(), String> {
        let physical = self.physical.clone();
        for block_id in blocks.iter().rev() {
            let instructions = self.get_block(block_id).instructions.clone();
            let live_out = self.get_block(block_id).live_out.clone();
            let block_from = self.get_block(block_id).start();
            let block_to = self.get_block(block_id).end();

            // Assume that each live_out interval lives for the whole time of block
            // NOTE: we'll shorten it later if definition of this interval appears to
            // be in this block
            for &int_id in live_out.iter() {
                self.get_mut_interval(&IntervalId(int_id))
                    .add_range(block_from, block_to);
            }

            for &instr_id in instructions.iter().rev() {
                let instr = self.get_instr(&instr_id).clone();

                // Call instructions should swap out all used registers into stack slots
                let groups: VecDeque<G> = GroupHelper::groups();
                for group in groups.iter() {
                    self.physical.insert(group.to_usize(), SmallIntMap::new());
                    if instr.kind.clobbers(group) {
                        let regs = group.registers();
                        for reg in regs.iter() {
                            self.get_mut_interval(
                                physical.get(&group.to_usize()).get(&reg.to_usize()),
                            )
                            .add_range(instr_id, instr_id.next());
                        }
                    }
                }

                // Process output
                match instr.output {
                    Some(output) => {
                        // Call instructions are defining their value after the call
                        let group = self.get_interval(&output).value.group();
                        let pos = if instr.kind.clobbers(&group) {
                            instr_id.next()
                        } else {
                            instr_id
                        };

                        if self.get_interval(&output).ranges.len() != 0 {
                            // Shorten range if output outlives block, or is used anywhere
                            self.get_mut_interval(&output).first_range().start = pos;
                        } else {
                            // Add short range otherwise
                            self.get_mut_interval(&output).add_range(pos, pos.next());
                        }
                        let out_kind = instr.kind.result_kind().unwrap();
                        self.get_mut_interval(&output).add_use(out_kind, pos);
                    }
                    None => (),
                }

                // Process temporary
                for tmp in instr.temporary.iter() {
                    let group = self.get_interval(tmp).value.group();
                    if instr.kind.clobbers(&group) {
                        return Err("Call instruction can't have temporary registers".into());
                    }
                    self.get_mut_interval(tmp)
                        .add_range(instr_id, instr_id.next());
                    self.get_mut_interval(tmp)
                        .add_use(group.use_reg(), instr_id);
                }

                // Process inputs
                for (i, input_instr) in instr.inputs.iter().enumerate() {
                    let input = self.get_output(input_instr);
                    if !self.get_interval(&input).covers(instr_id) {
                        self.get_mut_interval(&input)
                            .add_range(block_from, instr_id);
                    }
                    let kind = instr.kind.use_kind(i);
                    self.get_mut_interval(&input).add_use(kind, instr_id);
                }
            }
        }

        // Now split all intervals with fixed uses
        self.split_fixed();

        return Ok(());
    }
}

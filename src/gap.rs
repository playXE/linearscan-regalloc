use self::MoveStatus::*;
use crate::api::*;
use crate::graph::*;
use crate::*;
use std::collections::VecDeque;
use std::hash::Hash;

pub fn vec_from<T: Clone>(size: usize, t: T) -> VecDeque<T> {
    let mut v = VecDeque::new();
    for _ in 0..size {
        v.push_back(t.clone());
    }
    v
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum MoveStatus {
    ToMove,
    Moving,
    Moved,
}

pub trait GapResolver {
    fn resolve_gaps(&mut self);
}

trait GapResolverHelper {
    fn resolve_gap(&mut self, id: &InstrId) -> GapState {
        unreachable!()
    }

    fn move_one(
        &mut self,
        actions: &VecDeque<GapAction>,
        i: usize,
        s: &mut VecDeque<MoveStatus>,
        result: &mut VecDeque<GapAction>,
    ) -> bool {
        false
    }
}

impl<
        G: GroupHelper<R> + Clone + PartialEq + Eq + Hash,
        R: RegisterHelper<G> + Clone + PartialEq + Eq + Hash,
        K: KindHelper<G, R> + Clone + PartialEq + Eq + Hash,
    > GapResolver for Graph<K,G,R> {
        fn resolve_gaps(&mut self) {
            let mut keys = VecDeque::new();
            for (id,_) in self.gaps.iter().enumerate() {
                keys.push_back(InstrId(id));
            }
            for id in keys.iter() {
                let state = self.resolve_gap(id);
                self.gaps.insert(id.to_usize(),state);
            }
        }
    }

impl<
        G: GroupHelper<R> + Clone + PartialEq + Eq + Hash,
        R: RegisterHelper<G> + Clone + PartialEq + Eq + Hash,
        K: KindHelper<G, R> + Clone + PartialEq + Eq + Hash,
    > GapResolverHelper for Graph<K, G, R>
{
    fn resolve_gap(&mut self, id: &InstrId) -> GapState {
        let state = self.gaps.pop(&id.to_usize()).unwrap();
        let mut status = vec_from(state.actions.len(), ToMove);

        let mut i = 0;
        let mut result = VecDeque::new();

        while i < state.actions.len() {
            if status[i] == ToMove {
                self.move_one(&state.actions, i, &mut status, &mut result);
            }
            i += 1;
        }
        GapState { actions: result }
    }

    fn move_one(
        &mut self,
        actions: &VecDeque<GapAction>,
        i: usize,
        s: &mut VecDeque<MoveStatus>,
        result: &mut VecDeque<GapAction>,
    ) -> bool {
        assert!(actions[i].kind == Move);
        let from = self.get_interval(&actions[i].from).value.clone();
        let to = self.get_interval(&actions[i].to).value.clone();
        if from == to {
            return false;
        }
        s[i] = Moving;
        let mut j = 0;
        let mut circular = false;
        let mut sentinel = false;
        while j < actions.len() {
            assert!(actions[j].kind == Move);
            let other_from = self.get_interval(&actions[j].from).value.clone();
            if other_from == to {
                match s[j].clone() {
                    ToMove => {
                        let r = self.move_one(actions, j, s, result);
                        if r {
                            assert!(!circular);
                            circular = true;
                        }
                    }
                    Moving => {
                        sentinel = true;
                    }
                    Moved => (),
                }
            }
            j += 1
        }

        if circular {
            result.push_back(GapAction {
                kind: Swap,
                from: actions[i].from.clone(),
                to: actions[i].to.clone(),
            });
        } else if !sentinel {
            result.push_back(actions[i].clone());
        }

        return circular || sentinel;
    }
}

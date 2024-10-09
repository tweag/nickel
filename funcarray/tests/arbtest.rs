use std::collections::VecDeque;

use arbitrary::Unstructured;
use arbtest::{arbitrary, arbtest};
use nickel_lang_funcarray::{FunctionalArray, Vector};

#[derive(arbitrary::Arbitrary, Debug)]
enum Op {
    Push(u32),
    Pop,
    Extend(Vec<u32>),
    Clone,
    Truncate(u32),
}

impl Op {
    fn apply_to_vec(&self, vec: &mut Vec<u32>) {
        match self {
            Op::Push(x) => vec.push(*x),
            Op::Pop => {
                vec.pop();
            }
            Op::Extend(xs) => vec.extend_from_slice(xs),
            Op::Clone => {}
            Op::Truncate(len) => {
                if !vec.is_empty() {
                    vec.truncate(*len as usize % vec.len())
                }
            }
        }
    }

    fn apply_to_vector<const N: usize>(
        &self,
        vec: &mut Vector<u32, N>,
        arena: &mut Vec<Vector<u32, N>>,
    ) {
        match self {
            Op::Push(x) => vec.push(*x),
            Op::Pop => {
                vec.pop();
            }
            Op::Extend(xs) => vec.extend(xs.iter().copied()),
            Op::Clone => {
                arena.push(vec.clone());
            }
            Op::Truncate(len) => {
                if !vec.is_empty() {
                    vec.truncate(*len as usize % vec.len())
                }
            }
        }
    }
}

#[derive(arbitrary::Arbitrary, Debug)]
enum ArrayOp {
    PushFront(u32),
    PopFront,
    Prepend(Vec<u32>),
    Slice(u32, usize),
}

impl ArrayOp {
    fn apply_to_vec_deque(&self, vec: &mut VecDeque<u32>) {
        match self {
            ArrayOp::PushFront(x) => vec.push_front(*x),
            ArrayOp::PopFront => {
                vec.pop_front();
            }
            ArrayOp::Prepend(xs) => {
                for x in xs.iter().rev() {
                    vec.push_front(*x);
                }
            }
            ArrayOp::Slice(start, len) => {
                if !vec.is_empty() {
                    let start = *start as usize % vec.len();
                    for _ in 0..start {
                        vec.pop_front();
                    }
                    vec.truncate(*len);
                }
            }
        }
    }

    fn apply_to_array<const N: usize>(&self, vec: &mut FunctionalArray<u32, N>) {
        match self {
            ArrayOp::PushFront(x) => vec.push_front(*x),
            ArrayOp::PopFront => {
                vec.pop_front();
            }
            ArrayOp::Prepend(xs) => {
                let other = FunctionalArray::collect(xs.iter().cloned());
                vec.prepend(other);
            }
            ArrayOp::Slice(start, len) => {
                if !vec.is_empty() {
                    let start = *start as usize % vec.len();
                    let start = start.min(vec.len());
                    let end = (start + len).min(vec.len());
                    vec.slice(start, end);
                }
            }
        }
    }
}

// u.arbitrary() generates very short vecs by default:
// https://github.com/matklad/arbtest/issues/8
fn arb_vec(u: &mut Unstructured<'_>) -> arbitrary::Result<Vec<u32>> {
    let len = u.arbitrary_len::<u32>()?;
    std::iter::from_fn(|| Some(u.arbitrary::<u32>()))
        .take(len)
        .collect()
}

#[test]
fn mutations() {
    arbtest(|u| {
        let mut vec: Vec<u32> = arb_vec(u)?;
        let mut vector: Vector<u32, 4> = vec.iter().copied().collect();
        let mut arena = Vec::new();
        let ops: Vec<Op> = u.arbitrary()?;

        for op in ops {
            op.apply_to_vec(&mut vec);
            op.apply_to_vector(&mut vector, &mut arena);

            vector.check_invariants();

            assert_eq!(vec, vector.iter().cloned().collect::<Vec<_>>());
        }

        Ok(())
    });
}

#[test]
fn rev_iter() {
    arbtest(|u| {
        let mut vec: Vec<u32> = arb_vec(u)?;
        let vector: Vector<u32, 4> = vec.iter().copied().collect();

        let reversed: Vec<u32> = vector.rev_iter().copied().collect();
        vec.reverse();
        assert_eq!(reversed, vec);

        Ok(())
    });
}

#[test]
fn rev_iter_starting_at() {
    arbtest(|u| {
        let mut vec: Vec<u32> = arb_vec(u)?;
        if !vec.is_empty() {
            let vector: Vector<u32, 4> = vec.iter().copied().collect();
            let idx: usize = u.arbitrary()?;
            let idx = idx % vec.len();

            let reversed: Vec<u32> = vector.rev_iter_starting_at(idx).copied().collect();
            vec.truncate(idx + 1);
            vec.reverse();
            assert_eq!(reversed, vec);
        }

        Ok(())
    });
}

#[test]
fn rev_into_iter() {
    arbtest(|u| {
        let mut vec: Vec<u32> = arb_vec(u)?;
        let vector: Vector<u32, 4> = vec.iter().copied().collect();

        let reversed: Vec<u32> = vector.into_rev_iter().collect();
        vec.reverse();
        assert_eq!(reversed, vec);

        Ok(())
    });
}

#[test]
fn array_mutations() {
    arbtest(|u| {
        let vec: Vec<u32> = arb_vec(u)?;
        let mut vec: VecDeque<u32> = vec.into_iter().collect();
        let mut arr: FunctionalArray<u32, 4> = FunctionalArray::collect(vec.iter().copied());
        let ops: Vec<ArrayOp> = u.arbitrary()?;

        assert_eq!(vec, arr.iter().cloned().collect::<Vec<_>>());

        for op in ops {
            op.apply_to_vec_deque(&mut vec);
            op.apply_to_array(&mut arr);

            assert_eq!(vec, arr.iter().cloned().collect::<Vec<_>>());
            assert_eq!(vec, arr.clone().into_iter().collect::<Vec<_>>());
        }

        Ok(())
    });
}

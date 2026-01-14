use arbitrary::Unstructured;
use arbtest::{arbitrary, arbtest};
use nickel_lang_vector::{Const, Slice, ValidBranchingConstant, Vector};

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
    ) where
        Const<N>: ValidBranchingConstant,
    {
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
    Push(u32),
    Pop,
    Extend(Vec<u32>),
    Slice(u32, usize),
    // Overwrite(v, len) overwrites the first `len` entries
    // with the value v.
    Overwrite(u32, usize),
}

impl ArrayOp {
    fn apply_to_vec(&self, vec: &mut Vec<u32>) {
        match self {
            ArrayOp::Push(x) => vec.push(*x),
            ArrayOp::Pop => {
                vec.pop();
            }
            ArrayOp::Extend(xs) => {
                vec.extend(xs.iter().copied());
            }
            ArrayOp::Slice(start, len) => {
                if !vec.is_empty() {
                    let start = *start as usize % vec.len();
                    vec.drain(0..start);
                    vec.truncate(*len);
                }
            }
            ArrayOp::Overwrite(v, len) => {
                if !vec.is_empty() {
                    let len = len % vec.len();
                    for elt in vec.iter_mut().take(len) {
                        *elt = *v;
                    }
                }
            }
        }
    }

    fn apply_to_slice<const N: usize>(&self, vec: &mut Slice<u32, N>)
    where
        Const<N>: ValidBranchingConstant,
    {
        match self {
            ArrayOp::Push(x) => vec.push(*x),
            ArrayOp::Pop => {
                vec.pop();
            }
            ArrayOp::Extend(xs) => {
                vec.extend(xs.iter().copied());
            }
            ArrayOp::Slice(start, len) => {
                if !vec.is_empty() {
                    let start = *start as usize % vec.len();
                    let start = start.min(vec.len());
                    let end = (start + len).min(vec.len());
                    vec.slice(start, end);
                }
            }
            ArrayOp::Overwrite(v, len) => {
                if !vec.is_empty() {
                    let len = len % vec.len();
                    for elt in vec.iter_mut().take(len) {
                        *elt = *v;
                    }
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
fn iter_starting_at() {
    arbtest(|u| {
        let mut vec: Vec<u32> = arb_vec(u)?;
        if !vec.is_empty() {
            let vector: Vector<u32, 4> = vec.iter().copied().collect();
            let idx: usize = u.arbitrary()?;
            let idx = idx % vec.len();

            let claimed_len = vector.iter_starting_at(idx).len();
            let result: Vec<u32> = vector.iter_starting_at(idx).copied().collect();
            let into_result: Vec<u32> = vector.into_iter_starting_at(idx).collect();
            vec.drain(..idx);
            assert_eq!(result, vec);
            assert_eq!(into_result, vec);
            assert_eq!(claimed_len, vec.len());
        }

        Ok(())
    });
}

#[test]
fn into_iter() {
    arbtest(|u| {
        let vec: Vec<u32> = arb_vec(u)?;
        let vector: Vector<u32, 4> = vec.iter().copied().collect();

        let result: Vec<u32> = vector.into_iter().collect();
        assert_eq!(result, vec);

        Ok(())
    });
}

#[test]
fn array_mutations() {
    arbtest(|u| {
        let vec: Vec<u32> = arb_vec(u)?;
        let mut vec: Vec<u32> = vec.into_iter().collect();
        let mut slice: Slice<u32, 4> = vec.iter().copied().collect();
        let ops: Vec<ArrayOp> = u.arbitrary()?;

        assert_eq!(vec, slice.iter().cloned().collect::<Vec<_>>());

        for op in ops {
            op.apply_to_vec(&mut vec);
            op.apply_to_slice(&mut slice);

            assert_eq!(vec, slice.iter().cloned().collect::<Vec<_>>());
            assert_eq!(vec, slice.clone().into_iter().collect::<Vec<_>>());
        }

        Ok(())
    });
}

#[test]
fn iter_mut() {
    arbtest(|u| {
        let mut vec: Vec<u32> = arb_vec(u)?;
        let other_vec: Vec<u32> = arb_vec(u)?;

        let mut vector: Vector<_, 32> = vec.iter().copied().collect();
        let len = u.arbitrary::<usize>()? % vec.len().max(1);

        for (place, val) in vec.iter_mut().take(len).zip(&other_vec) {
            *place = *val;
        }
        for (place, val) in vector.iter_mut().take(len).zip(&other_vec) {
            *place = *val;
        }

        assert_eq!(vec, vector.iter().cloned().collect::<Vec<_>>());

        Ok(())
    });
}

use std::collections::HashSet;

/// De-duplicate a vec without changing the order. The first instance of each unique
/// element will be kept.
pub fn dedup<T: std::hash::Hash + Eq + Clone>(xs: &mut Vec<T>) {
    let mut seen = HashSet::new();
    // Clone is needed because the signature of retain doesn't let us keep the reference.
    xs.retain(|x| seen.insert(x.clone()));
}

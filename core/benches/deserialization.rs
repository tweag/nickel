use criterion::{Criterion, criterion_group, criterion_main};
use nickel_lang_core::{bytecode::ast::AstAlloc, serialize::yaml::load_json};

pub fn json_ast_construction(c: &mut Criterion) {
    fn generate_json(depth: usize, branching: usize) -> serde_json::Value {
        if depth == 0 {
            serde_json::Value::String("hi".into())
        } else {
            let mut map = serde_json::Map::new();
            for i in 0..branching {
                map.insert(format!("elt_{i}"), generate_json(depth - 1, branching));
            }
            serde_json::Value::Object(map)
        }
    }

    let json = serde_json::to_string_pretty(&generate_json(5, 10)).unwrap();

    c.bench_function("load json depth 5, branching 10", |b| {
        b.iter(|| {
            load_json(&AstAlloc::new(), &json, None).unwrap();
        })
    });

    let json = serde_json::to_string_pretty(&generate_json(6, 10)).unwrap();

    c.bench_function("load json depth 6, branching 10", |b| {
        b.iter(|| {
            load_json(&AstAlloc::new(), &json, None).unwrap();
        })
    });
}

criterion_group!(benches, json_ast_construction);
criterion_main!(benches);

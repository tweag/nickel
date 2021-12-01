use std::{
    collections::HashMap,
    time::{Duration, Instant},
};

use anyhow::Result;
use serde_json::Value;

use self::param::Param;

struct TraceItem<T> {
    time: T,
    params: TraceItemParams,
}

#[derive(Debug, Default)]
pub struct TraceItemParams {
    method: String,
    linearization_size: Option<usize>,
}

struct Received(Instant);
struct Replied {
    ingress: Instant,
    duration: Duration,
    with_error: bool,
}

struct Trace {
    received: HashMap<usize, TraceItem<Received>>,
    replied: Vec<TraceItem<Replied>>,
}

impl Trace {
    pub fn receive(&mut self, id: usize, method: impl ToString) {
        let time = Received(Instant::now());
        let params = TraceItemParams {
            method: method.to_string().into(),
            ..Default::default()
        };

        self.received.insert(id, TraceItem { time, params });
    }

    pub fn reply(&mut self, id: usize) {
        let received = self.received.remove(&id);
        for TraceItem {
            time: Received(ingress),
            params,
        } in received
        {
            let egress = Instant::now();

            let duration = egress - ingress;

            self.replied.push(TraceItem {
                time: Replied {
                    ingress,
                    duration,
                    with_error: false,
                },
                params,
            })
        }
    }
}

trait Enrich<T: Param> {
    fn enrich(&mut self, id: usize, param: impl Into<T>) {}
}

mod param {

    use super::{Enrich, Trace};
    use derive_more::From;

    pub trait Param {}

    #[derive(From, Debug, Default)]
    pub struct LinearizationSize(usize);
    impl Param for LinearizationSize {}

    impl Enrich<LinearizationSize> for Trace {
        fn enrich(&mut self, id: usize, param: impl Into<LinearizationSize>) {
            let LinearizationSize(size) = param.into();
            self.received.entry(id).and_modify(|item| {
                item.params.linearization_size.insert(size);
            });
        }
    }
}

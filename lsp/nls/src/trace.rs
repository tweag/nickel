use std::{
    collections::HashMap,
    sync::{Mutex, MutexGuard},
    time::{Duration, Instant},
};

use anyhow::Result;
use lazy_static::lazy_static;
use lsp_server::RequestId;
use serde_json::Value;

use self::param::Enrichment;

lazy_static! {
    static ref TRACE: Mutex<Trace> = Mutex::new(Trace::default());
}

#[derive(Debug)]
struct TraceItem<T> {
    time: T,
    params: TraceItemParams,
}

#[derive(Debug, Default)]
pub struct TraceItemParams {
    method: String,
    linearization_size: Option<usize>,
}

#[derive(Debug)]
struct Received(Instant);
#[derive(Debug)]
struct Replied {
    ingress: Instant,
    duration: Duration,
    with_error: bool,
}

#[derive(Debug, Default)]
struct Trace {
    received: HashMap<RequestId, TraceItem<Received>>,
    replied: Vec<TraceItem<Replied>>,
}

pub struct Tracer;

impl Tracer {
    fn with_trace<F, O>(f: F) -> anyhow::Result<O>
    where
        F: FnOnce(MutexGuard<Trace>) -> O,
    {
        TRACE
            .lock()
            .map(f)
            .or_else(|_| anyhow::bail!("Could not lock tracer mutex"))
    }

    pub fn receive(id: RequestId, method: impl ToString) {
        let time = Received(Instant::now());
        let params = TraceItemParams {
            method: method.to_string().into(),
            ..Default::default()
        };

        Tracer::with_trace(|mut trace| trace.received.insert(id, TraceItem { time, params }))
            .unwrap();
    }

    pub fn reply(&mut self, id: RequestId) -> anyhow::Result<()> {
        let received = Self::with_trace(|mut t| t.received.remove(&id))?;

        for TraceItem {
            time: Received(ingress),
            params,
        } in received
        {
            let egress = Instant::now();

            let duration = egress - ingress;

            Self::with_trace(|mut t| {
                t.replied.push(TraceItem {
                    time: Replied {
                        ingress,
                        duration,
                        with_error: false,
                    },
                    params,
                })
            })?;
        }
        Ok(())
    }
}

pub(crate) trait Enrich<E: Enrichment> {
    fn enrich(id: &RequestId, param: E) -> anyhow::Result<()>;
}

pub mod param {

    use super::{Enrich, Tracer};
    use lsp_server::RequestId;
    use nickel::typecheck::linearization::Completed;

    pub trait Enrichment {}

    impl Enrichment for &Completed {}

    impl Enrich<&Completed> for Tracer {
        fn enrich(id: &RequestId, param: &Completed) -> anyhow::Result<()> {
            Self::with_trace(|mut t| {
                t.received.entry(id.to_owned()).and_modify(|item| {
                    item.params.linearization_size.insert(param.lin.len());
                });
            })
        }
    }
}

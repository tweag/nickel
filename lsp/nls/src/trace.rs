use std::{
    collections::HashMap,
    fmt::Display,
    io::Write,
    sync::{Mutex, MutexGuard},
    time::{Duration, Instant},
};

use anyhow::{Context, Result};
use lazy_static::lazy_static;
use log::error;
use lsp_server::RequestId;
use serde::Serialize;

lazy_static! {
    static ref TRACE: Mutex<Trace> = Mutex::new(Trace::default());
}

#[derive(Debug)]
pub struct TraceItem<T> {
    pub time: T,
    pub params: TraceItemParams,
}

impl TraceItem<Received> {
    fn into_replied(self, with_error: bool) -> TraceItem<Replied> {
        let Received(ingress) = self.time;
        let duration = ingress.elapsed();
        TraceItem {
            time: Replied {
                duration,
                with_error,
            },
            params: self.params,
        }
    }
}

#[derive(Debug, Default, Serialize)]
pub struct TraceItemParams {
    method: String,
    linearization_size: Option<usize>,
    file_size: Option<usize>,
}

#[derive(Debug, Serialize)]
pub struct CsvTraceItem {
    duration_micros: u128,
    with_error: bool,

    method: String,
    linearization_size: Option<usize>,
    file_size: Option<usize>,
}

impl From<TraceItem<Replied>> for CsvTraceItem {
    fn from(replied: TraceItem<Replied>) -> Self {
        CsvTraceItem {
            duration_micros: replied.time.duration.as_micros(),
            with_error: replied.time.with_error,
            method: replied.params.method,
            linearization_size: replied.params.linearization_size,
            file_size: replied.params.file_size,
        }
    }
}

impl<W: Write> Writer for csv::Writer<W> {
    fn write_item(&mut self, item: TraceItem<Replied>) -> Result<()> {
        self.serialize(CsvTraceItem::from(item))
            .with_context(|| "Could not serialize Trace item to CSV")
            .and_then(|_| <Self as Writer>::flush(self))
    }

    fn flush(&mut self) -> Result<()> {
        self.flush().with_context(|| "Could not flush writer")
    }
}

#[derive(Debug)]
struct Received(Instant);

impl Serialize for Received {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // 3 is the number of fields in the struct.
        serializer.serialize_none()
    }
}
#[derive(Debug, Serialize)]
pub struct Replied {
    duration: Duration,
    with_error: bool,
}

#[derive(Default)]
pub(crate) struct Trace {
    received: HashMap<RequestId, TraceItem<Received>>,
    writer: Option<Box<dyn Writer + Send>>,
}

pub trait Writer {
    fn write_item(&mut self, item: TraceItem<Replied>) -> Result<()>;
    fn flush(&mut self) -> Result<()>;
}

impl Writer for Trace {
    fn write_item(&mut self, item: TraceItem<Replied>) -> Result<()> {
        if let Some(writer) = self.writer.as_mut() {
            writer.write_item(item)
        } else {
            Ok(())
        }
    }

    fn flush(&mut self) -> Result<()> {
        if let Some(writer) = self.writer.as_mut() {
            writer.flush()
        } else {
            Ok(())
        }
    }
}

impl Trace {
    pub fn set_writer<W>(writer: W) -> Result<()>
    where
        W: Writer + Send + 'static,
    {
        Self::with_trace(|mut trace| {
            let old = trace.writer.replace(Box::new(writer));
            if let Some(mut writer) = old {
                writer.flush()?;
            }
            Ok(())
        })
    }

    #[allow(dead_code)]
    pub fn remove_writer() -> Result<()> {
        Self::with_trace(|mut trace| {
            if let Some(ref mut writer) = trace.writer {
                writer.flush()?;
            }
            trace.writer = None;
            Ok(())
        })
    }

    fn with_trace<F>(f: F) -> anyhow::Result<()>
    where
        F: FnOnce(MutexGuard<Trace>) -> Result<()>,
    {
        return TRACE
            .lock()
            .or_else(|_| anyhow::bail!("Could not lock tracer mutex"))
            .and_then(f);
    }

    pub fn receive(id: RequestId, method: impl ToString) {
        // let lock not affect receive time
        let time = Received(Instant::now());

        Self::with_trace(|mut trace| {
            let params = TraceItemParams {
                method: method.to_string(),
                ..Default::default()
            };
            trace.received.insert(id, TraceItem { time, params });
            Ok(())
        })
        .report();
    }

    pub fn reply_with(id: RequestId, with_error: bool) {
        Self::with_trace(|mut t| {
            t.received
                .remove(&id)
                .map(|received| received.into_replied(with_error))
                .map(|item| t.write_item(item))
                .unwrap_or(Ok(()))
        })
        .report()
    }

    pub fn reply(id: RequestId) {
        Self::reply_with(id, false)
    }

    pub fn error_reply(id: RequestId) {
        Self::reply_with(id, true)
    }

    #[allow(dead_code)]
    pub fn drop_received() -> anyhow::Result<()> {
        Self::with_trace(|mut t| {
            t.received.clear();
            Ok(())
        })
    }
}

pub(crate) trait Enrich<E> {
    fn enrich(id: &RequestId, param: E);
}

trait ResultExt<E> {
    fn report(&self);
}

impl<T, E: Display> ResultExt<E> for Result<T, E> {
    fn report(&self) {
        if let Err(e) = self {
            error!("{}", e);
        };
    }
}

pub mod param {
    use super::{Enrich, ResultExt, Trace};
    use lsp_server::RequestId;

    pub struct FileUpdate<'a> {
        pub content: &'a str,
    }

    impl<'a> Enrich<FileUpdate<'a>> for Trace {
        fn enrich(id: &RequestId, param: FileUpdate<'a>) {
            Self::with_trace(|mut t| {
                t.received.entry(id.to_owned()).and_modify(|item| {
                    item.params.file_size = Some(param.content.len());
                });
                Ok(())
            })
            .report();
        }
    }
}

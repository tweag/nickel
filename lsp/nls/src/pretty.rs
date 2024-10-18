use std::cell::Cell;

use nickel_lang_core::{
    pretty::NickelAllocatorExt,
    term::{
        record::{Field, RecordData},
        RichTerm,
    },
    typ::{RecordRows, RecordRowsIteratorItem},
};
use pretty::{docs, BoxAllocator, DocAllocator, DocBuilder, Pretty};

pub struct BoundedAllocator<'a, A> {
    pub inner: &'a pretty::BoxAllocator,
    pub depth: Cell<usize>,
    pub size: Cell<usize>,
    pub marker: std::marker::PhantomData<A>,
}

impl<'a, A> BoundedAllocator<'a, A> {
    fn shrink(&self, child_size: usize) {
        self.depth.set(self.depth.get().saturating_sub(1));
        self.size.set(child_size);
    }

    fn unshrink(&self, original_size: usize) {
        self.depth.set(self.depth.get() + 1);
        self.size.set(original_size);
    }
}

impl<'a, A: 'a> DocAllocator<'a, A> for BoundedAllocator<'a, A> {
    type Doc = <BoxAllocator as DocAllocator<'a, A>>::Doc;

    fn alloc(&'a self, doc: pretty::Doc<'a, Self::Doc, A>) -> Self::Doc {
        self.inner.alloc(doc)
    }

    fn alloc_column_fn(
        &'a self,
        f: impl Fn(usize) -> Self::Doc + 'a,
    ) -> <Self::Doc as pretty::DocPtr<'a, A>>::ColumnFn {
        self.inner.alloc_column_fn(f)
    }

    fn alloc_width_fn(
        &'a self,
        f: impl Fn(isize) -> Self::Doc + 'a,
    ) -> <Self::Doc as pretty::DocPtr<'a, A>>::WidthFn {
        self.inner.alloc_width_fn(f)
    }
}

impl<'a, A: Clone + 'a> NickelAllocatorExt<'a, A> for BoundedAllocator<'a, A> {
    fn record(
        &'a self,
        record_data: &RecordData,
        dyn_fields: &[(RichTerm, Field)],
    ) -> DocBuilder<'a, Self, A> {
        let size_per_child = self.size.get() / (record_data.fields.len() + dyn_fields.len());
        if record_data.fields.is_empty() && dyn_fields.is_empty() && !record_data.attrs.open {
            self.text("{}")
        } else if size_per_child == 0 || self.depth.get() == 0 {
            "{…}".pretty(self)
        } else {
            let orig_size = self.size.get();
            self.shrink(size_per_child);
            let ret = docs![
                self,
                self.line(),
                self.fields(&record_data.fields),
                if !dyn_fields.is_empty() {
                    docs![self, self.line(), self.dyn_fields(dyn_fields)]
                } else {
                    self.nil()
                },
                if record_data.attrs.open {
                    docs![self, self.line(), ".."]
                } else {
                    self.nil()
                }
            ]
            .nest(2)
            .append(self.line())
            .braces()
            .group();
            self.unshrink(orig_size);
            ret
        }
    }

    fn record_type(&'a self, rows: &RecordRows) -> DocBuilder<'a, Self, A> {
        let child_count = rows.iter().count().max(1);
        let size_per_child = self.size.get() / child_count;

        let orig_size = self.size.get();
        self.shrink(size_per_child);

        let tail = match rows.iter().last() {
            Some(RecordRowsIteratorItem::TailDyn) => docs![self, ";", self.line(), "Dyn"],
            Some(RecordRowsIteratorItem::TailVar(id)) => {
                docs![self, ";", self.line(), id.to_string()]
            }
            _ => self.nil(),
        };

        let rows = rows.iter().filter_map(|r| match r {
            RecordRowsIteratorItem::Row(r) => Some(r),
            _ => None,
        });

        let ret = docs![
            self,
            self.line(),
            self.intersperse(rows, docs![self, ",", self.line()]),
            tail
        ]
        .nest(2)
        .append(self.line())
        .braces()
        .group();
        self.unshrink(orig_size);
        ret
    }
}

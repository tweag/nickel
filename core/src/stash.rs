use std::collections::HashSet;

use malachite::Natural;
use nickel_lang_parser::{
    ast::Number,
    files::{DeserializeInterned, Interned, SerializeInterned},
    position::TermPos,
};
use rkyv::{
    Archive, Deserialize, Serialize,
    de::Pooling,
    rc::{ArchivedRc, RcResolver},
    ser::{Allocator, Positional, Sharing, Writer},
    vec::{ArchivedVec, VecResolver},
    with::ArchiveWith,
};
use smallvec::SmallVec;

use crate::{
    eval::value::NickelValue,
    files::FileId,
    position::{PosIdx, PosTable},
};

pub enum StashError {
    InvalidFile { id: FileId },
}

pub struct Stasher<'a> {
    inner: rkyv::ser::Serializer<
        Vec<u8>,
        rkyv::ser::allocator::ArenaHandle<'a>,
        rkyv::ser::sharing::Share,
    >,
    allowed_files: HashSet<FileId>,
    pos_table: &'a PosTable,
}

impl SerializeInterned<FileId> for Stasher<'_> {
    fn serialize_id(&mut self, id: FileId) -> Result<(), Self::Error> {
        if self.allowed_files.contains(&id) {
            Ok(())
        } else {
            Err(StashError::InvalidFile { id })
        }
    }
}

impl Interned for PosIdx {
    type Resolved = TermPos;
}

impl SerializeInterned<PosIdx> for Stasher<'_> {
    fn serialize_id(&mut self, id: PosIdx) -> Result<(), Self::Error> {
        self.pos_table.get(id).serialize(self)?;
        Ok(())
    }
}

impl rkyv::rancor::Fallible for Stasher<'_> {
    type Error = StashError;
}

impl<'a> Positional for Stasher<'a> {
    fn pos(&self) -> usize {
        self.inner.pos()
    }
}

impl<'a, E> Writer<E> for Stasher<'a> {
    fn write(&mut self, bytes: &[u8]) -> Result<(), E> {
        self.inner.write(bytes)
    }
}

unsafe impl<'a, E> Allocator<E> for Stasher<'a> {
    unsafe fn push_alloc(
        &mut self,
        layout: std::alloc::Layout,
    ) -> Result<std::ptr::NonNull<[u8]>, E> {
        // Safety: we're just wrapping the inner serializer so our safety requirements
        // are the same as theirs.
        unsafe { self.inner.push_alloc(layout) }
    }

    unsafe fn pop_alloc(
        &mut self,
        ptr: std::ptr::NonNull<u8>,
        layout: std::alloc::Layout,
    ) -> Result<(), E> {
        // Safety: we're just wrapping the inner serializer so our safety requirements
        // are the same as theirs.
        unsafe { self.inner.pop_alloc(ptr, layout) }
    }
}

impl<'a, E: rkyv::rancor::Source> Sharing<E> for Stasher<'a> {
    fn start_sharing(&mut self, address: usize) -> rkyv::ser::sharing::SharingState {
        <rkyv::ser::Serializer<_, _, _> as Sharing<E>>::start_sharing(&mut self.inner, address)
    }

    fn finish_sharing(&mut self, address: usize, pos: usize) -> Result<(), E> {
        self.inner.finish_sharing(address, pos)
    }
}

pub enum UnstashError {
    InvalidFile { id: FileId },
}

pub struct Unstasher {
    pool: rkyv::de::Pool,
    pub(crate) allowed_files: HashSet<FileId>,
    // TODO: maybe deduplicate positions on deserialization?
    pub(crate) pos_table: PosTable,
}

impl DeserializeInterned<FileId> for Unstasher {
    fn deserialize_id(&mut self, raw_id: u32) -> Result<FileId, Self::Error> {
        let id = FileId::from_raw(raw_id);
        if self.allowed_files.contains(&id) {
            Ok(id)
        } else {
            Err(UnstashError::InvalidFile { id })
        }
    }
}

impl DeserializeInterned<PosIdx> for Unstasher {
    fn deserialize_id(&mut self, pos: TermPos) -> Result<PosIdx, Self::Error> {
        Ok(self.pos_table.push(pos))
    }
}

impl rkyv::rancor::Fallible for Unstasher {
    type Error = UnstashError;
}

impl<E: rkyv::rancor::Source> Pooling<E> for Unstasher {
    fn start_pooling(&mut self, address: usize) -> rkyv::de::PoolingState {
        <rkyv::de::Pool as Pooling<E>>::start_pooling(&mut self.pool, address)
    }

    unsafe fn finish_pooling(
        &mut self,
        address: usize,
        ptr: rkyv::de::ErasedPtr,
        drop: unsafe fn(rkyv::de::ErasedPtr),
    ) -> Result<(), E> {
        // Safety: we're just wrapping the inner pool so our safety requirements
        // are the same as theirs.
        unsafe { self.pool.finish_pooling(address, ptr, drop) }
    }
}

pub(crate) struct ArchiveSmallVec;

impl<A: smallvec::Array> ArchiveWith<SmallVec<A>> for ArchiveSmallVec
where
    A::Item: Archive,
{
    type Archived = ArchivedVec<A::Item>;
    type Resolver = VecResolver;

    fn resolve_with(
        field: &SmallVec<A>,
        resolver: Self::Resolver,
        out: rkyv::Place<Self::Archived>,
    ) {
        ArchivedVec::resolve_from_len(field.len(), resolver, out);
    }
}

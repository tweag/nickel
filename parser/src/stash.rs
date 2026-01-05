use std::collections::HashSet;

use rkyv::{
    de::Pooling,
    primitive::ArchivedU32,
    ser::{Allocator, Positional, Sharing, Writer},
};

use crate::files::{ArchivedFileId, FileId};

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
}

impl Stasher<'_> {
    pub fn stash_file(&mut self, file_id: FileId) -> Result<(), StashError> {
        if self.allowed_files.contains(&file_id) {
            Ok(())
        } else {
            Err(StashError::InvalidFile { id: file_id })
        }
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

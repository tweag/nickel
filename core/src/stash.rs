use std::collections::HashSet;

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
};

use crate::{
    eval::value::NickelValue,
    files::FileId,
    position::{PosIdx, PosTable},
};

fn numerator_limbs(n: &Number) -> &[u64] {
    todo!()
}

fn denominator_limbs(n: &Number) -> &[u64] {
    todo!()
}

// TODO: this fails with a lifetime error. Without the "remote" thing it doesn't fail.
// #[derive(Archive)]
// #[rkyv(remote = Number)]
// struct NumberDef<'a> {
//     #[rkyv(getter = numerator_limbs, with = rkyv::with::Inline)]
//     num_limbs: &'a [u64],
//     #[rkyv(getter = denominator_limbs, with = rkyv::with::Inline)]
//     denom_limbs: &'a [u64],
// }

struct WithNumberDef;

// impl rkyv::with::ArchiveWith<Number> for WithNumberDef {
//     type Archived = ArchivedNumberDef;
//     type Resolver = ();

//     fn resolve_with(n: &Number, resolver: Self::Resolver, out: rkyv::Place<Self::Archived>) {
//         let num_limbs = n.numerator_ref().into_limbs_asc();
//         todo!()
//     }
// }

pub struct NickelValueFlavor;

impl Archive for NickelValue {
    type Archived = ArchivedRc<i32, NickelValueFlavor>; // FIXME

    type Resolver = RcResolver;

    fn resolve(&self, resolver: Self::Resolver, out: rkyv::Place<Self::Archived>) {
        todo!()
    }
}

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

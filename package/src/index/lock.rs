//! Locks for the package index.
//!
//! The package index can be shared between multiple nickel processes. This module
//! contains utilities to ensure that they don't interfere with one another.

use nickel_lang_flock::FileLock;
use nickel_lang_git::Spec;
use tempfile::tempdir_in;

use crate::{
    config::Config,
    error::{Error, IoResultExt as _},
};

// We use an advisory file lock to prevent the package index from being modified
// by multiple Nickel processes. This lock file goes inside the cache directory
// (e.g. ~/.cache/nickel/) and it controls access to the index directory
// (e.g. ~/.cache/nickel/index).
const LOCK_INDEX_FILENAME: &str = "index.lock";

pub trait LockType {}

#[derive(Debug)]
pub struct Shared;

#[derive(Debug)]
pub struct Exclusive;

impl LockType for Shared {}
impl LockType for Exclusive {}

#[derive(Debug)]
pub struct IndexLock<T: LockType> {
    config: Config,
    _inner: FileLock,
    lock_type: std::marker::PhantomData<T>,
}

impl IndexLock<Shared> {
    pub fn shared(config: &Config) -> Result<Self, Error> {
        let path = config.index_dir.parent().unwrap().join(LOCK_INDEX_FILENAME);
        Ok(IndexLock {
            config: config.clone(),
            _inner: nickel_lang_flock::open_ro_shared_create(&path, "package index")
                .with_path(&path)?,
            lock_type: std::marker::PhantomData,
        })
    }
}

impl IndexLock<Exclusive> {
    pub fn exclusive(config: &Config) -> Result<Self, Error> {
        let path = config.index_dir.parent().unwrap().join(LOCK_INDEX_FILENAME);
        Ok(IndexLock {
            config: config.clone(),
            _inner: nickel_lang_flock::open_rw_exclusive_create(&path, "package index")
                .with_path(path)?,
            lock_type: std::marker::PhantomData,
        })
    }

    pub fn index_dir_exists(&self) -> bool {
        self.config.index_dir.exists()
    }

    /// Fetch an updated package index from github and save it to our cache directory.
    pub fn download(&self) -> Result<(), Error> {
        let config = &self.config;
        let parent_dir = config.index_dir.parent().unwrap();
        std::fs::create_dir_all(parent_dir).with_path(parent_dir)?;

        eprint!("Fetching an updated package index...");
        let tree_path = tempdir_in(parent_dir).with_path(parent_dir)?;
        let _id = nickel_lang_git::fetch(&Spec::head(config.index_url.clone()), tree_path.path())?;

        // If there's an existing index at the on-disk location, replace it with the
        // fresh one we just downloaded. Doing this atomically and cross-platform is
        // tricky (rename is weird with directories), so we delete and then rename.
        // If everyone is honoring the index lock, no one should interfere between
        // the delete and rename.
        if self.index_dir_exists() {
            // We could do better with error messages here: if the recursive delete fails
            // because of some problem with a child, our error message will nevertheless
            // point at the root path.
            std::fs::remove_dir_all(&config.index_dir).with_path(&config.index_dir)?;
        }
        std::fs::rename(tree_path.into_path(), &config.index_dir).with_path(&config.index_dir)?;
        eprintln!("done!");
        Ok(())
    }
}

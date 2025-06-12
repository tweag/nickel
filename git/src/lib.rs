//! A small crate providing a convienient interface to the git operations
//! that nickel uses.

use anyhow::anyhow;
use gix::{
    interrupt::IS_INTERRUPTED,
    progress::Discard,
    remote::{self, fetch, fetch::refmap, Direction},
    worktree::state::checkout,
    ObjectId,
};
use std::{num::NonZero, path::Path};

/// An error that occurred during a git operation.
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("io error {error} at {path}")]
    Io {
        error: std::io::Error,
        path: std::path::PathBuf,
    },

    #[error("target `{target}` not found in `{url}`")]
    TargetNotFound { url: Box<gix::Url>, target: Target },

    #[error("{0:?}")]
    Internal(#[from] anyhow::Error),
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

trait IoResultExt<T> {
    fn with_path<P: AsRef<Path>>(self, path: P) -> Result<T>;
}

impl<T> IoResultExt<T> for Result<T, std::io::Error> {
    fn with_path<P: AsRef<Path>>(self, path: P) -> Result<T> {
        self.map_err(|error| Error::Io {
            error,
            path: path.as_ref().to_owned(),
        })
    }
}

trait InternalResultExt<T> {
    fn wrap_err(self) -> Result<T>;
}

impl<T, E: Into<anyhow::Error>> InternalResultExt<T> for Result<T, E> {
    fn wrap_err(self) -> Result<T> {
        self.map_err(|e| Error::Internal(e.into()))
    }
}

/// Specifies a git location that we can fetch.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Spec {
    /// The url of a git repository.
    pub url: gix::Url,
    /// Specifies the target commit within the repository.
    pub target: Target,
}

impl Spec {
    /// The HEAD commit at the given url.
    pub fn head(url: gix::Url) -> Self {
        Self {
            url,
            target: Target::Head,
        }
    }

    pub fn commit(url: gix::Url, commit: ObjectId) -> Self {
        Self {
            url,
            target: Target::Commit(commit),
        }
    }
}

/// The different kinds of git "thing" that we can target.
#[serde_with::serde_as]
#[derive(
    Clone,
    Debug,
    PartialEq,
    Eq,
    Hash,
    Default,
    serde::Serialize,
    serde::Deserialize,
    PartialOrd,
    Ord,
)]
pub enum Target {
    /// By default, we target the remote HEAD.
    #[default]
    Head,
    /// Target the tip of a specific branch.
    Branch(String),
    /// Target a specific tag.
    Tag(String),
    /// Target a specific commit.
    ///
    /// Currently, we only support a full commit: this needs to be a full hex-encoded
    /// sha hash. We could try to support prefixes also, but it requires some work
    /// and it *appears* to be inherently less efficient because there doesn't seem
    /// to be a way to fetch it in one shot. At least, when `cargo` needs to fetch an
    /// abbreviated hash it fetches everything and then looks for the hash among the
    /// things that it finds.
    Commit(#[serde_as(as = "serde_with::DisplayFromStr")] ObjectId),
}

impl std::fmt::Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Target::Head => write!(f, "HEAD"),
            Target::Branch(branch) => write!(f, "refs/heads/{branch}"),
            Target::Tag(tag) => write!(f, "refs/tags/{tag}"),
            Target::Commit(c) => write!(f, "{}", c),
        }
    }
}

fn source_object_id(source: &refmap::Source) -> Result<ObjectId> {
    match source {
        refmap::Source::ObjectId(id) => Ok(*id),
        refmap::Source::Ref(r) => {
            let (_name, id, peeled) = r.unpack();

            Ok(peeled
                .or(id)
                .ok_or_else(|| anyhow!("unborn reference"))?
                .to_owned())
        }
    }
}

/// Fetches the contents of a git repository into directory.
///
/// The directory will be created if it doesn't exist yet. The data will be
/// fetched from scratch, even if it has already been fetched before. However,
/// we will try to minimize the amount of data to fetch (for example, by doing a
/// shallow fetch).
///
/// Only the contents of the git repository will be written to the given
/// directory; the git directory itself will be discarded.
pub fn fetch(spec: &Spec, dir: impl AsRef<Path>) -> Result<ObjectId> {
    let dir = dir.as_ref();
    std::fs::create_dir_all(dir).with_path(dir)?;

    // Fetch the git directory somewhere temporary.
    let git_tempdir = tempfile::tempdir().wrap_err()?;
    let repo = gix::init(git_tempdir.path()).wrap_err()?;
    let refspec = spec.target.to_string();

    let remote = repo
        .remote_at(spec.url.clone())
        .wrap_err()?
        .with_fetch_tags(fetch::Tags::None)
        .with_refspecs(Some(refspec.as_str()), Direction::Fetch)
        .wrap_err()?;

    // This does similar credentials stuff to the git CLI (e.g. it looks for ssh
    // keys if it's a fetch over ssh, or it tries to run `askpass` if it needs
    // credentials for https). Maybe we want to have explicit credentials
    // configuration instead of or in addition to the default?
    let connection = remote.connect(Direction::Fetch).wrap_err()?;
    let outcome = connection
        .prepare_fetch(&mut Discard, remote::ref_map::Options::default())
        .wrap_err()?
        // For now, we always fetch shallow. Maybe for the index it's more efficient to
        // keep a single repo around and update it? But that might be in another method.
        .with_shallow(fetch::Shallow::DepthAtRemote(NonZero::new(1).unwrap()))
        .receive(&mut Discard, &IS_INTERRUPTED)
        .map_err(|e| match e {
            fetch::Error::NoMapping { .. } => Error::TargetNotFound {
                url: Box::new(spec.url.clone()),
                target: spec.target.clone(),
            },
            // This is the error we get back if we ask for a commit that they don't have.
            fetch::Error::Fetch(gix::protocol::fetch::Error::FetchResponse(
                gix::protocol::fetch::response::Error::UnknownSectionHeader { .. },
            )) => Error::TargetNotFound {
                url: Box::new(spec.url.clone()),
                target: spec.target.clone(),
            },
            _ => Error::Internal(e.into()),
        })?;

    if outcome.ref_map.mappings.len() > 1 {
        return Err(anyhow!("we only asked for 1 ref; why did we get more?")).wrap_err();
    }
    if outcome.ref_map.mappings.is_empty() {
        return Err(Error::TargetNotFound {
            url: Box::new(spec.url.clone()),
            target: spec.target.clone(),
        });
    }
    let object_id = source_object_id(&outcome.ref_map.mappings[0].remote)?;

    let object = repo.find_object(object_id).wrap_err()?;
    let tree_id = object.peel_to_tree().wrap_err()?.id();
    let mut index = repo.index_from_tree(&tree_id).wrap_err()?;

    checkout(
        &mut index,
        dir,
        repo.objects.clone(),
        &Discard,
        &Discard,
        &IS_INTERRUPTED,
        checkout::Options {
            overwrite_existing: true,
            ..Default::default()
        },
    )
    .wrap_err()?;
    index.write(Default::default()).wrap_err()?;

    Ok(tree_id.detach())
}

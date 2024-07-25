use std::path::PathBuf;

use directories::ProjectDirs;
use pubgrub::version::SemanticVersion;
use tempfile::{tempdir_in, TempDir};

use crate::index::Id;

pub fn cache_dir() -> PathBuf {
    let dir = ProjectDirs::from("org", "nickel-lang", "nickel").unwrap();
    dir.cache_dir().to_owned()
}

pub fn clone_git<Url, E>(url: Url) -> anyhow::Result<(TempDir, gix::Repository)>
where
    Url: TryInto<gix::Url, Error = E>,
    gix::url::parse::Error: From<E>,
{
    let cache_dir = cache_dir();
    std::fs::create_dir_all(&cache_dir)?;
    let tmp_dir = tempdir_in(&cache_dir)?;
    let (mut prepare_checkout, _) = gix::prepare_clone(url, &tmp_dir)?
        .fetch_then_checkout(gix::progress::Discard, &gix::interrupt::IS_INTERRUPTED)?;
    let (repo, _) =
        prepare_checkout.main_worktree(gix::progress::Discard, &gix::interrupt::IS_INTERRUPTED)?;

    Ok((tmp_dir, repo))
}

pub fn clone_github(id: &Id) -> anyhow::Result<(TempDir, gix::Repository)> {
    let url = format!("https://github.com/{}/{}.git", id.org, id.name);
    clone_git(url.as_str())
}

pub fn semver_to_pg(v: semver::Version) -> SemanticVersion {
    SemanticVersion::new(v.major as u32, v.minor as u32, v.patch as u32)
}

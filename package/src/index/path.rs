//! Path utilities for the package index.

use std::path::{Path, PathBuf};

use nickel_lang_core::cache::normalize_rel_path;

/// A relative path that has no "parent" components and is convertible to UTF-8.
///
/// The UTF-8 requirement comes from the fact that these paths need to be expressible
/// in Nickel package manifests, which are always UTF-8.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, Default)]
#[serde(transparent)]
pub struct RelativePath {
    inner: String,
}

impl std::fmt::Display for RelativePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl RelativePath {
    /// Is this an empty path?
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Iterate over the path components.
    pub fn components(&self) -> impl Iterator<Item = &str> + '_ {
        self.as_ref().components().map(|c| {
            match c {
                // unwrap: RelativePath is always UTF-8.
                std::path::Component::Normal(os_str) => os_str.to_str().unwrap(),
                // We're a normalized, relative path so we only have normal components.
                _ => unreachable!(),
            }
        })
    }
}

impl<'de> serde::Deserialize<'de> for RelativePath {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let path = PathBuf::deserialize(deserializer)?;
        path.try_into().map_err(serde::de::Error::custom)
    }
}

impl AsRef<Path> for RelativePath {
    fn as_ref(&self) -> &Path {
        self.inner.as_ref()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, thiserror::Error)]
pub enum RelativePathErrorKind {
    /// The path was an absolute path, where a relative one was expected.
    #[error("absolute path")]
    Absolute,
    /// The path pointed to something outside of the containing directory.
    #[error("points outside its parent")]
    OutOfBounds,
    /// The path contains invalid UTF-8.
    #[error("invalid UTF-8")]
    InvalidUtf8,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, thiserror::Error)]
#[error("could not convert {path} to a relative path: {kind}")]
pub struct RelativePathError {
    /// The path that could not be converted to a relative path.
    pub path: PathBuf,
    /// The reason the conversion failed.
    pub kind: RelativePathErrorKind,
}

impl TryFrom<PathBuf> for RelativePath {
    type Error = RelativePathError;

    fn try_from(path: PathBuf) -> Result<Self, Self::Error> {
        if path.is_absolute() {
            Err(RelativePathError {
                path,
                kind: RelativePathErrorKind::Absolute,
            })
        } else {
            let normalized = normalize_rel_path(&path);
            if normalized.components().next() == Some(std::path::Component::ParentDir) {
                Err(RelativePathError {
                    path,
                    kind: RelativePathErrorKind::OutOfBounds,
                })
            } else {
                let inner =
                    normalized
                        .into_os_string()
                        .into_string()
                        .map_err(|s| RelativePathError {
                            path: s.into(),
                            kind: RelativePathErrorKind::InvalidUtf8,
                        })?;
                Ok(RelativePath { inner })
            }
        }
    }
}

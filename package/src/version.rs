//! This module contains everything to do with version numbers.

use std::{num::ParseIntError, str::FromStr};

/// A full semantic version, including prerelease and build metadata.
#[serde_with::serde_as]
#[derive(
    Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct FullSemVer {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
    #[serde(default = "String::default")]
    pub pre: String,
    #[serde(default = "String::default")]
    pub build: String,
}

/// Our most-widely-used version type.
///
/// This drops the build metadata part (which we allow during parsing but
/// ignore for all version-resolution purposes).
///
/// Possible optimizations:
/// - shrink the numbers to `u32`
/// - intern the prerelease tag. This needs to be done in a way that preserves
///   the ordering rules, which are rather more complicated than a string comparison.
#[derive(
    Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct SemVer {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
    #[serde(default = "String::default")]
    pub pre: String,
}

impl SemVer {
    pub fn new(major: u64, minor: u64, patch: u64) -> Self {
        Self {
            major,
            minor,
            patch,
            pre: String::default(),
        }
    }

    pub fn bump_major(&self) -> SemVer {
        SemVer {
            major: self.major + 1,
            minor: 0,
            patch: 0,
            pre: String::default(),
        }
    }

    pub fn bump_minor(&self) -> SemVer {
        SemVer {
            major: self.major,
            minor: self.minor + 1,
            patch: 0,
            pre: String::default(),
        }
    }

    pub fn next_incompatible(&self) -> SemVer {
        // TODO: should we panic or something if pre is non-empty?
        if self.major == 0 {
            self.bump_minor()
        } else {
            self.bump_major()
        }
    }
}

impl From<FullSemVer> for SemVer {
    fn from(fsv: FullSemVer) -> Self {
        Self {
            major: fsv.major,
            minor: fsv.minor,
            patch: fsv.patch,
            pre: fsv.pre,
        }
    }
}

impl From<semver::Version> for SemVer {
    fn from(fsv: semver::Version) -> Self {
        Self {
            major: fsv.major,
            minor: fsv.minor,
            patch: fsv.patch,
            pre: fsv.pre.to_string(),
        }
    }
}

impl From<SemVer> for FullSemVer {
    fn from(sv: SemVer) -> Self {
        Self {
            major: sv.major,
            minor: sv.minor,
            patch: sv.patch,
            pre: sv.pre,
            build: String::default(),
        }
    }
}

// This conversion loses information on which of the fields were present. This
// information is sometimes relevant for comparing version requirements (e.g.,
// "1.3.0" matches the requirement "1.2" but it doesn't match the requirement
// "1.2.0").
impl From<PartialSemVer> for SemVer {
    fn from(psv: PartialSemVer) -> Self {
        Self {
            major: psv.major,
            minor: psv.minor.unwrap_or(0),
            patch: psv.patch.unwrap_or(0),
            pre: String::default(),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SemVerParseError {
    #[error("build metadata is not allowed in this semver")]
    Metadata,
    #[error(transparent)]
    Inner(#[from] semver::Error),
}

impl FromStr for SemVer {
    type Err = SemVerParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let full = semver::Version::from_str(s)?;
        if !full.build.is_empty() {
            Err(SemVerParseError::Metadata)
        } else {
            Ok(full.into())
        }
    }
}

impl std::fmt::Display for SemVer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let SemVer {
            major,
            minor,
            patch,
            pre,
        } = self;

        if pre.is_empty() {
            write!(f, "{major}.{minor}.{patch}")
        } else {
            write!(f, "{major}.{minor}.{patch}-{pre}")
        }
    }
}

/// A partial semantic version, with no pre-release part, and optional minor and patch versions.
#[derive(
    Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct PartialSemVer {
    pub major: u64,
    pub minor: Option<u64>,
    pub patch: Option<u64>,
}

impl PartialSemVer {
    pub fn major_minor(major: u64, minor: u64) -> Self {
        Self {
            major,
            minor: Some(minor),
            patch: None,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum PartialSemVerParseError {
    #[error("empty string")]
    Empty,
    #[error("a semantic version can contain at most 2 dots")]
    TooManyDots,
    #[error("invalid number: `{0}`")]
    Num(#[from] ParseIntError),
}

impl FromStr for PartialSemVer {
    type Err = PartialSemVerParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split('.');
        let major = parts
            .next()
            .ok_or(PartialSemVerParseError::Empty)?
            .parse()?;
        let minor = parts.next().map(u64::from_str).transpose()?;
        let patch = parts.next().map(u64::from_str).transpose()?;
        if parts.next().is_some() {
            return Err(PartialSemVerParseError::TooManyDots);
        }

        Ok(Self {
            major,
            minor,
            patch,
        })
    }
}

impl std::fmt::Display for PartialSemVer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (self.minor, self.patch) {
            (None, _) => {
                write!(f, "{}", self.major)
            }
            (Some(minor), None) => {
                write!(f, "{}.{}", self.major, minor)
            }
            (Some(minor), Some(patch)) => {
                write!(f, "{}.{}.{}", self.major, minor, patch)
            }
        }
    }
}

impl pubgrub::version::Version for SemVer {
    fn lowest() -> Self {
        Self::new(0, 0, 0)
    }

    fn bump(&self) -> Self {
        // `bump` ignores any pre-release version. Since we require that pre-release versions
        // come with exact version constraints, pubgrub should never try to bump any of those.
        debug_assert!(self.pre.is_empty());

        Self::new(self.major, self.minor, self.patch + 1)
    }
}

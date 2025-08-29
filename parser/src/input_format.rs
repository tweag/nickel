//! Nickel's supported input formats.

use std::{ffi::OsStr, path::Path};

/// Supported input formats.
#[derive(Default, Clone, Copy, Eq, Debug, PartialEq, Hash)]
pub enum InputFormat {
    #[default]
    Nickel,
    Json,
    Yaml,
    Toml,
    #[cfg(feature = "nix-experimental")]
    Nix,
    Text,
}

impl InputFormat {
    /// Returns an [InputFormat] based on the file extension of a path.
    pub fn from_path(path: impl AsRef<Path>) -> Option<InputFormat> {
        match path.as_ref().extension().and_then(OsStr::to_str) {
            Some("ncl") => Some(InputFormat::Nickel),
            Some("json") => Some(InputFormat::Json),
            Some("yaml") | Some("yml") => Some(InputFormat::Yaml),
            Some("toml") => Some(InputFormat::Toml),
            #[cfg(feature = "nix-experimental")]
            Some("nix") => Some(InputFormat::Nix),
            Some("txt") => Some(InputFormat::Text),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &'static str {
        match self {
            InputFormat::Nickel => "Nickel",
            InputFormat::Json => "Json",
            InputFormat::Yaml => "Yaml",
            InputFormat::Toml => "Toml",
            InputFormat::Text => "Text",
            #[cfg(feature = "nix-experimental")]
            InputFormat::Nix => "Nix",
        }
    }
}

impl std::str::FromStr for InputFormat {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "Json" => InputFormat::Json,
            "Nickel" => InputFormat::Nickel,
            "Text" => InputFormat::Text,
            "Yaml" => InputFormat::Yaml,
            "Toml" => InputFormat::Toml,
            #[cfg(feature = "nix-experimental")]
            "Nix" => InputFormat::Nix,
            _ => return Err(()),
        })
    }
}

use std::path::PathBuf;

use clap::{ArgGroup, Parser};
use gix::ObjectId;
use nickel_lang_git::{Spec, Target};

#[derive(Parser)]
#[command(group = ArgGroup::new("target").args(["branch", "tag", "commit"]))]
struct Args {
    repository: String,

    directory: PathBuf,

    #[arg(long)]
    branch: Option<String>,

    #[arg(long)]
    tag: Option<String>,

    #[arg(long)]
    commit: Option<ObjectId>,
}

pub fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let target = if let Some(branch) = args.branch {
        Target::Branch(branch)
    } else if let Some(tag) = args.tag {
        Target::Tag(tag)
    } else if let Some(commit) = args.commit {
        Target::Commit(commit)
    } else {
        Target::Head
    };

    let spec = Spec {
        url: args.repository.try_into()?,
        target,
    };

    nickel_lang_git::fetch(&spec, args.directory)?;
    Ok(())
}

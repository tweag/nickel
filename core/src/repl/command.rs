//! REPL commands helpers common to all frontends.
use super::*;
use std::fmt;

/// Available commands.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum CommandType {
    Load,
    Typecheck,
    Query,
    Print,
    Help,
    Exit,
}

impl CommandType {
    pub fn all() -> Vec<&'static str> {
        vec!["load", "typecheck", "query", "print", "help", "exit"]
    }
}

/// A parsed command with corresponding argument(s). Required argument are checked for
/// non-emptiness.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Command {
    Load(OsString),
    Typecheck(String),
    Query(String),
    Print(String),
    Help(Option<String>),
    Exit,
}

pub struct UnknownCommandError {}

/// Check that an argument is non-empty, or return an error with the given optional message.
fn require_arg(cmd: CommandType, arg: &str, msg_opt: Option<&str>) -> Result<(), ReplError> {
    if arg.trim().is_empty() {
        Err(ReplError::MissingArg {
            cmd,
            msg_opt: msg_opt.map(String::from),
        })
    } else {
        Ok(())
    }
}

impl FromStr for CommandType {
    type Err = UnknownCommandError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use CommandType::*;

        match s {
            "load" | "l" => Ok(Load),
            "typecheck" | "tc" => Ok(Typecheck),
            "query" | "q" => Ok(Query),
            "print" | "p" => Ok(Print),
            "help" | "?" | "h" => Ok(Help),
            "exit" | "e" => Ok(Exit),
            _ => Err(UnknownCommandError {}),
        }
    }
}

impl CommandType {
    /// Return the aliases of a command.
    pub fn aliases(&self) -> Vec<String> {
        use CommandType::*;

        match self {
            Load => vec![String::from("l")],
            Typecheck => vec![String::from("tc")],
            Query => vec![String::from("q")],
            Print => vec![String::from("p")],
            Help => vec![String::from("h"), String::from("?")],
            Exit => vec![String::from("e")],
        }
    }
}

impl std::fmt::Display for CommandType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CommandType::*;

        match self {
            Load => write!(f, "load"),
            Typecheck => write!(f, "typecheck"),
            Query => write!(f, "query"),
            Print => write!(f, "print"),
            Help => write!(f, "help"),
            Exit => write!(f, "exit"),
        }
    }
}

impl FromStr for Command {
    type Err = ReplError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let cmd_end = s.find(' ').unwrap_or(s.len());
        let cmd_str: String = s.chars().take(cmd_end).collect();
        let cmd: CommandType = cmd_str
            .parse()
            .map_err(|_| ReplError::UnknownCommand(cmd_str.clone()))?;
        let arg: String = s
            .chars()
            .skip(cmd_end + 1)
            .collect::<String>()
            .trim()
            .to_string();

        match cmd {
            CommandType::Load => {
                require_arg(cmd, &arg, Some("Please provide a file to load"))?;
                let arg = if arg.starts_with('"') && arg.ends_with('"') {
                    arg.chars().skip(1).take(arg.len() - 2).collect::<String>()
                } else {
                    arg
                };
                println!("{arg}");
                Ok(Command::Load(OsString::from(arg)))
            }
            CommandType::Typecheck => {
                require_arg(cmd, &arg, None)?;
                Ok(Command::Typecheck(arg))
            }
            CommandType::Query => {
                require_arg(cmd, &arg, None)?;
                Ok(Command::Query(arg))
            }
            CommandType::Print => {
                require_arg(cmd, &arg, None)?;
                Ok(Command::Print(arg))
            }
            CommandType::Exit => Ok(Command::Exit),
            CommandType::Help => {
                let arg_opt = if arg.trim().is_empty() {
                    None
                } else {
                    Some(String::from(arg.trim()))
                };

                Ok(Command::Help(arg_opt))
            }
        }
    }
}

impl Command {
    pub fn typ(&self) -> CommandType {
        use Command::*;

        match self {
            Load(..) => CommandType::Load,
            Typecheck(..) => CommandType::Typecheck,
            Query { .. } => CommandType::Query,
            Print(..) => CommandType::Print,
            Help(..) => CommandType::Help,
            Exit => CommandType::Exit,
        }
    }
}

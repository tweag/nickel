use continuation::Continuation;
use eval::Closure;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use term::Term;

#[derive(Debug)]
pub enum Marker {
    Arg(Closure),
    Thunk(Weak<RefCell<Closure>>),
    Cont(Continuation),
}

impl Marker {
    pub fn is_arg(&self) -> bool {
        match *self {
            Marker::Arg(_) => true,
            Marker::Thunk(_) => false,
            Marker::Cont(_) => false,
        }
    }

    pub fn is_thunk(&self) -> bool {
        match *self {
            Marker::Arg(_) => false,
            Marker::Thunk(_) => true,
            Marker::Cont(_) => false,
        }
    }

    pub fn is_cont(&self) -> bool {
        match *self {
            Marker::Arg(_) => false,
            Marker::Thunk(_) => false,
            Marker::Cont(_) => true,
        }
    }
}

#[derive(Debug)]
pub struct Stack(Vec<Marker>);

impl IntoIterator for Stack {
    type Item = Marker;
    type IntoIter = ::std::vec::IntoIter<Marker>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Stack {
    pub fn new() -> Stack {
        Stack(Vec::new())
    }

    fn count<P>(&self, pred: P) -> usize
    where
        P: Fn(&Marker) -> bool,
    {
        let mut count = 0;
        for marker in self.0.iter().rev() {
            if pred(marker) {
                count += 1;
            } else {
                break;
            }
        }
        count
    }

    /// Count the number of arguments at the top of the stack.
    pub fn count_args(&self) -> usize {
        Stack::count(self, Marker::is_arg)
    }

    pub fn count_thunks(&self) -> usize {
        Stack::count(self, Marker::is_thunk)
    }

    pub fn count_conts(&self) -> usize {
        Stack::count(self, Marker::is_cont)
    }

    pub fn push_arg(&mut self, arg: Closure) {
        self.0.push(Marker::Arg(arg))
    }

    pub fn push_thunk(&mut self, thunk: Weak<RefCell<Closure>>) {
        self.0.push(Marker::Thunk(thunk))
    }

    pub fn push_cont(&mut self, cont: Continuation) {
        self.0.push(Marker::Cont(cont))
    }

    pub fn pop_arg(&mut self) -> Option<Closure> {
        match self.0.pop() {
            Some(Marker::Arg(arg)) => Some(arg),
            Some(m) => {
                self.0.push(m);
                None
            }
            _ => None,
        }
    }

    pub fn pop_thunk(&mut self) -> Option<Weak<RefCell<Closure>>> {
        match self.0.pop() {
            Some(Marker::Thunk(thunk)) => Some(thunk),
            Some(m) => {
                self.0.push(m);
                None
            }
            _ => None,
        }
    }

    pub fn pop_cont(&mut self) -> Option<Continuation> {
        match self.0.pop() {
            Some(Marker::Cont(cont)) => Some(cont),
            Some(m) => {
                self.0.push(m);
                None
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn some_closure() -> Closure {
        Closure {
            body: Term::Bool(true),
            env: HashMap::new(),
        }
    }

    fn some_cont() -> Continuation {
        Continuation::Plus1(4.5)
    }

    fn some_arg_marker() -> Marker {
        Marker::Arg(some_closure())
    }

    fn some_thunk_marker() -> Marker {
        let rc = Rc::new(RefCell::new(some_closure()));
        Marker::Thunk(Rc::downgrade(&rc))
    }

    fn some_cont_marker() -> Marker {
        Marker::Cont(some_cont())
    }

    #[test]
    fn marker_differentiates() {
        assert!(some_arg_marker().is_arg());
        assert!(some_thunk_marker().is_thunk());
        assert!(some_cont_marker().is_cont());
    }

    #[test]
    fn pushing_and_poping_args() {
        let mut s = Stack::new();
        assert_eq!(0, s.count_args());

        s.push_arg(some_closure());
        s.push_arg(some_closure());
        assert_eq!(2, s.count_args());
        assert_eq!(some_closure(), s.pop_arg().expect("Already checked"));
        assert_eq!(1, s.count_args());
    }

    #[test]
    fn pushing_and_poping_thunks() {
        let mut s = Stack::new();
        assert_eq!(0, s.count_thunks());

        s.push_thunk(Rc::downgrade(&Rc::new(RefCell::new(some_closure()))));
        s.push_thunk(Rc::downgrade(&Rc::new(RefCell::new(some_closure()))));
        assert_eq!(2, s.count_thunks());
        s.pop_thunk().expect("Already checked");
        assert_eq!(1, s.count_thunks());
    }

    #[test]
    fn pushing_and_poping_conts() {
        let mut s = Stack::new();
        assert_eq!(0, s.count_conts());

        s.push_cont(some_cont());
        s.push_cont(some_cont());
        assert_eq!(2, s.count_conts());
        assert_eq!(some_cont(), s.pop_cont().expect("Already checked"));
        assert_eq!(1, s.count_conts());
    }

}

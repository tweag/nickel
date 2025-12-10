//! File-locking support.
//!
//! This was extracted from
//! [Cargo](https://github.com/rust-lang/cargo/blob/511e3ab9458ea6a6fa6a3eadc0b46c05829b0243/src/cargo/util/flock.rs),
//! so presumably the platform-dependent parts have been tested.

use std::fs::{File, OpenOptions};
use std::io;
use std::path::{Path, PathBuf};

use sys::*;

/// A locked file.
///
/// Locks are either shared (multiple processes can access the file) or
/// exclusive (only one process can access the file).
///
/// When this value is dropped, the lock will be released.
#[derive(Debug)]
pub struct FileLock {
    f: Option<File>,
    path: PathBuf,
}

impl Drop for FileLock {
    fn drop(&mut self) {
        if let Some(f) = self.f.take()
            && let Err(e) = unlock(&f)
        {
            eprintln!("failed to release lock {}: {e:?}", self.path.display());
        }
    }
}

fn open(path: &Path, opts: &OpenOptions, create: bool) -> io::Result<File> {
    let f = opts.open(path).or_else(|e| {
        // If we were requested to create this file, and there was a
        // NotFound error, then that was likely due to missing
        // intermediate directories. Try creating them and try again.
        if e.kind() == io::ErrorKind::NotFound && create {
            std::fs::create_dir_all(path.parent().unwrap())?;
            Ok(opts.open(path)?)
        } else {
            Err(e)
        }
    })?;
    Ok(f)
}

pub fn open_rw_exclusive_create<P>(path: P, msg: &str) -> io::Result<FileLock>
where
    P: AsRef<Path>,
{
    let mut opts = OpenOptions::new();
    let path = path.as_ref();
    opts.read(true).write(true).create(true);
    let f = open(path, &opts, true)?;
    acquire(msg, path, &|| try_lock_exclusive(&f), &|| {
        lock_exclusive(&f)
    })?;
    Ok(FileLock {
        f: Some(f),
        path: path.to_owned(),
    })
}

pub fn open_ro_shared_create<P: AsRef<Path>>(path: P, msg: &str) -> std::io::Result<FileLock> {
    let mut opts = OpenOptions::new();
    let path = path.as_ref();
    opts.read(true).write(true).create(true);
    let f = open(path, &opts, true)?;
    acquire(msg, path, &|| try_lock_shared(&f), &|| lock_shared(&f))?;
    Ok(FileLock {
        f: Some(f),
        path: path.to_owned(),
    })
}

fn try_acquire(path: &Path, lock_try: &dyn Fn() -> io::Result<()>) -> std::io::Result<bool> {
    // File locking on Unix is currently implemented via `flock`, which is known
    // to be broken on NFS. We could in theory just ignore errors that happen on
    // NFS, but apparently the failure mode [1] for `flock` on NFS is **blocking
    // forever**, even if the "non-blocking" flag is passed!
    //
    // As a result, we just skip all file locks entirely on NFS mounts. That
    // should avoid calling any `flock` functions at all, and it wouldn't work
    // there anyway.
    //
    // [1]: https://github.com/rust-lang/cargo/issues/2615
    if is_on_nfs_mount(path) {
        eprintln!("{path:?} appears to be an NFS mount, not trying to lock");
        return Ok(true);
    }

    match lock_try() {
        Ok(()) => return Ok(true),

        // In addition to ignoring NFS which is commonly not working we also
        // just ignore locking on filesystems that look like they don't
        // implement file locking.
        Err(e) if error_unsupported(&e) => return Ok(true),

        Err(e) => {
            if !error_contended(&e) {
                //let cx = format!("failed to lock file: {}", path.display());
                return Err(e);
            }
        }
    }
    Ok(false)
}

/// Acquires a lock on a file in a "nice" manner.
///
/// Almost all long-running blocking actions in Cargo have a status message
/// associated with them as we're not sure how long they'll take. Whenever a
/// conflicted file lock happens, this is the case (we're not sure when the lock
/// will be released).
///
/// This function will acquire the lock on a `path`, printing out a nice message
/// to the console if we have to wait for it. It will first attempt to use `try`
/// to acquire a lock on the crate, and in the case of contention it will emit a
/// status message based on `msg` to [`GlobalContext`]'s shell, and then use `block` to
/// block waiting to acquire a lock.
///
/// Returns an error if the lock could not be acquired or if any error other
/// than a contention error happens.
fn acquire(
    msg: &str,
    path: &Path,
    lock_try: &dyn Fn() -> io::Result<()>,
    lock_block: &dyn Fn() -> io::Result<()>,
) -> io::Result<()> {
    if try_acquire(path, lock_try)? {
        return Ok(());
    }
    eprintln!("Waiting for file lock on {msg}");

    lock_block()?;
    Ok(())
}

#[cfg(all(target_os = "linux", not(target_env = "musl")))]
fn is_on_nfs_mount(path: &Path) -> bool {
    use std::ffi::CString;
    use std::mem;
    use std::os::unix::prelude::*;

    let Ok(path) = CString::new(path.as_os_str().as_bytes()) else {
        return false;
    };

    unsafe {
        let mut buf: libc::statfs = mem::zeroed();
        let r = libc::statfs(path.as_ptr(), &mut buf);

        r == 0 && buf.f_type as u32 == libc::NFS_SUPER_MAGIC as u32
    }
}

#[cfg(any(not(target_os = "linux"), target_env = "musl"))]
fn is_on_nfs_mount(_path: &Path) -> bool {
    false
}

#[cfg(unix)]
mod sys {
    use std::fs::File;
    use std::io::{Error, Result};
    use std::os::unix::io::AsRawFd;

    #[cfg(not(target_os = "solaris"))]
    const LOCK_SH: i32 = libc::LOCK_SH;
    #[cfg(target_os = "solaris")]
    const LOCK_SH: i32 = 1;
    #[cfg(not(target_os = "solaris"))]
    const LOCK_EX: i32 = libc::LOCK_EX;
    #[cfg(target_os = "solaris")]
    const LOCK_EX: i32 = 2;
    #[cfg(not(target_os = "solaris"))]
    const LOCK_NB: i32 = libc::LOCK_NB;
    #[cfg(target_os = "solaris")]
    const LOCK_NB: i32 = 4;
    #[cfg(not(target_os = "solaris"))]
    const LOCK_UN: i32 = libc::LOCK_UN;
    #[cfg(target_os = "solaris")]
    const LOCK_UN: i32 = 8;

    pub(super) fn lock_shared(file: &File) -> Result<()> {
        flock(file, LOCK_SH)
    }

    pub(super) fn lock_exclusive(file: &File) -> Result<()> {
        flock(file, LOCK_EX)
    }

    pub(super) fn try_lock_shared(file: &File) -> Result<()> {
        flock(file, LOCK_SH | LOCK_NB)
    }

    pub(super) fn try_lock_exclusive(file: &File) -> Result<()> {
        flock(file, LOCK_EX | LOCK_NB)
    }

    pub(super) fn unlock(file: &File) -> Result<()> {
        flock(file, LOCK_UN)
    }

    pub(super) fn error_contended(err: &Error) -> bool {
        err.raw_os_error() == Some(libc::EWOULDBLOCK)
    }

    pub(super) fn error_unsupported(err: &Error) -> bool {
        match err.raw_os_error() {
            // Unfortunately, depending on the target, these may or may not be the same.
            // For targets in which they are the same, the duplicate pattern causes a warning.
            #[allow(unreachable_patterns)]
            Some(libc::ENOTSUP | libc::EOPNOTSUPP) => true,
            Some(libc::ENOSYS) => true,
            _ => false,
        }
    }

    #[cfg(not(target_os = "solaris"))]
    fn flock(file: &File, flag: libc::c_int) -> Result<()> {
        let ret = unsafe { libc::flock(file.as_raw_fd(), flag) };
        if ret < 0 {
            Err(Error::last_os_error())
        } else {
            Ok(())
        }
    }

    #[cfg(target_os = "solaris")]
    fn flock(file: &File, flag: libc::c_int) -> Result<()> {
        // Solaris lacks flock(), so try to emulate using fcntl()
        let mut flock = libc::flock {
            l_type: 0,
            l_whence: 0,
            l_start: 0,
            l_len: 0,
            l_sysid: 0,
            l_pid: 0,
            l_pad: [0, 0, 0, 0],
        };
        flock.l_type = if flag & LOCK_UN != 0 {
            libc::F_UNLCK
        } else if flag & LOCK_EX != 0 {
            libc::F_WRLCK
        } else if flag & LOCK_SH != 0 {
            libc::F_RDLCK
        } else {
            panic!("unexpected flock() operation")
        };

        let mut cmd = libc::F_SETLKW;
        if (flag & LOCK_NB) != 0 {
            cmd = libc::F_SETLK;
        }

        let ret = unsafe { libc::fcntl(file.as_raw_fd(), cmd, &flock) };

        if ret < 0 {
            Err(Error::last_os_error())
        } else {
            Ok(())
        }
    }
}

#[cfg(windows)]
mod sys {
    use std::fs::File;
    use std::io::{Error, Result};
    use std::mem;
    use std::os::windows::io::AsRawHandle;

    use windows_sys::Win32::Foundation::HANDLE;
    use windows_sys::Win32::Foundation::{ERROR_INVALID_FUNCTION, ERROR_LOCK_VIOLATION};
    use windows_sys::Win32::Storage::FileSystem::{
        LOCKFILE_EXCLUSIVE_LOCK, LOCKFILE_FAIL_IMMEDIATELY, LockFileEx, UnlockFile,
    };

    pub(super) fn lock_shared(file: &File) -> Result<()> {
        lock_file(file, 0)
    }

    pub(super) fn lock_exclusive(file: &File) -> Result<()> {
        lock_file(file, LOCKFILE_EXCLUSIVE_LOCK)
    }

    pub(super) fn try_lock_shared(file: &File) -> Result<()> {
        lock_file(file, LOCKFILE_FAIL_IMMEDIATELY)
    }

    pub(super) fn try_lock_exclusive(file: &File) -> Result<()> {
        lock_file(file, LOCKFILE_EXCLUSIVE_LOCK | LOCKFILE_FAIL_IMMEDIATELY)
    }

    pub(super) fn error_contended(err: &Error) -> bool {
        err.raw_os_error()
            .map_or(false, |x| x == ERROR_LOCK_VIOLATION as i32)
    }

    pub(super) fn error_unsupported(err: &Error) -> bool {
        err.raw_os_error()
            .map_or(false, |x| x == ERROR_INVALID_FUNCTION as i32)
    }

    pub(super) fn unlock(file: &File) -> Result<()> {
        unsafe {
            let ret = UnlockFile(file.as_raw_handle() as HANDLE, 0, 0, !0, !0);
            if ret == 0 {
                Err(Error::last_os_error())
            } else {
                Ok(())
            }
        }
    }

    fn lock_file(file: &File, flags: u32) -> Result<()> {
        unsafe {
            let mut overlapped = mem::zeroed();
            let ret = LockFileEx(
                file.as_raw_handle() as HANDLE,
                flags,
                0,
                !0,
                !0,
                &mut overlapped,
            );
            if ret == 0 {
                Err(Error::last_os_error())
            } else {
                Ok(())
            }
        }
    }
}

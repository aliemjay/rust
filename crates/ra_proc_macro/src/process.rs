//! Handle process life-time and message passing for proc-macro client

use crossbeam_channel::{bounded, Receiver, Sender};
use ra_tt::Subtree;

use crate::msg::{ErrorCode, Request, Response, ResponseError, Message};
use crate::rpc::{ExpansionResult, ExpansionTask, ListMacrosResult, ListMacrosTask, ProcMacroKind};

use io::{BufRead, BufReader};
use std::{
    convert::{TryFrom, TryInto},
    io::{self, Write},
    path::{Path, PathBuf},
    process::{Child, Command, Stdio},
    thread::{spawn, JoinHandle},
};

#[derive(Debug, Default)]
pub(crate) struct ProcMacroProcessSrv {
    inner: Option<Sender<Task>>,
}

#[derive(Debug)]
pub(crate) struct ProcMacroProcessThread {
    handle: Option<JoinHandle<()>>,
    sender: Sender<Task>,
}

enum Task {
    Request { req: Request, result_tx: Sender<Response> },
    Close,
}

struct Process {
    path: PathBuf,
    child: Child,
}

impl Process {
    fn run(process_path: &Path) -> Result<Process, io::Error> {
        let child = Command::new(process_path.clone())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;

        Ok(Process { path: process_path.into(), child })
    }

    fn restart(&mut self) -> Result<(), io::Error> {
        let _ = self.child.kill();
        self.child =
            Command::new(self.path.clone()).stdin(Stdio::piped()).stdout(Stdio::piped()).spawn()?;
        Ok(())
    }

    fn stdio(&mut self) -> Option<(impl Write, impl BufRead)> {
        let stdin = self.child.stdin.take()?;
        let stdout = self.child.stdout.take()?;
        let read = BufReader::new(stdout);

        Some((stdin, read))
    }
}

impl std::ops::Drop for ProcMacroProcessThread {
    fn drop(&mut self) {
        if let Some(handle) = self.handle.take() {
            let _ = self.sender.send(Task::Close);

            // Join the thread, it should finish shortly. We don't really care
            // whether it panicked, so it is safe to ignore the result
            let _ = handle.join();
        }
    }
}

impl ProcMacroProcessSrv {
    pub fn run(
        process_path: &Path,
    ) -> Result<(ProcMacroProcessThread, ProcMacroProcessSrv), io::Error> {
        let process = Process::run(process_path)?;

        let (task_tx, task_rx) = bounded(0);
        let handle = spawn(move || {
            client_loop(task_rx, process);
        });

        let srv = ProcMacroProcessSrv { inner: Some(task_tx.clone()) };
        let thread = ProcMacroProcessThread { handle: Some(handle), sender: task_tx };

        Ok((thread, srv))
    }

    pub fn find_proc_macros(
        &self,
        dylib_path: &Path,
    ) -> Result<Vec<(String, ProcMacroKind)>, ra_tt::ExpansionError> {
        let task = ListMacrosTask { lib: dylib_path.to_path_buf() };

        let result: ListMacrosResult = self.send_task(Request::ListMacro(task))?;
        Ok(result.macros)
    }

    pub fn custom_derive(
        &self,
        dylib_path: &Path,
        subtree: &Subtree,
        derive_name: &str,
    ) -> Result<Subtree, ra_tt::ExpansionError> {
        let task = ExpansionTask {
            macro_body: subtree.clone(),
            macro_name: derive_name.to_string(),
            attributes: None,
            lib: dylib_path.to_path_buf(),
        };

        let result: ExpansionResult = self.send_task(Request::ExpansionMacro(task))?;
        Ok(result.expansion)
    }

    pub fn send_task<R>(&self, req: Request) -> Result<R, ra_tt::ExpansionError>
    where
        R: TryFrom<Response, Error = &'static str>,
    {
        let sender = match &self.inner {
            None => return Err(ra_tt::ExpansionError::Unknown("No sender is found.".to_string())),
            Some(it) => it,
        };

        let (result_tx, result_rx) = bounded(0);

        sender.send(Task::Request { req: req.into(), result_tx }).map_err(|err| {
            ra_tt::ExpansionError::Unknown(format!(
                "Fail to send task in channel, reason : {:#?} ",
                err
            ))
        })?;

        let res = result_rx.recv().unwrap();
        match res {
            Response::Error(err) => {
                return Err(ra_tt::ExpansionError::ExpansionError(err.message));
            }
            _ => Ok(res.try_into().map_err(|err| {
                ra_tt::ExpansionError::Unknown(format!(
                    "Fail to get response, reason : {:#?} ",
                    err
                ))
            })?),
        }
    }
}

fn client_loop(task_rx: Receiver<Task>, mut process: Process) {
    let (mut stdin, mut stdout) = match process.stdio() {
        None => return,
        Some(it) => it,
    };

    loop {
        let task = match task_rx.recv() {
            Ok(task) => task,
            Err(_) => break,
        };

        let (req, result_tx) = match task {
            Task::Request { req, result_tx } => (req, result_tx),
            Task::Close => break,
        };

        let res = match send_request(&mut stdin, &mut stdout, req) {
            Ok(res) => res,
            Err(_err) => {
                let res = Response::Error(ResponseError {
                    code: ErrorCode::ServerErrorEnd,
                    message: "Server closed".into(),
                });
                if result_tx.send(res.into()).is_err() {
                    break;
                }
                // Restart the process
                if process.restart().is_err() {
                    break;
                }
                let stdio = match process.stdio() {
                    None => break,
                    Some(it) => it,
                };
                stdin = stdio.0;
                stdout = stdio.1;
                continue;
            }
        };

        if let Some(res) = res {
            if result_tx.send(res).is_err() {
                break;
            }
        }
    }

    let _ = process.child.kill();
}

fn send_request(
    mut writer: &mut impl Write,
    mut reader: &mut impl BufRead,
    req: Request,
) -> Result<Option<Response>, io::Error> {
    req.write(&mut writer)?;
    Ok(Response::read(&mut reader)?)
}

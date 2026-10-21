"""Bounded real-PTY qualification for the separate terminal executable."""

import fcntl
import json
import os
import platform
import pty
import re
import select
import signal
import struct
import subprocess
import sys
import tempfile
import termios
import time
from pathlib import Path

TERMINAL = sys.argv[1]
STDIO = sys.argv[2]
TIMEOUT = 12.0
MAX_TRANSCRIPT = 1_000_000
ANSI_CSI = re.compile(r"\x1b\[[0-?]*[ -/]*[@-~]")


def rendered_text(data):
    # Notty uses both CSI positioning and ESC E (next line) for complete frames.
    text = data.decode("utf-8", "replace").replace("\x1bE", "\n")
    return ANSI_CSI.sub("", text)


def check(condition, message, transcript=b""):
    if not condition:
        raise AssertionError(f"{message}: {transcript[-500:]!r}")


def same_terminal(before, after):
    before = list(before)
    after = list(after)
    # XNU adds PENDIN when canonical mode is restored through TIOCSETA.
    # https://github.com/apple-oss-distributions/xnu/blob/main/bsd/kern/tty.c#L1313-L1345
    if platform.system() == "Darwin":
        pending = getattr(termios, "PENDIN", 0x20000000)
        after[3] &= ~pending
        before[3] &= ~pending
    return before == after


def policy(root):
    return f"""(project_sources ((external_roots ({root}))))
(authoring_project_access
 ((read_grants (((root {root}) (principal_id Embedded_operator)
                 (authentication_kind embedded.local))))
  (selections (((root {root}) (project check.chatml)
                (request (entry ((name main)))) (target one_off_v1))))))
(developer
 ((selections
   (((source (Local_source (root {root}) (member check.chatml)))
     (target One_off_v1)
     (principals (((principal_id Embedded_operator)
                   (authentication_kind embedded.local)))))))
  (capacity
   ((max_runs 128) (max_active_commands 4) (max_active_agent_calls 8)
    (max_agent_sources 6) (max_native_bindings 32) (max_retained_tasks 8)
    (max_watches_per_connection 4) (max_pending_approvals 4)
    (max_repl_instances 8) (max_repl_keys_per_instance 128)
    (max_repl_submissions_per_epoch 64) (max_repl_source_bytes 65536)))
  (model Disabled) (approvals Disabled) (repl Standard_v1) (portable true)))
(developer_scopes (developer.repl developer.live.run))"""


def project_policy(root):
    """The same local authority boundary with an explicit Project environment."""
    return policy(root).replace(
        "(project check.chatml)\n                (request (entry ((name main))))",
        "(project chatml.project.json)\n"
        "                (request (entry ((name main) (environment workflow))))",
    ).replace(
        f"(Local_source (root {root}) (member check.chatml))",
        f"(Local_project (root {root}) (project chatml.project.json) "
        "(request (Entry (name main) (environment (workflow)))))",
    )


class Session:
    def __init__(self, command):
        self.master, self.slave = pty.openpty()
        try:
            fcntl.ioctl(self.slave, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 100, 0, 0))
            self.before = termios.tcgetattr(self.slave)
            environment = os.environ.copy()
            environment["TERM"] = "xterm-256color"
            for key in (
                "OPENAI_API_KEY", "OPENAI_KEY", "ANTHROPIC_API_KEY",
                "GEMINI_API_KEY", "GOOGLE_API_KEY",
            ):
                environment.pop(key, None)
            self.child = subprocess.Popen(
                command, stdin=self.slave, stdout=self.slave, stderr=self.slave,
                start_new_session=True, env=environment,
            )
        except BaseException:
            os.close(self.master)
            os.close(self.slave)
            raise
        self.output = bytearray()
        self.closed = False
        self.completed = False

    def __enter__(self):
        return self

    def __exit__(self, _kind, error, _traceback):
        try:
            self.close()
        except Exception:
            if error is None:
                raise
        return False

    def close(self):
        if self.closed:
            return
        self.closed = True
        try:
            if not self.completed:
                try:
                    os.killpg(self.child.pid, signal.SIGTERM)
                except ProcessLookupError:
                    pass
                try:
                    # Cleanup must not call drain(), whose transcript assertion
                    # may be the original failure being cleaned up.
                    self.child.wait(timeout=2.0)
                except subprocess.TimeoutExpired:
                    pass
                finally:
                    # A reaped leader does not prove its stdio child exited.
                    try:
                        os.killpg(self.child.pid, signal.SIGKILL)
                    except ProcessLookupError:
                        pass
                    self.child.wait(timeout=TIMEOUT)
        finally:
            try:
                os.close(self.master)
            finally:
                os.close(self.slave)

    def drain(self, seconds):
        until = time.monotonic() + seconds
        while time.monotonic() < until:
            readable, _, _ = select.select([self.master], [], [], 0.05)
            if readable:
                try:
                    chunk = os.read(self.master, 65536)
                except OSError:
                    break
                self.output.extend(chunk)
                check(len(self.output) <= MAX_TRANSCRIPT, "PTY transcript exceeded bound")
            if self.child.poll() is not None:
                break

    def await_bytes(self, wanted):
        deadline = time.monotonic() + TIMEOUT
        while wanted not in self.output and time.monotonic() < deadline:
            self.drain(0.05)
        check(wanted in self.output, f"PTY did not show {wanted!r}", self.output)

    def await_any_after(self, offset, choices):
        deadline = time.monotonic() + TIMEOUT
        while time.monotonic() < deadline:
            for choice in choices:
                if choice in self.output[offset:]:
                    return choice
            self.drain(0.05)
        raise AssertionError(f"PTY did not show {choices!r}: {self.output[-500:]!r}")

    def write(self, data):
        os.write(self.master, data)

    def await_ready_line_after(self, offset, wanted):
        deadline = time.monotonic() + TIMEOUT
        while time.monotonic() < deadline:
            text = rendered_text(self.output[offset:])
            before, ready, final = text.rpartition("[ready]")
            if (
                ready and "[busy " in before and "[busy " not in final
                and wanted in {line.strip() for line in final.splitlines()}
            ):
                return
            self.drain(0.05)
        raise AssertionError(f"ready frame did not show line {wanted!r}: {text[-1000:]!r}")

    def await_quiescence(self):
        """Allow queued real transport hints to settle, then require idle silence."""
        deadline = time.monotonic() + TIMEOUT
        start = len(self.output)
        while time.monotonic() < deadline:
            offset = len(self.output)
            self.drain(0.2)
            check(
                len(self.output) - start <= 65536,
                "terminal kept redrawing while idle", self.output,
            )
            if len(self.output) == offset:
                return
        raise AssertionError(f"terminal did not become idle: {self.output[-500:]!r}")

    def finish(self, expected_code):
        deadline = time.monotonic() + TIMEOUT
        while self.child.poll() is None and time.monotonic() < deadline:
            self.drain(0.05)
        if self.child.poll() is None:
            raise AssertionError(f"terminal exit timed out: {self.output[-500:]!r}")
        self.drain(0.1)
        check(self.child.returncode == expected_code, "wrong terminal exit", self.output)
        check(same_terminal(self.before, termios.tcgetattr(self.slave)), "PTY mode not restored", self.output)
        check(b"\x1b[?1049l" in self.output, "alternate screen not released", self.output)
        self.completed = True


with tempfile.TemporaryDirectory(prefix="och255-terminal-") as tmp:
    root = Path(tmp)
    (root / "workspace").mkdir()
    (root / "smoke.chatmd").write_text("<developer>Deterministic local terminal.</developer>")
    (root / "check.chatml").write_text("let main input = Task.pure(input)")
    (root / "policy.sexp").write_text(policy(root))
    (root / "src").mkdir()
    (root / "src" / "main.chatml").write_text(
        'let marker = "é"\nlet dependency_value = Util.value\n'
        'let main input = Task.pure(input)\n'
    )
    (root / "src" / "util.chatml").write_text('let marker = "é"\nlet value = 8\n')
    (root / "chatml.project.json").write_text(json.dumps({
        "schema_version": 1,
        "namespace": "terminal-workflow",
        "source_dirs": ["src"],
        "default_environment": "workflow",
        "environments": {"workflow": {"modules": {"Util": "self.Util"}}},
        "entries": {"main": {"source": "src/main.chatml", "target": "one_off_v1"}},
    }))
    (root / "project-policy.sexp").write_text(project_policy(root))

    def command(name, project=False):
        policy_name = "project-policy.sexp" if project else "policy.sexp"
        selection = (
            ["--project", "chatml.project.json", "--entry", "main", "--environment", "workflow"]
            if project else ["--source", "check.chatml"]
        )
        return [
            TERMINAL, "--local", "--stdio-executable", STDIO,
            "--developer-policy", str(root / policy_name),
            "--prompt-file", str(root / "smoke.chatmd"),
            "--workspace", str(root / "workspace"),
            "--data-root", str(root / name),
        ] + selection

    for redirected in ("stdin", "stdout"):
        master, slave = pty.openpty()
        try:
            name = f"redirected-{redirected}-data"
            result = subprocess.run(
                command(name),
                stdin=subprocess.DEVNULL if redirected == "stdin" else slave,
                stdout=slave if redirected == "stdin" else subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=TIMEOUT,
            )
            diagnostic = (result.stdout or b"") + result.stderr
            check(result.returncode == 1, "redirected terminal invocation did not fail", diagnostic)
            check(
                b"requires terminal stdin and stdout" in diagnostic,
                "redirected invocation lost its terminal diagnostic", diagnostic,
            )
            check(
                all(marker not in diagnostic for marker in (
                    b"\x1b", b"Raised at", b"Called from", b"Fatal error",
                )),
                "redirected invocation entered terminal rendering or leaked a backtrace", diagnostic,
            )
            check(not (root / name).exists(), "redirected invocation started the local host")
        finally:
            os.close(master)
            os.close(slave)

    help_result = subprocess.run(
        [TERMINAL, "--help"], stdin=subprocess.DEVNULL,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=TIMEOUT,
    )
    check(help_result.returncode == 0, "redirected --help failed", help_result.stderr)
    check(b"--local" in help_result.stdout, "redirected --help omitted usage", help_result.stdout)

    with Session(command("normal-data")) as normal:
        normal.await_bytes(b"[ready]")
        normal.write(b"Task.pure(8)\r:submit\r")
        normal.await_bytes(b"Task 1:")
        check(b"Task succeeded" not in normal.output, "Submit auto-ran Task", normal.output)
        normal.await_quiescence()
        offset = len(normal.output)
        normal.write(b":refs\r")
        normal.await_any_after(offset, (b"[busy References]",))
        normal.await_ready_line_after(offset, "finished")
        normal.await_quiescence()
        page = rendered_text(normal.output[offset:]).rpartition("[ready]")[2]
        check(page.count("Task 1:") == 2, "reference page did not show stored Task", normal.output)
        check("submission key not found" not in page, "reference page lost submission key", normal.output)
        normal.write(b":run 1\r")
        normal.await_bytes(b"Task succeeded")
        normal.await_quiescence()
        offset = len(normal.output)
        normal.write(b":refs\r")
        normal.await_any_after(offset, (b"[busy References]",))
        normal.await_ready_line_after(offset, "finished")
        normal.await_quiescence()
        page = rendered_text(normal.output[offset:]).rpartition("[ready]")[2]
        check(page.count("Task 1:") == 2, "Task execution lost submission reference page", normal.output)
        check("submission key not found" not in page, "Task execution lost reference-page key", normal.output)
        offset = len(normal.output)
        normal.write(b"617\r:submit\r")
        normal.await_any_after(offset, (b"1. ok 617",))
        normal.await_quiescence()
        submitted = rendered_text(normal.output[offset:]).rpartition("[ready]")[2]
        check("Task succeeded" not in submitted, "new submission retained previous Task result", normal.output)
        offset = len(normal.output)
        normal.write(b":type 1\r")
        normal.await_any_after(offset, (b"[busy Inspect_type]",))
        normal.await_ready_line_after(offset, "int")
        normal.await_quiescence()
        check(
            b"submission key not found" not in normal.output[offset:],
            "type inspection lost the stored submission key", normal.output,
        )
        offset = len(normal.output)
        normal.write(b"618\r:submit\r")
        normal.await_any_after(offset, (b"1. ok 618",))
        normal.await_quiescence()
        submitted = rendered_text(normal.output[offset:]).rpartition("[ready]")[2]
        check("int" not in {line.strip() for line in submitted.splitlines()}, "new submission retained old type notice", normal.output)
        offset = len(normal.output)
        normal.write(b":type 1\r")
        normal.await_any_after(offset, (b"[busy Inspect_type]",))
        normal.await_ready_line_after(offset, "int")
        normal.await_quiescence()
        offset = len(normal.output)
        normal.write(b":reset\r")
        normal.await_any_after(offset, (b"[busy (Keyed Reset)]",))
        normal.await_quiescence()
        check(
            b"No exact submission or Task key to observe" not in normal.output[offset:],
            "reset retained a stale observation notice", normal.output,
        )
        reset_frame = rendered_text(normal.output[offset:]).rpartition("[ready]")[2]
        check("int" not in {line.strip() for line in reset_frame.splitlines()}, "reset retained old type notice", normal.output)
        offset = len(normal.output)
        normal.write(b"619\r:submit\r")
        normal.await_any_after(offset, (b"1. ok 619",))
        normal.await_quiescence()
        normal.write(b"\x11")  # Ctrl+Q
        normal.finish(0)

    with Session(command("project-data", project=True)) as project:
        project.await_bytes(b"[ready]")
        project.await_bytes(b"environment=workflow")
        project.write(b"Task.pure(Util.value)\r:submit\r")
        project.await_bytes(b"Task 1:")
        check(b"Task succeeded" not in project.output, "Project Submit auto-ran Task", project.output)
        project.write(b":run 1\r")
        project.await_bytes(b"Task succeeded 8")
        project.write(b"\x11")
        project.finish(0)

    with Session(command("cancel-data")) as cancelled:
        cancelled.await_bytes(b"[ready]")
        cancelled.write(b"Task.pure(9)\r:submit\r")
        cancelled.await_bytes(b"Task 1:")
        cancelled.write(b"\x03")  # Ctrl+C, explicit exact-key cancel
        cancelled.await_bytes(b"busy Cancel")
        cancelled.write(b"\x11")
        cancelled.finish(0)

    with Session(command("held-data")) as held:
        held.await_bytes(b"[ready]")
        # Queue Ctrl+C with Submit so the UI can issue control before the
        # bounded interpreter reaches its fuel limit.
        held.write(b"let rec loop x = loop(x)\rlet never = loop(0)\r:submit\r\x03")
        for _attempt in range(4):
            offset = len(held.output)
            if _attempt > 0:
                held.write(b"\x03")  # Retry the same key after an early stale reply.
            answer = held.await_any_after(
                offset, (b"Cancellation answered", b"Cancellation unresolved")
            )
            if answer == b"Cancellation answered":
                break
        else:
            raise AssertionError(f"held Submit cancellation did not resolve: {held.output[-500:]!r}")
        held.await_bytes(b"stopped: REPL statement cancelled")
        held.write(b"\x11")
        held.finish(0)

    with Session(command("error-data")) as failed:
        failed.await_bytes(b"[ready]")
        failed.write(b"\x1b[200~\x7f")  # Unsupported Backspace inside bracketed paste
        failed.finish(1)

print("terminal local source/Project, Task quiescence/input/reset/type, redirected streams/help, cancellation and cleanup passed")

from __future__ import annotations

import base64
import logging
import os
import shlex
import socket
import sys
import tempfile
import threading
from contextlib import ExitStack
from multiprocessing.connection import Listener
from pathlib import Path
from typing import Callable, Dict, List, NamedTuple

logger = logging.getLogger(__name__)


script_text = """
import os
import sys
from base64 import b64decode
from multiprocessing.connection import Client


authkey = b64decode(os.environ["PARENT_AUTHKEY"])
port = os.environ["PARENT_PORT"]
address = ("localhost", int(port))
context = {
    "sys.argv": sys.argv,
}


with Client(address, "AF_INET", authkey) as conn:
    conn.send(context)
    conn.recv()
    sys.exit(0)
"""


class ExecutionContext(NamedTuple):
    argv: List[str]


class Script:
    """Some applications use executable programs as their interface.

    An example is the EDITOR environment variable, which applications use when
    text needs to be edited.

    When testing against those applications, usually the easiest approach is
    an ad-hoc script written to a temporary directory. The downside is that
    developers tools do not generally recognize the text the same way it would
    normal source code. If the source code is kept in a separate file, then developers
    have to remember that it is "special" with regards to imports and execution context.

    This wrapper class provides a way to hook a provided callback function into
    the execution of a separate command, so that the code that actually does
    the important work for testing is treated the same way as other source code.

    The provided callback is executed in a separate thread when the script
    is executed.
    """

    def __init__(self, callback: Callable[[ExecutionContext], None]):
        self._callback = callback
        self._context = None
        self._authkey = os.urandom(32)
        self._path = None
        self._listener = None
        self._thread = None
        self._connection = None
        self._connection_lock = threading.Lock()

    def __enter__(self) -> Script:
        self._context = ExitStack()
        self._start()
        return self

    def __exit__(self, exc_type, exc_value, tb) -> None:
        if self._context is None:
            return
        context, self._context = self._context, None
        context.close()

    def _start(self) -> None:
        assert self._context, "Must __enter__ script"
        self._thread = threading.Thread(target=self._run, daemon=True)
        # If we do not provide an address then the Listener internally requests
        # a random one.
        self._listener = self._context.enter_context(
            Listener(family="AF_INET", authkey=self._authkey)
        )
        self._thread.start()
        self._context.callback(self._thread.join)
        self._context.callback(self._shutdown_listener)

    def _shutdown_listener(self) -> None:
        # Just closing the socket causes the listening thread to hang, so we
        # reach into the listener and shutdown the socket explicitly.
        self._listener._listener._socket.shutdown(socket.SHUT_RDWR)
        self._listener.close()

        with self._connection_lock:
            if self._connection is not None:
                # TODO: May need to shutdown the underlying socket here, too.
                self._connection.close()

    def _run(self) -> None:
        with ExitStack() as stack:
            try:
                with self._connection_lock:
                    conn = self._connection = stack.enter_context(
                        self._listener.accept()
                    )
            except OSError:
                logger.debug("Failed to accept connection", exc_info=True)
                return

            try:
                data = conn.recv()
            except OSError:
                logger.debug("Failed to receive from connection", exc_info=True)
                return

            context = ExecutionContext(data["sys.argv"])

            try:
                self._callback(context)
            except:
                conn.send({"returncode": 1})
                raise
            else:
                conn.send({"returncode": 0})

    @property
    def _port(self) -> int:
        return self._listener.address[1]

    @property
    def environ(self) -> Dict[str, str]:
        """Environment that should be set for the child process.
        """
        assert self._context, "Must __enter__ script"
        return {
            "PARENT_PORT": str(self._port),
            "PARENT_AUTHKEY": base64.b64encode(self._authkey),
        }

    @property
    def path(self) -> str:
        assert self._context, "Must __enter__ script"
        if self._path is None:
            d = self._context.enter_context(tempfile.TemporaryDirectory())
            script_path = Path(d) / "script.py"
            script_path.write_text(script_text, encoding="utf-8")
            self._path = str(script_path)
        return self._path

    @property
    def command(self) -> str:
        """Command to use for the child process, like what would be passed to a shell.
        """
        args = [sys.executable, self.path]
        args = map(shlex.quote, args)
        return " ".join(args)

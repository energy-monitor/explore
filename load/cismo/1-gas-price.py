#!/usr/bin/env python3
import argparse
import datetime as dt
import json
import os
import random
import re
import ssl
import string
import sys
import threading
import time
import urllib.request


BASE_URL = "https://monitor.cismo.at/Gas/Gas_Ref_Prices/__sockjs__"
CHANNEL_ID = "0"
ALL_START = "2009-12-12"
TAG_PREFIX_RE = re.compile(r"^[0-9A-F]+#")


def parse_args() -> argparse.Namespace:
    today = str(dt.date.today())
    parser = argparse.ArgumentParser(
        description="Read raw xhr_streaming messages from the Cismo Gas Ref. Prices Shiny app."
    )
    parser.add_argument("--start", default=today, help="Start date in YYYY-MM-DD format")
    parser.add_argument("--end", default=today, help="End date in YYYY-MM-DD format")
    parser.set_defaults(all=True)
    parser.add_argument(
        "--all",
        action="store_true",
        dest="all",
        help="Trigger the app's 'All' button after initialization",
    )
    parser.add_argument(
        "--no-all",
        action="store_false",
        dest="all",
        help="Do not trigger the app's 'All' button after initialization",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=15.0,
        help="Seconds to keep the stream open after initialization",
    )
    parser.add_argument(
        "--raw-sockjs",
        action="store_true",
        help="Print raw SockJS lines instead of decoded Shiny messages",
    )
    parser.add_argument(
        "--raw-multiplex",
        action="store_true",
        help="Print multiplex payloads like '0|m|...' instead of inner JSON only",
    )
    parser.add_argument(
        "--pretty",
        action="store_true",
        help="Pretty-print decoded inner JSON messages",
    )
    parser.add_argument(
        "--insecure",
        action="store_true",
        default=True,
        help="Ignore TLS certificate verification (needed for this host here)",
    )
    parser.add_argument(
        "--last-message-file",
        default="data/tmp/gas-price.json",
        help="Write only the last emitted message to this file",
    )
    return parser.parse_args()


def validate_date(value: str) -> str:
    try:
        dt.date.fromisoformat(value)
    except ValueError as exc:
        raise SystemExit(f"Invalid date '{value}'. Expected YYYY-MM-DD.") from exc
    return value


def build_init_payload(start: str, end: str) -> dict:
    return {
        "DateSelector": [start, end],
        "M1Update:shiny.action": 0,
        "M3Update:shiny.action": 0,
        "M12Update:shiny.action": 0,
        "MallUpdate:shiny.action": 0,
        ".clientdata_output_YearsFilter_hidden": False,
        ".clientdata_output_ChartPlot_hidden": False,
        ".clientdata_output_ChartPlot_width": 1200,
        ".clientdata_output_ChartPlot_height": 600,
        ".clientdata_pixelratio": 1,
        ".clientdata_url_protocol": "https:",
        ".clientdata_url_hostname": "monitor.cismo.at",
        ".clientdata_url_port": "",
        ".clientdata_url_pathname": "/Gas/Gas_Ref_Prices/",
        ".clientdata_url_search": "",
        ".clientdata_url_hash_initial": "",
        ".clientdata_url_hash": "",
        ".clientdata_singletons": "",
    }


def strip_tag_prefix(frame: str) -> str:
    return TAG_PREFIX_RE.sub("", frame)


class StreamPrinter:
    def __init__(
        self,
        pretty: bool,
        raw_sockjs: bool,
        raw_multiplex: bool,
        last_message_file: str | None,
    ):
        self.pretty = pretty
        self.raw_sockjs = raw_sockjs
        self.raw_multiplex = raw_multiplex
        self.last_message_file = last_message_file
        self.opened = threading.Event()
        self.initialized = threading.Event()
        self.closed = threading.Event()
        self._lock = threading.Lock()
        self.last_emitted = ""

    def _emit(self, text: str) -> None:
        self.last_emitted = text
        if self.last_message_file:
            return
        try:
            print(text, flush=True)
        except BrokenPipeError:
            self.closed.set()

    def write_last_message(self) -> None:
        if not self.last_message_file:
            return

        parent = os.path.dirname(self.last_message_file)
        if parent:
            os.makedirs(parent, exist_ok=True)

        with open(self.last_message_file, "w", encoding="utf-8") as handle:
            handle.write(self.last_emitted)
            if self.last_emitted:
                handle.write("\n")

    def on_line(self, line: str) -> None:
        if not line:
            return

        if self.raw_sockjs:
            self._emit(line)

        if line == "o":
            self.opened.set()
            return

        if set(line) == {"h"}:
            return

        if line.startswith("c["):
            if not self.raw_sockjs:
                self._emit(line)
            self.closed.set()
            return

        if not line.startswith("a["):
            if not self.raw_sockjs:
                self._emit(line)
            return

        try:
            frames = json.loads(line[1:])
        except json.JSONDecodeError:
            if not self.raw_sockjs:
                self._emit(line)
            return

        for frame in frames:
            self._on_frame(frame)

    def _on_frame(self, frame: str) -> None:
        if self.raw_sockjs:
            return

        tagged_frame = frame
        frame = strip_tag_prefix(frame)

        if frame.startswith("ACK ") or frame.startswith("CONTINUE "):
            if self.raw_multiplex:
                self._emit(tagged_frame)
            return

        prefix = f"{CHANNEL_ID}|m|"
        if not frame.startswith(prefix):
            self._emit(tagged_frame if self.raw_multiplex else frame)
            return

        inner = frame[len(prefix) :]
        try:
            parsed = json.loads(inner)
        except json.JSONDecodeError:
            self._emit(inner)
            return

        if "config" in parsed:
            self.initialized.set()

        if self.raw_multiplex:
            self._emit(tagged_frame)
            return

        with self._lock:
            if self.pretty:
                self._emit(json.dumps(parsed, indent=2, sort_keys=True))
            else:
                self._emit(json.dumps(parsed, separators=(",", ":")))


class SockJsSession:
    def __init__(self, ctx: ssl.SSLContext, printer: StreamPrinter):
        self.ctx = ctx
        self.printer = printer
        self.server_id = f"{random.randint(0, 999):03d}"
        alphabet = string.ascii_letters + string.digits
        self.session_id = "".join(random.choice(alphabet) for _ in range(8))
        self.stream_url = f"{BASE_URL}/{self.server_id}/{self.session_id}/xhr_streaming"
        self.send_url = f"{BASE_URL}/{self.server_id}/{self.session_id}/xhr_send"
        self.thread = threading.Thread(target=self._stream, daemon=True)

    def start(self) -> None:
        self.thread.start()

    def _stream(self) -> None:
        req = urllib.request.Request(self.stream_url, method="POST", data=b"")
        buffer = ""
        with urllib.request.urlopen(req, context=self.ctx, timeout=60) as resp:
            while not self.printer.closed.is_set():
                chunk = resp.read1(65536) if hasattr(resp, "read1") else resp.read(65536)
                if not chunk:
                    break
                buffer += chunk.decode("utf-8", "replace")
                while "\n" in buffer:
                    line, buffer = buffer.split("\n", 1)
                    self.printer.on_line(line)
        if buffer:
            self.printer.on_line(buffer)

    def send_frames(self, frames: list[str]) -> None:
        body = json.dumps(frames).encode()
        req = urllib.request.Request(
            self.send_url,
            method="POST",
            data=body,
            headers={"Content-Type": "text/plain;charset=UTF-8"},
        )
        with urllib.request.urlopen(req, context=self.ctx, timeout=30) as resp:
            resp.read()


def main() -> int:
    args = parse_args()
    start = validate_date(ALL_START if args.all else args.start)
    end = validate_date(str(dt.date.today()) if args.all else args.end)
    if start > end:
        raise SystemExit("--start must be on or before --end.")

    ctx = ssl._create_unverified_context() if args.insecure else ssl.create_default_context()
    printer = StreamPrinter(
        pretty=args.pretty,
        raw_sockjs=args.raw_sockjs,
        raw_multiplex=args.raw_multiplex,
        last_message_file=args.last_message_file,
    )
    session = SockJsSession(ctx=ctx, printer=printer)

    print(
        f"Connecting to SockJS session {session.server_id}/{session.session_id}...",
        file=sys.stderr,
        flush=True,
    )
    session.start()

    if not printer.opened.wait(timeout=10):
        raise SystemExit("Timed out waiting for xhr_streaming to open.")

    init_payload = build_init_payload(start=start, end=end)
    session.send_frames(
        [
            f"{CHANNEL_ID}|o|",
            f"{CHANNEL_ID}|m|"
            + json.dumps({"method": "init", "data": init_payload}, separators=(",", ":")),
        ]
    )

    if not printer.initialized.wait(timeout=10):
        print("Warning: did not observe a Shiny config message after init.", file=sys.stderr)

    deadline = time.time() + args.timeout
    while time.time() < deadline and not printer.closed.is_set():
        time.sleep(0.1)

    printer.write_last_message()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())

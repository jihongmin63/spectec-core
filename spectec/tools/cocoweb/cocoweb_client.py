#!/usr/bin/env python3
"""Submit a (C)TRS to CoCoWeb and report the tool's verdict.

Reads a rewrite system (COPS CTRS/TRS format) from stdin (or --file), POSTs it
to the CoCoWeb web interface for a single tool/property, parses the verdict from
the response, and prints one token to stdout:

    YES | NO | MAYBE | TIMEOUT

Diagnostics and the raw tool output go to stderr. Exit code is 0 when a verdict
was parsed, non-zero on a network or parse error. This single-token stdout
contract keeps the OCaml caller (Cocoweb.check) trivial.

CoCoWeb runs the tools of the annual Confluence Competition (CoCo); it only
offers confluence-family properties (CR, GCR, INF, NFP, UNC, UNR, COM), so this
client targets CR (confluence) with CONFident by default.
"""

import argparse
import re
import sys
import uuid
import urllib.request
import urllib.error

DEFAULT_URL = "http://colo7-c703.uibk.ac.at/cocoweb/index.php"
DEFAULT_TOOL = "CR/2023/CTRS/CONFident"
DEFAULT_PROP = "CR"


def build_multipart(fields):
    """Encode ``fields`` (a list of (name, value) pairs, repeats allowed) as a
    multipart/form-data body. Returns (content_type, body_bytes)."""
    boundary = "----cocoweb" + uuid.uuid4().hex
    crlf = "\r\n"
    parts = []
    for name, value in fields:
        parts.append("--" + boundary)
        parts.append('Content-Disposition: form-data; name="%s"' % name)
        parts.append("")
        parts.append(value)
    parts.append("--" + boundary + "--")
    parts.append("")
    body = crlf.join(parts).encode("utf-8")
    content_type = "multipart/form-data; boundary=" + boundary
    return content_type, body


def submit(url, problem, tool, prop, timeout, http_timeout):
    """POST the rewrite system to CoCoWeb and return the response HTML."""
    cat = tool.rsplit("/", 1)[0]      # CR/2023/CTRS
    year = cat.rsplit("/", 1)[0]      # CR/2023
    fields = [
        ("prop[]", prop),
        ("year[]", year),
        ("cat[]", cat),
        ("tool[]", tool),
        ("timeout", str(timeout)),
        ("problem", problem),
        ("button", "check"),
    ]
    content_type, body = build_multipart(fields)
    req = urllib.request.Request(url, data=body, method="POST")
    req.add_header("Content-Type", content_type)
    with urllib.request.urlopen(req, timeout=http_timeout) as resp:
        charset = resp.headers.get_content_charset() or "utf-8"
        return resp.read().decode(charset, errors="replace")


def strip_tags(html):
    """Crude tag removal so we can scan the result text for a verdict token."""
    text = re.sub(r"(?is)<script.*?</script>", " ", html)
    text = re.sub(r"(?is)<style.*?</style>", " ", text)
    text = re.sub(r"(?s)<[^>]+>", " ", text)
    return text


CLASS_VERDICT = {"yes": "YES", "no": "NO", "maybe": "MAYBE", "timeout": "TIMEOUT"}


def parse_verdict(html):
    """Extract YES/NO/MAYBE/TIMEOUT from a CoCoWeb response. Returns the token
    or None if no verdict is present (e.g. an error page).

    CoCoWeb colours each result tab button by the tool's verdict via a CSS class
    (``<button class="tablinks active yes" ...>``); that class is the
    authoritative answer and is preferred over scanning the detailed output,
    which contains many incidental YES/NO tokens. Falls back to the first
    verdict token in the stripped text if no classed button is present."""
    for cls in re.findall(r'<button class="tablinks([^"]*)"', html):
        for word in cls.split():
            if word in CLASS_VERDICT:
                return CLASS_VERDICT[word]
    m = re.search(r"\b(YES|NO|MAYBE|TIMEOUT)\b", strip_tags(html))
    return m.group(1) if m else None


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--file", help="read the rewrite system from this file "
                        "instead of stdin")
    parser.add_argument("--tool", default=DEFAULT_TOOL,
                        help="CoCoWeb tool path (default: %(default)s)")
    parser.add_argument("--prop", default=DEFAULT_PROP,
                        help="property to check (default: %(default)s)")
    parser.add_argument("--timeout", type=int, default=30,
                        help="tool timeout in seconds (default: %(default)s)")
    parser.add_argument("--url", default=DEFAULT_URL,
                        help="CoCoWeb endpoint (default: %(default)s)")
    parser.add_argument("--debug", action="store_true",
                        help="dump the raw response HTML to stderr")
    args = parser.parse_args()

    if args.file:
        with open(args.file, "r") as f:
            problem = f.read()
    else:
        problem = sys.stdin.read()

    if not problem.strip():
        print("cocoweb_client: empty rewrite system", file=sys.stderr)
        return 2

    http_timeout = args.timeout + 30
    try:
        html = submit(args.url, problem, args.tool, args.prop, args.timeout,
                      http_timeout)
    except (urllib.error.URLError, OSError) as e:
        print("cocoweb_client: request failed: %s" % e, file=sys.stderr)
        return 3

    if args.debug:
        sys.stderr.write(html)
        sys.stderr.write("\n")

    verdict = parse_verdict(html)
    if verdict is None:
        print("cocoweb_client: no verdict found in response", file=sys.stderr)
        return 4

    print(verdict)
    return 0


if __name__ == "__main__":
    sys.exit(main())

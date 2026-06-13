#!/usr/bin/env python3
"""Submit a (C)TRS to MuTerm and report the tool's termination verdict.

Reads a rewrite system (COPS/TPDB .trs text) from stdin (or --file), POSTs it to
the MuTerm web interface, parses the verdict from the response, and prints one
token to stdout:

    YES | NO | MAYBE | TIMEOUT

Diagnostics and the raw tool output go to stderr. Exit code is 0 when a verdict
was parsed, non-zero on a network or parse error. This single-token stdout
contract keeps the OCaml caller (Muterm.check) trivial, and mirrors the sibling
tools/cocoweb/cocoweb_client.py used for confluence.

MuTerm checks termination only. Unlike CoCoWeb (multipart), the MuTerm page
submits via an AJAX POST to filter.php with an application/x-www-form-urlencoded
body (see the site's scripts/webinterface.js, filterTRS()).
"""

import argparse
import re
import sys
import urllib.parse
import urllib.request
import urllib.error

DEFAULT_URL = "http://zenon.dsic.upv.es/muterm/filter.php"
# solver: 0 = automatic (best methods), 1 = polynomials, 2 = RPO, 3 = DP.
DEFAULT_SOLVER = 0


def submit(url, problem, solver, timeout, http_timeout):
    """POST the rewrite system to MuTerm and return the response HTML.

    The field names and defaults match what scripts/webinterface.js sends:
    TRSCad/solver/timeouti plus the polynomial-interpretation knobs (left at the
    web defaults, only consulted when solver selects polynomials)."""
    fields = {
        "TRSCad": problem,
        "solver": str(solver),
        "timeouti": str(timeout),
        "maxvalue": "2",
        "polytype": "Linear",
        "coeffstype": "Rational",
        "coeffsdim": "1",
    }
    body = urllib.parse.urlencode(fields).encode("utf-8")
    req = urllib.request.Request(url, data=body, method="POST")
    req.add_header("Content-Type", "application/x-www-form-urlencoded")
    with urllib.request.urlopen(req, timeout=http_timeout) as resp:
        charset = resp.headers.get_content_charset() or "utf-8"
        return resp.read().decode(charset, errors="replace")


def strip_tags(html):
    """Crude tag removal so we can scan the result text for a verdict token."""
    text = re.sub(r"(?is)<script.*?</script>", " ", html)
    text = re.sub(r"(?is)<style.*?</style>", " ", text)
    text = re.sub(r"(?s)<[^>]+>", " ", text)
    return text


# MuTerm crashes on malformed input (e.g. a COPS CONDITIONTYPE header or "=="
# conditions) by printing a Haskell call-stack trace and *then* a fallback
# MAYBE. That MAYBE is not a real verdict, so these markers mean "parse error".
ERROR_MARKERS = ("CallStack", "Prelude.", "Parse error", "Exception")


def parse_verdict(html):
    """Extract YES/NO/MAYBE/TIMEOUT from a MuTerm response. Returns the token or
    None if no verdict is present (an error page, or a crash trace).

    MuTerm's filter.php returns a proof fragment that *leads* with the verdict
    word, so the first verdict token in the tag-stripped text is the answer."""
    text = strip_tags(html)
    if any(marker in text for marker in ERROR_MARKERS):
        return None
    m = re.search(r"\b(YES|NO|MAYBE|TIMEOUT)\b", text)
    return m.group(1) if m else None


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--file", help="read the rewrite system from this file "
                        "instead of stdin")
    parser.add_argument("--solver", type=int, default=DEFAULT_SOLVER,
                        help="MuTerm method: 0 auto, 1 poly, 2 RPO, 3 DP "
                        "(default: %(default)s)")
    parser.add_argument("--timeout", type=int, default=30,
                        help="tool timeout in seconds (default: %(default)s)")
    parser.add_argument("--url", default=DEFAULT_URL,
                        help="MuTerm endpoint (default: %(default)s)")
    parser.add_argument("--debug", action="store_true",
                        help="dump the raw response HTML to stderr")
    args = parser.parse_args()

    if args.file:
        with open(args.file, "r") as f:
            problem = f.read()
    else:
        problem = sys.stdin.read()

    if not problem.strip():
        print("muterm_client: empty rewrite system", file=sys.stderr)
        return 2

    http_timeout = args.timeout + 30
    try:
        html = submit(args.url, problem, args.solver, args.timeout, http_timeout)
    except (urllib.error.URLError, OSError) as e:
        print("muterm_client: request failed: %s" % e, file=sys.stderr)
        return 3

    if args.debug:
        sys.stderr.write(html)
        sys.stderr.write("\n")

    verdict = parse_verdict(html)
    if verdict is None:
        print("muterm_client: no verdict found in response", file=sys.stderr)
        return 4

    print(verdict)
    return 0


if __name__ == "__main__":
    sys.exit(main())

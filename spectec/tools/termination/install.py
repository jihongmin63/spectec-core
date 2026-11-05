#!/usr/bin/env python3
"""Install one pinned optional prover locally: python3 install.py TOOL.

Requires Python 3.12+ and Linux x86_64 for the published native binaries.
NTI is built from source and requires JAVA_HOME pointing to JDK 21+; its
upstream Maven wrapper runs the vendor tests. Runtime java on PATH must also
be version 21+. Downloads, Maven and build
outputs stay in .cache; installed binaries stay in ../TOOL/bin (gitignored).
Native prerequisites are libc, libgmp, libffi and (NaTT++) libstdc++.
Mint and NaTT++ additionally require z3 on PATH at runtime.

Only the explicit files in sources.toml are extracted from each verified OCI
layer. This avoids installing a container runtime or modifying the host OS.
The publisher images for MU-TERM/Mint/NaTT++ do not provide a verified license
grant here; their binaries are downloaded for local use, never redistributed.
"""

import hashlib
import json
import logging
import os
from pathlib import Path
import platform
import re
import shutil
import subprocess
import sys
import tarfile
import time
import tomllib
import urllib.error
import urllib.request


def string_field(record: dict[str, object], field: str) -> str:
    value = record[field]
    if not isinstance(value, str) or not value:
        raise ValueError(f"Required nonempty string field: {field}")
    return value


def record_value(value: object) -> dict[str, object]:
    if not isinstance(value, dict) or not all(isinstance(key, str) for key in value):
        raise ValueError("Expected an object with string keys")
    return value


def download(url: str, headers: dict[str, str], target: Path) -> None:
    for attempt in range(1, 4):
        try:
            request = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(request, timeout=120) as response, target.open("wb") as output:
                shutil.copyfileobj(response, output)
            return
        except (urllib.error.URLError, TimeoutError) as error:
            status = error.code if isinstance(error, urllib.error.HTTPError) else None
            body = error.read().decode("utf-8", errors="replace") if isinstance(error, urllib.error.HTTPError) else ""
            logging.warning("Download failed", extra={"url": url, "attempt": attempt, "error": str(error), "status": status, "body": body})
            if attempt == 3:
                raise
            time.sleep(2)


def verify(archive: Path, expected: str) -> None:
    with archive.open("rb") as stream:
        actual = hashlib.file_digest(stream, "sha256").hexdigest()
    if actual != expected:
        raise ValueError(f"SHA-256 mismatch for {archive}: expected={expected}, actual={actual}")


def install_image(config: dict[str, object], cache: Path, destination: Path) -> None:
    if platform.system() != "Linux" or platform.machine() != "x86_64":
        raise RuntimeError("Published native binaries require Linux x86_64")
    repo = string_field(config, "repository")
    digest = string_field(config, "layer")
    if re.fullmatch(r"[0-9a-f]{64}", digest) is None:
        raise ValueError(f"Invalid layer SHA-256: {digest}")
    archive = cache / (digest + ".tar.gz")
    if not archive.exists():
        token_file = cache / "token.json"
        try:
            download(f"https://auth.docker.io/token?service=registry.docker.io&scope=repository:{repo}:pull", {}, token_file)
            token = string_field(record_value(json.loads(token_file.read_text())), "token")
        finally:
            token_file.unlink(missing_ok=True)
        temporary = archive.with_suffix(".partial")
        download(f"https://registry-1.docker.io/v2/{repo}/blobs/sha256:{digest}", {"Authorization": "Bearer " + token}, temporary)
        verify(temporary, digest)
        temporary.replace(archive)
    verify(archive, digest)
    prefix = string_field(config, "prefix")
    names = config["files"]
    if not isinstance(names, list) or not names:
        raise ValueError("Expected a nonempty files list")
    with tarfile.open(archive) as contents:
        for name in names:
            if not isinstance(name, str) or Path(name).name != name or name in ("", ".", ".."):
                raise ValueError(f"Invalid installation filename: {name!r}")
            member = contents.getmember(prefix + name)
            if not member.isfile():
                raise ValueError(f"Expected regular file: {member.name}")
            source = contents.extractfile(member)
            if source is None:
                raise ValueError(f"Cannot read archive member: {member.name}")
            with source, (destination / name).open("wb") as output:
                shutil.copyfileobj(source, output)
            (destination / name).chmod(0o755)


def install_nti(config: dict[str, object], cache: Path, destination: Path) -> None:
    java_home = os.environ.get("JAVA_HOME")
    if java_home is None or not (Path(java_home) / "bin/javac").is_file():
        raise RuntimeError("Set JAVA_HOME to a JDK 21+ directory; a JRE cannot build NTI")
    digest = string_field(config, "sha256")
    archive = cache / (digest + ".tar.gz")
    if not archive.exists():
        temporary = archive.with_suffix(".partial")
        download(string_field(config, "url"), {}, temporary)
        verify(temporary, digest)
        temporary.replace(archive)
    verify(archive, digest)
    with tarfile.open(archive) as contents:
        contents.extractall(cache, filter="data")
    source = cache / string_field(config, "directory")
    environment = {**os.environ, "MAVEN_USER_HOME": str(cache / "maven")}
    subprocess.run([str(source / "mvnw"), "-B", f"-Dmaven.repo.local={cache / 'maven/repository'}", "package"], cwd=source, env=environment, check=True)
    shutil.copy2(source / "target/nti.jar", destination / "nti.jar")


def main(arguments: list[str]) -> None:
    installers = {"muterm": install_image, "mint": install_image, "nattpp": install_image, "nti": install_nti}
    if len(arguments) != 1 or arguments[0] not in installers:
        raise ValueError("Usage: install.py {muterm|nti|mint|nattpp}")
    name = arguments[0]
    root = Path(__file__).resolve().parent
    with (root / "sources.toml").open("rb") as source:
        config = record_value(tomllib.load(source)[name])
    cache = root / ".cache" / name
    cache.mkdir(parents=True, exist_ok=True)
    destination = root.parent / name / "bin"
    destination.mkdir(parents=True, exist_ok=True)
    installers[name](config, cache, destination)
    shutil.copy2(root / "sources.toml", destination / "sources.toml")
    print(f"Installed {name}: {destination}")


if __name__ == "__main__":
    logging.basicConfig(format="%(levelname)s %(message)s url=%(url)s attempt=%(attempt)s status=%(status)s error=%(error)s body=%(body)s")
    main(sys.argv[1:])

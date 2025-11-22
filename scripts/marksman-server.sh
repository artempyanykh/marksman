#!/usr/bin/env bash

set -x

RESOLVED_PATH="$(readlink -f "${BASH_SOURCE[0]}")"
SCRIPT_DIR=$(cd -- "$(dirname -- "$RESOLVED_PATH")" &>/dev/null && pwd)
TOP_DIR="$(realpath "$SCRIPT_DIR/..")"
make -C "$TOP_DIR" build </dev/null 1>&2

exec "$TOP_DIR/Marksman/bin/Debug/net9.0/marksman" "${@:1}"

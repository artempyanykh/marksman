#!/usr/bin/env bash

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
TOP_DIR="$SCRIPT_DIR/.."
make -C "$TOP_DIR" build </dev/null 1>&2

exec "$TOP_DIR/Marksman/bin/Debug/net7.0/marksman" "${@:1}" server -v=4

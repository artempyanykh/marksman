#!/usr/bin/env bash

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
TOP_DIR="$SCRIPT_DIR/.."
make -C "$TOP_DIR" build
exec "$TOP_DIR/Marksman/bin/Debug/net6.0/Marksman" "${@:1}"
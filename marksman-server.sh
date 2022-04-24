#!/usr/bin/env bash

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)

pushd "$SCRIPT_DIR" > /dev/null || exit
dotnet build 1>&2
popd > /dev/null || exit

exec "$SCRIPT_DIR/Marksman/bin/Debug/net6.0/Marksman" server -v 4

#!/usr/bin/env bash

# Build zeta-note and run the debug version of the binary

pushd -n "$(pwd)" || exit

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_DIR="$( dirname "$SCRIPT_DIR" )"

cd "$PROJECT_DIR" || exit
cargo build
popd || exit

exec /usr/bin/env NO_COLOR=1 RUST_LOG=zeta_note=trace,lsp_server=trace RUST_BACKTRACE=1 \
    "$PROJECT_DIR/target/debug/zeta-note" "$@"

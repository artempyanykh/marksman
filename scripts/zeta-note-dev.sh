#!/usr/bin/env bash

# Build zeta-note and run the debug version of the binary

pushd -n "$(pwd)" || exit

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_DIR="$( dirname "$SCRIPT_DIR" )"

cd "$PROJECT_DIR" || exit
cargo build
popd || exit

RUST_LOG=zeta_node=debug,lsp_server=debug RUST_BACKTRACE=1 "$PROJECT_DIR/target/debug/zeta-note" "$@"

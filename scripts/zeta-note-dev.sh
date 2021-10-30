#!/usr/bin/env bash

# Build zeta-note and run the debug version of the binary

pushd -n "$(pwd)" || exit

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_DIR="$( dirname "$SCRIPT_DIR" )"

cd "$PROJECT_DIR" || exit

if test -z "$(type -P cargo)"; then
    echo "warn: cargo is not in PATH; trying to locate under a known location..." >&2
    KNOWN_CARGO_BIN="$HOME/.cargo/bin"

    if test -f "$KNOWN_CARGO_BIN/cargo"; then
        echo "warn: found cargo under $KNOWN_CARGO_BIN, adding to PATH." >&2
        export PATH="$KNOWN_CARGO_BIN:$PATH"
    else
        echo "error: no known cargo installation on the system." >&2
        exit
    fi
fi

cargo build || exit
popd || exit

NO_COLOR=1 TERM=dumb RUST_LOG=zeta_note=trace,lsp_server=trace RUST_BACKTRACE=1 \
    exec "$PROJECT_DIR/target/debug/zeta-note" "$@"

# `lsp-document`

Helpers to convert between LSP representations of text documents and Rust strings.

## TL;DR:

LSP uses UTF16-encoded strings while Rust's strings are UTF8-encoded. This
means that text offsets in LSP and in Rust are different:
- LSP offsets are in 16-bit code-units and each character is either 1 or 2 of those,
- Rust strings are indexed in bytes and each character takes from 1 to 4 bytes.

To ensure that LSP client and server "talk" about the same part of a text
document we need a translation layer.

This crate provides such a layer.
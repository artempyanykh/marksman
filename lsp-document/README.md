# `lsp-document`

[![Docs](https://docs.rs/lsp-document/badge.svg)](https://docs.rs/lsp-document)

Helpers to convert between LSP representations of text documents and Rust strings.

## TL;DR:

LSP uses UTF16-encoded strings while Rust's strings are UTF8-encoded. This
means that text offsets in LSP and in Rust are different:
- LSP offsets are in 16-bit code-units and each character is either 1 or 2 of those,
- Rust strings are indexed in bytes and each character takes from 1 to 4 bytes.

To ensure that LSP client and server "talk" about the same part of a text
document we need a translation layer.

This crate provides such a layer.


## Example usage

See [the docs](https://docs.rs/lsp-document) for more details.

```rust
use lsp_document::{TextMap, TextAdapter, Pos, IndexedText};
use lsp_types::Position;

// Character width
// U16:     1111111111111 1111111111 1 11 1 1 111111111 21
// U8:      1111111111111 1222122221 1 13 3 3 111111111 41
// U8 offset
//          0         1       2      3       4          5
//          0123456789012 3468013579 0 12 5 8 123456789 04
let text = "Hello, world!\nКак дела?\r\n做得好\nThis is 💣!";
let text = IndexedText::new(text);
//
// Examples of using TextMap methods
//
// Pos of 💣 from its offset
assert_eq!(text.offset_to_pos(50).unwrap(), Pos::new(3, 8));
// Raw line range info
assert_eq!(text.line_range(2).unwrap(), Pos::new(2, 0)..Pos::new(2, 10));
// Extracting part of text between two positions
assert_eq!(text.substr(Pos::new(1, 7)..Pos::new(1, 15)).unwrap(), "дела");

//
// Example of using TextAdapter methods
//
// Pos of `!` after 💣
assert_eq!(text.lsp_pos_to_pos(&Position::new(3, 10)).unwrap(), Pos::new(3, 12));
assert_eq!(text.pos_to_lsp_pos(&Pos::new(3, 12)).unwrap(), Position::new(3, 10));
```

## Using `String`s for text manipulation

Currently, the crate works for `str`-like representation of text. UTF8-encoded strings are efficiently packed in memory, which means:
1. 👍 There's low memory overhead of storing these strings.
2. 👍 The contents is contiguous in memory, hence random access and iteration
  over chars are fast (the latter is important for conversion between lsp and
  native positions).
3. 👎 Making changes to strings is slow as it requires time proportional to the length of the string.

Most likely, the performance impact of 3. won't be a problem as we _query_
data much more often than we _change_ it (in the context of LSP servers). So,
using `String`s should be just fine for a lot of applications.

However, having an implementation backed by a `Rope` wouldn't hurt (although this is not a priority at the moment).
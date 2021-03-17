use std::{borrow::Borrow, cmp::Ordering, ops::Range};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pos {
    line: u32,
    /// Byte offset from the beginning of the line
    col: u32,
}

impl Pos {
    pub fn new(line: u32, col_offset: u32) -> Self {
        Self {
            line,
            col: col_offset,
        }
    }
}

impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pos {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let line_cmp = self.line.cmp(&other.line);
        if line_cmp == Ordering::Equal {
            self.col.cmp(&other.col)
        } else {
            line_cmp
        }
    }
}

pub struct TextChange {
    range: Option<Range<Pos>>,
    patch: String,
}

pub trait TextMap {
    fn text(&self) -> &str;
    fn offset_to_pos(&self, offset: usize) -> Option<Pos>;
    fn offset_range_to_range(&self, offsets: Range<usize>) -> Option<Range<Pos>> {
        let start = self.offset_to_pos(offsets.start)?;
        let end = self.offset_to_pos(offsets.end)?;
        Some(start..end)
    }
    fn line_range(&self, line: u32) -> Option<Range<Pos>>;
    fn substr(&self, range: Range<Pos>) -> Option<&str>;
}

pub trait TextAdapter {
    fn pos_to_lsp_pos(&self, pos: &Pos) -> Option<lsp_types::Position>;
    fn lsp_pos_to_pos(&self, lsp_pos: &lsp_types::Position) -> Option<Pos>;
    fn range_to_lsp_range(&self, range: &Range<Pos>) -> Option<lsp_types::Range>;
    fn lsp_range_to_range(&self, lsp_range: &lsp_types::Range) -> Option<Range<Pos>>;
    fn change_to_lsp_change(
        &self,
        change: TextChange,
    ) -> Option<lsp_types::TextDocumentContentChangeEvent>;
    fn lsp_change_to_change(
        &self,
        lsp_change: lsp_types::TextDocumentContentChangeEvent,
    ) -> Option<TextChange>;
}

impl<T: TextMap> TextAdapter for T {
    fn pos_to_lsp_pos(&self, pos: &Pos) -> Option<lsp_types::Position> {
        let line_num = pos.line;
        let line_range = self.line_range(line_num)?;
        let line = self.substr(line_range)?;

        let target_u8_offset = pos.col as usize;

        let mut u8_offset: usize = 0;
        let mut u16_offset: usize = 0;
        let mut found = false;

        for c in line.chars() {
            if u8_offset == target_u8_offset {
                found = true;
                break;
            } else {
                u8_offset += c.len_utf8();
                u16_offset += c.len_utf16();
            }
        }

        // Handle "append"/"after eol" case
        if !found && u8_offset == target_u8_offset {
            found = true;
        }

        assert!(found, "Offset not found in line");
        Some(lsp_types::Position::new(line_num as u32, u16_offset as u32))
    }

    fn lsp_pos_to_pos(&self, lsp_pos: &lsp_types::Position) -> Option<Pos> {
        let line_range = self.line_range(lsp_pos.line)?;
        let line = self.substr(line_range)?;

        let mut u8_offset: usize = 0;
        let mut u16_offset: usize = 0;
        let mut found = false;

        // Handle the case of artificial blank line
        if lsp_pos.character == 0 {
            found = true;
        }

        for c in line.chars() {
            if u16_offset == lsp_pos.character as usize {
                found = true;
                break;
            } else {
                u16_offset += c.len_utf16();
                u8_offset += c.len_utf8();
            }
        }

        // Handle "append" case
        if !found && u16_offset == lsp_pos.character as usize {
            found = true;
        }

        assert!(found, "LSP pos not found in line");
        Some(Pos::new(lsp_pos.line, u8_offset as u32))
    }

    fn range_to_lsp_range(&self, range: &Range<Pos>) -> Option<lsp_types::Range> {
        Some(lsp_types::Range::new(
            self.pos_to_lsp_pos(&range.start)?,
            self.pos_to_lsp_pos(&range.end)?,
        ))
    }

    fn lsp_range_to_range(&self, lsp_range: &lsp_types::Range) -> Option<Range<Pos>> {
        Some(self.lsp_pos_to_pos(&lsp_range.start)?..self.lsp_pos_to_pos(&lsp_range.end)?)
    }

    fn change_to_lsp_change(
        &self,
        change: TextChange,
    ) -> Option<lsp_types::TextDocumentContentChangeEvent> {
        if let Some(range) = change.range {
            let lsp_range = self.range_to_lsp_range(&range)?;
            Some(lsp_types::TextDocumentContentChangeEvent {
                range: Some(lsp_range),
                range_length: None,
                text: change.patch,
            })
        } else {
            Some(lsp_types::TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: change.patch,
            })
        }
    }

    fn lsp_change_to_change(
        &self,
        lsp_change: lsp_types::TextDocumentContentChangeEvent,
    ) -> Option<TextChange> {
        if let Some(lsp_range) = lsp_change.range {
            let range = self.lsp_range_to_range(&lsp_range)?;
            Some(TextChange {
                range: Some(range),
                patch: lsp_change.text,
            })
        } else {
            Some(TextChange {
                range: None,
                patch: lsp_change.text,
            })
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IndexedText<T>
where
    T: Borrow<str>,
{
    pub text: T,
    line_ranges: Vec<Range<u32>>, // u32 should be enough for upto 4GB files; show me a source file like this!
}

impl<T: Borrow<str>> IndexedText<T> {
    pub fn new(text: T) -> Self {
        let mut line_ranges: Vec<Range<u32>> = Vec::new();

        let mut line_start: Option<usize> = None;
        let mut last_char: Option<(usize, char)> = None;

        let mut char_iter = text.borrow().char_indices().peekable();

        while let Some((pos, c)) = char_iter.next() {
            if line_start.is_none() {
                line_start = Some(pos);
            }
            last_char = Some((pos, c));

            let mut is_newline = false;

            if c == '\n' {
                is_newline = true;
            } else if c == '\r' {
                if char_iter.peek().filter(|(_, pc)| *pc == '\n').is_some() {
                    continue;
                }
                is_newline = true;
            }

            if is_newline {
                let start = line_start.expect("line_start should be always initialized");
                debug_assert!(
                    text.borrow().is_char_boundary(start),
                    "Start is not at char boundary"
                );
                let end = pos + c.len_utf8();
                debug_assert!(
                    text.borrow().is_char_boundary(end),
                    "End is not at char boundary"
                );

                line_ranges.push(start as u32..end as u32);
                line_start = None;
            }
        }

        // Handle a situation when there's no newline at the end
        if let (Some(start), Some((pos, c))) = (line_start, last_char) {
            line_ranges.push(start as u32..(pos + c.len_utf8()) as u32);
        }

        // Insert an artificial blank line with an empty range
        if let Some((pos, c)) = last_char {
            line_ranges.push((pos + c.len_utf8()) as u32..(pos + c.len_utf8()) as u32);
        }

        // Insert an artificial blank line for an empty string
        if text.borrow().is_empty() {
            line_ranges.push(0..0);
        }

        IndexedText { text, line_ranges }
    }

    fn offset_to_line(&self, offset: usize) -> Option<u32> {
        if offset > self.text.borrow().len() {
            return None;
        } else if offset == self.text.borrow().len() {
            Some((self.line_ranges.len().max(2) - 2) as u32)
        } else {
            let line = self.line_ranges.binary_search_by(|r| {
                if offset < r.start as usize {
                    Ordering::Greater
                } else if offset >= r.end as usize {
                    Ordering::Less
                } else if offset >= r.start as usize && offset < r.end as usize {
                    Ordering::Equal
                } else {
                    panic!("Impossible case: offset={} and range={:?}", offset, r)
                }
            });
            Some(
                line.unwrap_or_else(|_| panic!("Couldn't translate u8 offset {} to line", offset))
                    as u32,
            )
        }
    }

    fn pos_to_offset(&self, pos: &Pos) -> Option<usize> {
        let line_range = self.line_ranges.get(pos.line as usize)?;
        Some(line_range.start as usize + (pos.col as usize))
    }
}

impl<T: Borrow<str>> TextMap for IndexedText<T> {
    fn text(&self) -> &str {
        self.text.borrow()
    }

    fn offset_to_pos(&self, offset: usize) -> Option<Pos> {
        let line = self.offset_to_line(offset)?;
        let range = &self.line_ranges[line as usize];
        let char = offset - (range.start as usize);
        Some(Pos {
            line,
            col: char as u32,
        })
    }

    fn line_range(&self, line: u32) -> Option<Range<Pos>> {
        let offset = self.line_ranges.get(line as usize)?;
        Some(Pos::new(line, 0)..Pos::new(line, offset.end - offset.start))
    }

    fn substr(&self, range: Range<Pos>) -> Option<&str> {
        let start_line = self.line_ranges.get(range.start.line as usize)?;
        let end_line = self.line_ranges.get(range.end.line as usize)?;
        let start_offset = start_line.start + range.start.col;
        let end_offset = end_line.start + range.end.col;

        Some(&self.text()[start_offset as usize..end_offset as usize])
    }
}

pub fn apply_change<S: Borrow<str>>(text: &IndexedText<S>, change: TextChange) -> String {
    match change.range {
        None => change.patch.to_string(),
        Some(range) => {
            let orig = text.text();

            let offset_start = text.pos_to_offset(&range.start).unwrap();
            let offset_end = text.pos_to_offset(&range.end).unwrap();
            debug_assert!(
                offset_start <= offset_end,
                "Expected start <= end, got {}..{}",
                offset_start,
                offset_end
            );
            debug_assert!(
                offset_end <= orig.len(),
                "Expected end <= text.len(), got {} > {}",
                offset_end,
                orig.len()
            );

            let mut new = orig.to_string();

            if offset_start == text.text().len() {
                new.push_str(&change.patch);
            } else {
                new.replace_range(offset_start..offset_end, &change.patch)
            }
            new
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use k9::assert_equal;

    mod offset_map {
        use super::*;

        #[test]
        fn no_newline() {
            //          012
            let text = "Hi!";
            let offsets = IndexedText::new(text);
            assert_equal!(offsets.line_ranges, vec![0..3, 3..3]);
        }

        #[test]
        fn newline() {
            //          012 3
            let text = "Hi!\n";
            let offsets = IndexedText::new(text);
            assert_equal!(offsets.line_ranges, vec![0..4, 4..4]);
        }

        #[test]
        fn win_newline() {
            //          012 3 4
            let text = "Hi!\r\n";
            let offsets = IndexedText::new(text);
            assert_equal!(offsets.line_ranges, vec![0..5, 5..5]);
        }

        #[test]
        fn two_lines() {
            //          012 345678
            let text = "Hi!\nWorld";
            let offsets = IndexedText::new(text);
            assert_equal!(offsets.line_ranges, vec![0..4, 4..9, 9..9]);
        }

        #[test]
        fn eof_to_lsp() {
            let text = "H";
            let text = IndexedText::new(text);
            let pos = text.offset_to_pos(1).unwrap();
            let lsp_pos = text.pos_to_lsp_pos(&pos);
            assert_equal!(lsp_pos, Some(lsp_types::Position::new(0, 1)));
        }

        #[test]
        fn empty_lsp() {
            let text = "";
            let text = IndexedText::new(text);
            let pos = text.offset_to_pos(0).unwrap();
            assert_equal!(
                text.pos_to_lsp_pos(&pos),
                Some(lsp_types::Position::new(0, 0))
            );
        }
    }

    mod apply_change {
        use lsp_types::TextDocumentContentChangeEvent;

        use super::*;
        #[test]
        fn within_line() {
            let text = "# Hello World";
            let text = IndexedText::new(text);

            let change = TextDocumentContentChangeEvent {
                range: Some(lsp_types::Range::new(
                    lsp_types::Position::new(0, 2),
                    lsp_types::Position::new(0, 7),
                )),
                range_length: None,
                text: "Hi".to_string(),
            };
            let change = text.lsp_change_to_change(change).unwrap();
            let replaced = apply_change(&text, change);
            assert_equal!(&replaced, "# Hi World");
        }

        #[test]
        fn at_newline() {
            //          01 2
            let text = "Hi\n";
            let text = IndexedText::new(text);

            let change = TextDocumentContentChangeEvent {
                range: Some(lsp_types::Range::new(
                    lsp_types::Position::new(0, 2),
                    lsp_types::Position::new(1, 0),
                )),
                range_length: None,
                text: "".to_string(),
            };
            let change = text.lsp_change_to_change(change).unwrap();
            let replaced = apply_change(&text, change);
            assert_equal!(&replaced, "Hi");
        }

        #[test]
        fn at_linend() {
            //          01
            let text = "Hi";
            let text = IndexedText::new(text);

            let change = TextDocumentContentChangeEvent {
                range: Some(lsp_types::Range::new(
                    lsp_types::Position::new(0, 2),
                    lsp_types::Position::new(0, 2),
                )),
                range_length: None,
                text: "\n".to_string(),
            };
            let change = text.lsp_change_to_change(change).unwrap();
            let replaced = apply_change(&text, change);
            assert_equal!(&replaced, "Hi\n");
        }
    }
}

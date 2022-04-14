use std::{
    fmt::{Debug, Display},
    iter::Peekable,
    ops::Range,
    path::{Path, PathBuf},
};

use lsp_document::{Pos, TextMap};
use pulldown_cmark::{BrokenLink, CowStr, Event, LinkType, OffsetIter, Options, Parser, Tag};
use serde::{Deserialize, Serialize};

pub const LINK_PREFIX_1: &str = "[:";
pub const LINK_SUFFIX: char = ']';
pub const START_COLON: char = ':';
pub const SEP_AT: char = '@';
pub const SEP_BAR: char = '|';

pub type ElementWithLoc = (Element, Range<Pos>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Element {
    Heading(Heading),
    ExternLink(ExternLink),
    InternLink(InternLink),
}

impl Element {
    pub fn text(&self) -> &str {
        match self {
            Element::Heading(hd) => &hd.text,
            Element::ExternLink(el) => &el.text,
            Element::InternLink(il) => &il.text,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Heading {
    pub level: u8,
    pub text: String,
    pub scope: Range<Pos>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InternLink {
    pub text: String,
    pub note_name: Option<NoteName>,
    pub heading: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExternLink {
    text: String,
    dest: Option<String>,
    title: Option<String>,
}

#[derive(Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
pub struct NoteName(String);

impl From<String> for NoteName {
    fn from(name: String) -> Self {
        Self(name)
    }
}

impl From<&str> for NoteName {
    fn from(name: &str) -> Self {
        name.to_string().into()
    }
}

impl Debug for NoteName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

impl Display for NoteName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

impl NoteName {
    pub fn from_path(path: &Path, root: &Path) -> NoteName {
        let rel = path.strip_prefix(root).unwrap();
        let stem = rel.with_extension("");
        stem.to_string_lossy().to_string().into()
    }

    pub fn to_path(&self, root: &Path) -> PathBuf {
        root.join(&self.0).with_extension("md")
    }

    pub fn to_str(&self) -> &str {
        &self.0
    }
}

pub fn parse_intern_link(text: &str) -> Option<InternLink> {
    if text.starts_with(LINK_PREFIX_1) && text.ends_with(LINK_SUFFIX) {
        let content = text
            .trim_start_matches(LINK_PREFIX_1)
            .trim_end_matches(LINK_SUFFIX);
        let (name, heading) = match content.split_once([SEP_AT, SEP_BAR]) {
            Some((n, h)) => (n, h),
            _ => (content, ""),
        };
        let name = Some(name)
            .filter(|s| !s.trim().is_empty())
            .map(str::to_string);
        let heading = Some(heading)
            .filter(|s| !s.trim().is_empty())
            .map(str::to_string);

        Some(InternLink {
            text: text.to_string(),
            note_name: name.map(Into::into),
            heading,
        })
    } else {
        None
    }
}

pub fn parse_link_regular(text: &str, dest: CowStr, title: CowStr) -> ExternLink {
    let text = text.to_string();
    let dest = if dest.is_empty() {
        None
    } else {
        Some(dest.to_string())
    };
    let title = if title.is_empty() {
        None
    } else {
        Some(title.to_string())
    };
    ExternLink { text, dest, title }
}

type ParseIter<'a, 'b> = Peekable<OffsetIter<'a, 'b>>;

pub fn scrape(index: &impl TextMap) -> Vec<ElementWithLoc> {
    let mut callback = |_: BrokenLink<'_>| Some(("".into(), "".into()));
    let parser =
        Parser::new_with_broken_link_callback(index.text(), Options::all(), Some(&mut callback));
    let stop_when = |_: &Event<'_>| false;

    let mut iter: ParseIter<'_, '_> = parser.into_offset_iter().peekable();
    let mut elements = scrape_document(index, &mut iter, stop_when);
    elements.sort_by_key(|(_, span)| span.start);
    elements
}

fn scrape_document<'a, 'b>(
    index: &impl TextMap,
    iter: &mut ParseIter<'a, 'b>,
    stop_when: impl Fn(&Event<'a>) -> bool,
) -> Vec<ElementWithLoc> {
    let mut elements = Vec::new();

    while iter.peek().is_some() {
        let (next_event, _) = iter.peek().unwrap();
        if stop_when(next_event) {
            return elements;
        }

        let (next_event, next_span) = iter.next().unwrap();
        match next_event {
            Event::Start(next_tag) => {
                let block_elements = scrape_block(index, &next_tag, next_span, iter);
                elements.extend(block_elements);
            }
            Event::End(tag) => {
                let text_span = index.substr(index.offset_range_to_range(next_span).unwrap());
                panic!(
                    "scrape_document: unexpected end event: {:?}\nText for event: {:?}",
                    tag, text_span
                );
            }
            Event::Text(txt) => {
                let partial_links = scrape_partial_links(index, txt, next_span);
                elements.extend(partial_links);
            }
            _ => (),
        }
    }

    elements
}

fn scrape_block<'a, 'b>(
    index: &impl TextMap,
    start_tag: &Tag<'a>,
    start_span: Range<usize>,
    iter: &mut ParseIter<'a, 'b>,
) -> Vec<ElementWithLoc> {
    match start_tag {
        Tag::Heading(..) => scrape_heading(index, start_tag, start_span, iter),
        Tag::Link(..) => scrape_link(index, start_tag, start_span, iter),
        Tag::Paragraph | Tag::List(..) | Tag::Item => {
            let stop_when =
                |seen_event: &Event<'_>| matches!(seen_event, Event::End(t) if t == start_tag);
            let par_elements = scrape_document(index, iter, stop_when);

            let par_end_event = iter.next();
            if let Some((Event::End(_), _)) = par_end_event {
                par_elements
            } else {
                panic!(
                    "scrape_block: expected end event, actual: {:?}",
                    par_end_event
                );
            }
        }
        _ => {
            skip_block(start_tag, iter);
            Vec::new()
        }
    }
}

fn scrape_heading<'a, 'b>(
    index: &impl TextMap,
    start_tag: &Tag<'a>,
    start_span: Range<usize>,
    iter: &mut ParseIter<'a, 'b>,
) -> Vec<ElementWithLoc> {
    let mut elements = Vec::new();

    let stop_when = |seen_event: &Event<'_>| matches!(seen_event, Event::End(t) if t == start_tag);
    elements.extend(scrape_document(index, iter, stop_when));

    let end_event = iter.next();
    if let Some((Event::End(Tag::Heading(level, ..)), end_span)) = end_event {
        let heading_text = &index.text()[start_span.start..start_span.end];

        // Trim newlines, whitespaces on the right
        let trim_right_text = heading_text.trim_end().to_string();
        let trimmed_on_right = heading_text.len() - trim_right_text.len();
        let heading_span = start_span.start..(start_span.end - trimmed_on_right);

        let heading = Heading {
            level: level as u8,
            text: trim_right_text,
            scope: index
                .offset_range_to_range(start_span.start..end_span.end)
                .unwrap(),
        };
        elements.push((
            Element::Heading(heading),
            index.offset_range_to_range(heading_span).unwrap(),
        ));
    } else {
        panic!(
            "scrape_heading: expected a heading end event, actual is: {:?}",
            end_event
        );
    }

    elements
}

fn scrape_link<'a, 'b>(
    index: &impl TextMap,
    start_tag: &Tag<'a>,
    start_span: Range<usize>,
    iter: &mut ParseIter<'a, 'b>,
) -> Vec<ElementWithLoc> {
    let mut elements = Vec::new();

    match start_tag {
        Tag::Link(typ, dest, title) => match typ {
            LinkType::Inline
            | LinkType::Reference
            | LinkType::ReferenceUnknown
            | LinkType::Collapsed
            | LinkType::CollapsedUnknown
            | LinkType::Shortcut
            | LinkType::ShortcutUnknown => {
                let link_text = &index.text()[start_span.start..start_span.end].trim();
                let link = parse_intern_link(link_text)
                    .map(Element::InternLink)
                    .unwrap_or_else(|| {
                        Element::ExternLink(parse_link_regular(
                            link_text,
                            dest.clone(),
                            title.clone(),
                        ))
                    });
                elements.push((link, index.offset_range_to_range(start_span).unwrap()));
            }
            _ => (),
        },
        _ => panic!("scrape_link: unexpected link tag: {:?}", start_tag),
    }
    skip_block(start_tag, iter);

    elements
}

fn skip_block<'a, 'b>(tag: &Tag<'a>, iter: &mut ParseIter<'a, 'b>) {
    for (event, _) in iter {
        match event {
            Event::End(end_tag) if end_tag == *tag => break,
            _ => (),
        }
    }
}

fn scrape_partial_links(
    _index: &impl TextMap,
    _txt: CowStr,
    _span: Range<usize>,
) -> Vec<ElementWithLoc> {
    Vec::new()
}

#[cfg(test)]
mod test {
    use anyhow::Result;
    use lsp_document::IndexedText;

    use super::*;
    use pretty_assertions::assert_eq;
    use std::{fs, io, path::PathBuf};

    fn read_resource(name: &str) -> io::Result<String> {
        let mut root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        root.push("tests");
        root.push("resources");
        root.push(name);

        fs::read_to_string(&root)
    }

    #[test]
    fn test_intern_link() {
        let parsed = parse_intern_link("[:]");
        assert_eq!(
            Some(InternLink {
                text: "[:]".to_string(),
                note_name: None,
                heading: None
            }),
            parsed
        );
    }

    #[test]
    fn scrape_note() -> Result<()> {
        let text = IndexedText::new(read_resource("example1.md")?);
        let elements = scrape(&text);
        insta::assert_debug_snapshot!(elements);
        Ok(())
    }

    #[test]
    fn scrape_eof() {
        let elements = scrape(&IndexedText::new("#"));
        assert_eq!(
            elements,
            vec![(
                Element::Heading(Heading {
                    level: 1,
                    text: "#".to_string(),
                    scope: Pos::new(0, 0)..Pos::new(0, 1)
                }),
                Pos::new(0, 0)..Pos::new(0, 1)
            )]
        );
    }
}

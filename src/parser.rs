use std::{
    fmt::{Debug, Display},
    iter::Peekable,
    ops::{Deref, DerefMut, Range},
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Element {
    Heading(Node<Heading>),
    ExternLink(Node<ExternLink>),
    InternLink(Node<InternLink>),
}

impl From<Node<InternLink>> for Element {
    fn from(v: Node<InternLink>) -> Self {
        Self::InternLink(v)
    }
}

impl From<Node<ExternLink>> for Element {
    fn from(v: Node<ExternLink>) -> Self {
        Self::ExternLink(v)
    }
}

impl From<Node<Heading>> for Element {
    fn from(v: Node<Heading>) -> Self {
        Self::Heading(v)
    }
}

impl Element {
    pub fn text(&self) -> &str {
        match self {
            Element::Heading(hd) => &hd.text,
            Element::ExternLink(el) => &el.text,
            Element::InternLink(il) => &il.text,
        }
    }

    pub fn span(&self) -> &Range<Pos> {
        match self {
            Element::Heading(n) => &n.span,
            Element::ExternLink(n) => &n.span,
            Element::InternLink(n) => &n.span,
        }
    }

    pub fn as_heading(&self) -> Option<&Node<Heading>> {
        if let Self::Heading(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_heading_mut(&mut self) -> Option<&mut Node<Heading>> {
        if let Self::Heading(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Node<E> {
    pub span: Range<Pos>,
    pub inner: E,
}

impl<E> Node<E> {
    pub fn new(inner: E, span: Range<Pos>) -> Self {
        Self { inner, span }
    }
}

impl<E> Deref for Node<E> {
    type Target = E;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<E> DerefMut for Node<E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Heading {
    pub level: u8,
    pub text: String,
    pub scope: Range<Pos>,
    pub children: Vec<Element>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct InternLink {
    pub text: String,
    pub note_name: Option<NoteName>,
    pub heading: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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

pub fn scrape(index: &impl TextMap) -> Vec<Element> {
    let mut callback = |_: BrokenLink<'_>| Some(("".into(), "".into()));
    let parser =
        Parser::new_with_broken_link_callback(index.text(), Options::all(), Some(&mut callback));
    let stop_when = |_: &Event<'_>| false;

    let mut iter: ParseIter<'_, '_> = parser.into_offset_iter().peekable();
    let mut elements = scrape_document(index, &mut iter, stop_when);

    sort_elements(&mut elements);
    elements
}

fn scrape_document<'a, 'b>(
    index: &impl TextMap,
    iter: &mut ParseIter<'a, 'b>,
    stop_when: impl Fn(&Event<'a>) -> bool,
) -> Vec<Element> {
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
) -> Vec<Element> {
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
) -> Vec<Element> {
    let mut elements = Vec::new();
    let current_heading_level = if let Tag::Heading(level, ..) = start_tag {
        level
    } else {
        panic!("Unexpected start tag for heading: {:?}", start_tag)
    };

    // Process the heading block and advance the iterator
    let stop_when = |seen_event: &Event<'_>| matches!(seen_event, Event::End(t) if t == start_tag);
    elements.extend(scrape_document(index, iter, stop_when));
    let heading_end = iter.next();
    assert!(
        matches!(heading_end, Some((Event::End(_), _))),
        "scrape_heading: expected a heading end event, actual is: {:?}",
        heading_end
    );

    // Process all child elements (until the heading of <= level)
    let stop_when = |seen_event: &Event<'_>| matches!(seen_event, Event::Start(Tag::Heading(level, ..)) if level <= current_heading_level);
    elements.extend(scrape_document(index, iter, stop_when));

    let next_section_event = iter.peek();
    let end_offset = match next_section_event {
        Some(next_el) => next_el.1.start,
        _ => index.text().len(),
    };

    let heading_text = &index.text()[start_span.start..start_span.end];

    // Trim newlines, whitespaces on the right
    let trim_right_text = heading_text.trim_end().to_string();
    let trimmed_on_right = heading_text.len() - trim_right_text.len();
    let heading_span = start_span.start..(start_span.end - trimmed_on_right);

    let heading = Heading {
        level: *current_heading_level as u8,
        text: trim_right_text,
        scope: index
            .offset_range_to_range(start_span.start..end_offset)
            .unwrap(),
        children: elements,
    };
    vec![Node::new(heading, index.offset_range_to_range(heading_span).unwrap()).into()]
}

fn scrape_link<'a, 'b>(
    index: &impl TextMap,
    start_tag: &Tag<'a>,
    start_span: Range<usize>,
    iter: &mut ParseIter<'a, 'b>,
) -> Vec<Element> {
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
                let pos_span = index.offset_range_to_range(start_span.clone()).unwrap();
                let link_text = index.text()[start_span].trim();
                let link = parse_intern_link(link_text)
                    .map(|l| Node::new(l, pos_span.clone()).into())
                    .unwrap_or_else(|| {
                        let fallback_extern_link =
                            parse_link_regular(link_text, dest.clone(), title.clone());
                        Node::new(fallback_extern_link, pos_span).into()
                    });
                elements.push(link);
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

fn scrape_partial_links(_index: &impl TextMap, _txt: CowStr, _span: Range<usize>) -> Vec<Element> {
    Vec::new()
}

fn sort_elements(elements: &mut [Element]) {
    elements.sort_by_key(|e| e.span().start);
    for e in elements {
        if let Some(heading) = e.as_heading_mut() {
            sort_elements(&mut heading.children);
        }
    }
}

#[cfg(test)]
mod test {
    use anyhow::Result;
    use lsp_document::{IndexedText, Pos};

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
            vec![Element::Heading(Node::new(
                Heading {
                    level: 1,
                    text: "#".to_string(),
                    scope: Pos::new(0, 0)..Pos::new(0, 1),
                    children: vec![]
                },
                Pos::new(0, 0)..Pos::new(0, 1)
            ))]
        );
    }

    #[test]
    fn scrape_no_heading() {
        let text = r#"
        Text before heading
        "#;
        let elements = scrape(&IndexedText::new(text));
        insta::assert_debug_snapshot!(elements);
    }

    #[test]
    fn scrape_link_before_heading() {
        let text = r#"Text; [:link]"#;
        let elements = scrape(&IndexedText::new(text));
        insta::assert_debug_snapshot!(elements);
    }

    #[test]
    fn scrape_only_title() {
        let text = r#"# Title"#;
        let elements = scrape(&IndexedText::new(text));
        insta::assert_debug_snapshot!(elements);
    }
}

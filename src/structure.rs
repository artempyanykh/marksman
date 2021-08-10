use lsp_document::{Pos, TextMap};
use regex::Regex;

use std::{
    fmt::{Debug, Display},
    ops::Range,
    path::{Path, PathBuf},
    sync::Arc,
};

use pulldown_cmark::{BrokenLink, CowStr, Event, LinkType, Options, Parser, Tag};

use serde::{Deserialize, Serialize};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NoteID(u32);

impl NoteID {
    pub fn to_u32(&self) -> u32 {
        self.0
    }

    pub fn to_usize(&self) -> usize {
        self.to_u32() as usize
    }
}

impl From<usize> for NoteID {
    fn from(idx: usize) -> Self {
        NoteID(idx as u32)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Structure {
    elements: Arc<[ElementWithLoc]>,
}

impl Structure {
    pub fn new(elements: Vec<ElementWithLoc>) -> Self {
        Self {
            elements: elements.into(),
        }
    }

    pub fn elements(&self) -> Vec<ElementID> {
        let mut els = Vec::with_capacity(self.elements.len());
        for (idx, (el, _)) in self.elements.iter().enumerate() {
            match el {
                Element::Heading(..) => els.push(ElementID::Heading(HeadingID(idx as u32))),
                Element::LinkRef(..) => els.push(ElementID::Ref(LinkRefID(idx as u32))),
                Element::LinkRegular(..) => (),
            }
        }

        els
    }

    pub fn elements_with_loc(&self) -> Vec<(ElementID, &ElementWithLoc)> {
        let mut els = Vec::with_capacity(self.elements.len());
        for (idx, ewl) in self.elements.iter().enumerate() {
            match ewl.0 {
                Element::Heading(..) => els.push((ElementID::Heading(HeadingID(idx as u32)), ewl)),
                Element::LinkRef(..) => els.push((ElementID::Ref(LinkRefID(idx as u32)), ewl)),
                Element::LinkRegular(..) => (),
            }
        }

        els
    }

    pub fn elements_with_ids<'a, 'b: 'a>(
        &'a self,
        ids: &'b [ElementID],
    ) -> impl Iterator<Item = &'a ElementWithLoc> {
        ids.iter().map(move |id| &self.elements[id.to_usize()])
    }

    pub fn headings(&self) -> Vec<HeadingID> {
        let mut headings = Vec::new();
        for (idx, (el, _)) in self.elements.iter().enumerate() {
            if let Element::Heading(..) = el {
                headings.push(HeadingID(idx as u32))
            }
        }

        headings
    }

    pub fn element_by_id(&self, id: ElementID) -> &ElementWithLoc {
        &self.elements[id.to_usize()]
    }

    pub fn heading_by_id(&self, id: HeadingID) -> (&Heading, Range<Pos>) {
        let el = &self.elements[id.0 as usize];
        if let (Element::Heading(hd), span) = el {
            (hd, span.clone())
        } else {
            panic!("Expected a heading at idx {:?} in {:?}", id, self.elements)
        }
    }

    pub fn headings_with_ids(&self, ids: &[HeadingID]) -> Vec<(&Heading, Range<Pos>)> {
        ids.iter().map(move |&id| self.heading_by_id(id)).collect()
    }

    pub fn refs(&self) -> Vec<LinkRefID> {
        let mut refs = Vec::new();
        for (idx, (el, _)) in self.elements.iter().enumerate() {
            if let Element::LinkRef(..) = el {
                refs.push(LinkRefID(idx as u32))
            }
        }

        refs
    }

    pub fn ref_by_id(&self, id: LinkRefID) -> (&LinkRef, Range<Pos>) {
        let el = &self.elements[id.0 as usize];
        if let (Element::LinkRef(lr), span) = el {
            (lr, span.clone())
        } else {
            panic!("Expected a ref at idx {:?} in {:?}", id, self.elements)
        }
    }

    pub fn refs_with_ids(&self, ids: &[LinkRefID]) -> Vec<(&LinkRef, Range<Pos>)> {
        ids.iter().map(move |&id| self.ref_by_id(id)).collect()
    }
}

pub type ElementWithLoc = (Element, Range<Pos>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HeadingID(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LinkRefID(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ElementID {
    Heading(HeadingID),
    Ref(LinkRefID),
}

impl ElementID {
    pub fn to_u32(&self) -> u32 {
        match self {
            ElementID::Heading(HeadingID(id)) => *id,
            ElementID::Ref(LinkRefID(id)) => *id,
        }
    }

    pub fn to_usize(&self) -> usize {
        self.to_u32() as usize
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Element {
    Heading(Heading),
    LinkRegular(LinkRegular),
    LinkRef(LinkRef),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Heading {
    pub level: u8,
    pub text: String,
    pub scope: Range<Pos>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LinkRef {
    pub text: String,
    pub note_name: Option<NoteName>,
    pub heading: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LinkRegular {
    text: String,
    dest: Option<String>,
    title: Option<String>,
}

pub fn parse_link_ref(text: &str) -> Option<LinkRef> {
    let ref_link_regex = Regex::new(r"^\[:([^@]*)(@(.*))?\]$").unwrap();

    if let Some(captures) = ref_link_regex.captures(text) {
        let text = text.to_string();
        let note_name = captures
            .get(1)
            .map(|m| m.as_str())
            .filter(|s| !s.is_empty())
            .map(|s| s.into());
        let heading = captures
            .get(3)
            .map(|m| m.as_str().to_string())
            .filter(|s| !s.is_empty());
        Some(LinkRef {
            text,
            note_name,
            heading,
        })
    } else {
        None
    }
}

pub fn parse_link_regular(text: &str, dest: CowStr, title: CowStr) -> LinkRegular {
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
    LinkRegular { text, dest, title }
}

pub fn scrape(index: &impl TextMap) -> Vec<ElementWithLoc> {
    let mut callback = |_: BrokenLink<'_>| Some(("".into(), "".into()));
    let parser =
        Parser::new_with_broken_link_callback(index.text(), Options::all(), Some(&mut callback));
    let mut elements = Vec::new();

    let mut scoped_headings: Vec<(u8, String, Range<usize>)> = Vec::new();

    for (event, el_span) in parser.into_offset_iter() {
        match event {
            Event::Start(Tag::Heading(level)) => {
                let heading_text = &index.text()[el_span.start..el_span.end];

                // Trim newlines, whitespaces on the right
                let trim_right_text = heading_text.trim_end().to_string();
                let trimmed_on_right = heading_text.len() - trim_right_text.len();
                let heading_span = el_span.start..(el_span.end - trimmed_on_right);

                while let Some(last) = scoped_headings.last() {
                    if last.0 >= level as u8 {
                        let last = scoped_headings.pop().unwrap();
                        let heading = Heading {
                            level: last.0,
                            text: last.1,
                            scope: index
                                .offset_range_to_range(last.2.start..el_span.start)
                                .unwrap(),
                        };
                        elements.push((
                            Element::Heading(heading),
                            index.offset_range_to_range(last.2).unwrap(),
                        ));
                    } else {
                        break;
                    }
                }

                scoped_headings.push((level as u8, trim_right_text, heading_span));
            }
            Event::Start(Tag::Link(typ, dest, title)) => match typ {
                LinkType::Inline
                | LinkType::Reference
                | LinkType::ReferenceUnknown
                | LinkType::Collapsed
                | LinkType::CollapsedUnknown
                | LinkType::Shortcut
                | LinkType::ShortcutUnknown => {
                    let link_text = &index.text()[el_span.start..el_span.end].trim();
                    let link = parse_link_ref(link_text)
                        .map(Element::LinkRef)
                        .unwrap_or_else(|| {
                            Element::LinkRegular(parse_link_regular(link_text, dest, title))
                        });
                    elements.push((link, index.offset_range_to_range(el_span).unwrap()));
                }
                _ => (),
            },
            _ => (),
        }
    }

    for remaining in scoped_headings {
        let heading = Heading {
            level: remaining.0,
            text: remaining.1,
            scope: index
                .offset_range_to_range(remaining.2.start..index.text().len())
                .unwrap(),
        };
        elements.push((
            Element::Heading(heading),
            index.offset_range_to_range(remaining.2).unwrap(),
        ));
    }

    elements.sort_by_key(|(_, span)| span.start);

    elements
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

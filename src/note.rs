use regex::Regex;

use std::{ops::Range, path::Path};

use pulldown_cmark::{BrokenLink, CowStr, Event, LinkType, Options, Parser, Tag};

pub type NoteID = String;

pub fn note_id_from_path(path: &Path, root: &Path) -> NoteID {
    let rel = path.strip_prefix(root).unwrap();
    let stem = rel.with_extension("");
    stem.to_string_lossy().to_string()
}

pub type ElementWithLoc = (Element, Range<usize>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Element {
    Heading(Heading),
    LinkRegular(LinkRegular),
    LinkRef(LinkRef),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Heading {
    pub level: u8,
    pub text: String,
    pub scope: Range<usize>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LinkRef {
    pub text: String,
    pub note_id: Option<String>,
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
        let note_id = captures
            .get(1)
            .map(|m| m.as_str().to_string())
            .filter(|s| !s.is_empty());
        let heading = captures
            .get(3)
            .map(|m| m.as_str().to_string())
            .filter(|s| !s.is_empty());
        Some(LinkRef {
            text,
            note_id,
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

pub fn scrape(text: &str) -> Vec<ElementWithLoc> {
    let mut callback = |_: BrokenLink<'_>| Some(("".into(), "".into()));
    let parser = Parser::new_with_broken_link_callback(text, Options::all(), Some(&mut callback));
    let mut elements = Vec::new();

    let mut scoped_headings: Vec<(u8, String, Range<usize>)> = Vec::new();

    for (event, el_span) in parser.into_offset_iter() {
        match event {
            Event::Start(Tag::Heading(level)) => {
                let heading_text = &text[el_span.start..el_span.end];

                // Trim newlines, whitespaces on the right
                let trim_right_text = heading_text.trim_end().to_string();
                let trimmed_on_right = heading_text.len() - trim_right_text.len();
                let heading_span = el_span.start..(el_span.end - trimmed_on_right);

                while let Some(last) = scoped_headings.last() {
                    if last.0 > level as u8 {
                        let last = scoped_headings.pop().unwrap();
                        let heading = Heading {
                            level: last.0,
                            text: last.1,
                            scope: last.2.start..el_span.start,
                        };
                        elements.push((Element::Heading(heading), last.2));
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
                    let link_text = text[el_span.start..el_span.end].trim();
                    let link = parse_link_ref(link_text)
                        .map(|r| Element::LinkRef(r))
                        .unwrap_or_else(|| {
                            Element::LinkRegular(parse_link_regular(link_text, dest, title))
                        });
                    elements.push((link, el_span));
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
            scope: remaining.2.start..text.len(),
        };
        elements.push((Element::Heading(heading), remaining.2));
    }

    elements.sort_by_key(|(_, span)| span.start);

    elements
}

#[cfg(test)]
mod test {
    use anyhow::Result;

    use super::*;
    use k9::{assert_equal, snapshot};
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
        let text = read_resource("example1.md")?;
        let elements = scrape(&text);
        snapshot!(
            elements,
            r###"
[
    (
        Heading(
            Heading {
                level: 1,
                text: "# Some text in heading 1",
                scope: 28..634,
            },
        ),
        28..52,
    ),
    (
        Heading(
            Heading {
                level: 2,
                text: "## Some text in heading 1-2",
                scope: 56..118,
            },
        ),
        56..83,
    ),
    (
        Heading(
            Heading {
                level: 1,
                text: "#     Some text in heading 2",
                scope: 118..634,
            },
        ),
        118..146,
    ),
    (
        Heading(
            Heading {
                level: 2,
                text: "## Heading with links",
                scope: 150..634,
            },
        ),
        150..171,
    ),
    (
        LinkRef(
            LinkRef {
                text: "[:noteid]",
                note_id: Some(
                    "noteid",
                ),
                heading: None,
            },
        ),
        196..205,
    ),
    (
        LinkRef(
            LinkRef {
                text: "[:@# Some text in heading 1]",
                note_id: None,
                heading: Some(
                    "# Some text in heading 1",
                ),
            },
        ),
        235..263,
    ),
    (
        LinkRef(
            LinkRef {
                text: "[:othernote@#Some heading]",
                note_id: Some(
                    "othernote",
                ),
                heading: Some(
                    "#Some heading",
                ),
            },
        ),
        301..327,
    ),
    (
        LinkRef(
            LinkRef {
                text: "[:othernote@]",
                note_id: Some(
                    "othernote",
                ),
                heading: None,
            },
        ),
        372..385,
    ),
    (
        LinkRegular(
            LinkRegular {
                text: "[foo][bar]",
                dest: Some(
                    "https://bar.com",
                ),
                title: None,
            },
        ),
        411..421,
    ),
    (
        LinkRegular(
            LinkRegular {
                text: "[foo][non-existent]",
                dest: None,
                title: None,
            },
        ),
        450..469,
    ),
    (
        LinkRegular(
            LinkRegular {
                text: "[bar]",
                dest: Some(
                    "https://bar.com",
                ),
                title: None,
            },
        ),
        498..503,
    ),
    (
        LinkRegular(
            LinkRegular {
                text: "[foo]",
                dest: None,
                title: None,
            },
        ),
        537..542,
    ),
    (
        LinkRef(
            LinkRef {
                text: "[:]",
                note_id: None,
                heading: None,
            },
        ),
        602..605,
    ),
]
"###
        );

        Ok(())
    }

    #[test]
    fn scrape_eof() {
        let elements = scrape("#");
        assert_equal!(
            elements,
            vec![(
                Element::Heading(Heading {
                    level: 1,
                    text: "#".to_string(),
                    scope: 0..1
                }),
                0..1
            )]
        );
    }
}

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

#[derive(Debug, PartialEq, Eq)]
pub enum Element {
    Heading(Heading),
    Link(Link),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Heading {
    pub level: u8,
    pub text: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Link {
    Ref {
        text: String,
        note_id: Option<String>,
        heading: Option<String>,
    },
    Regular {
        text: String,
        dest: Option<String>,
        title: Option<String>,
    },
}

impl Link {
    pub fn parse(text: &str, dest: CowStr, title: CowStr) -> Link {
        let ref_link_regex = Regex::new(r"^\[:([^@]*)(@(.*))?\]$").unwrap();
        match ref_link_regex.captures(text) {
            Some(captures) => {
                let text = text.to_string();
                let note_id = captures
                    .get(1)
                    .map(|m| m.as_str().to_string())
                    .filter(|s| !s.is_empty());
                let heading = captures
                    .get(3)
                    .map(|m| m.as_str().to_string())
                    .filter(|s| !s.is_empty());
                Link::Ref {
                    text,
                    note_id,
                    heading,
                }
            }
            _ => {
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
                Link::Regular { text, dest, title }
            }
        }
    }
}

pub fn scrape(text: &str) -> Vec<ElementWithLoc> {
    let mut callback = |_: BrokenLink<'_>| Some(("".into(), "".into()));
    let parser = Parser::new_with_broken_link_callback(text, Options::all(), Some(&mut callback));
    let mut elements = Vec::new();

    for (event, span) in parser.into_offset_iter() {
        match event {
            Event::Start(Tag::Heading(level)) => {
                let heading_text = &text[span.start..span.end];

                // Trim newlines, whitespaces on the right
                let trim_right_text = heading_text.trim_end();
                let trimmed_on_right = heading_text.len() - trim_right_text.len();
                let heading_span = span.start..(span.end - trimmed_on_right);

                let heading = Element::Heading(Heading {
                    level: level as u8,
                    text: trim_right_text.to_string(),
                });
                elements.push((heading, heading_span));
            }
            Event::Start(Tag::Link(typ, dest, title)) => match typ {
                LinkType::Inline
                | LinkType::Reference
                | LinkType::ReferenceUnknown
                | LinkType::Collapsed
                | LinkType::CollapsedUnknown
                | LinkType::Shortcut
                | LinkType::ShortcutUnknown => {
                    let link_text = text[span.start..span.end].trim();
                    let link = Link::parse(link_text, dest, title);
                    elements.push((Element::Link(link), span));
                }
                _ => (),
            },
            _ => (),
        }
    }

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
            },
        ),
        28..52,
    ),
    (
        Heading(
            Heading {
                level: 2,
                text: "## Some text in heading 1-2",
            },
        ),
        56..83,
    ),
    (
        Heading(
            Heading {
                level: 1,
                text: "#     Some text in heading 2",
            },
        ),
        118..146,
    ),
    (
        Heading(
            Heading {
                level: 2,
                text: "## Heading with links",
            },
        ),
        150..171,
    ),
    (
        Link(
            Ref {
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
        Link(
            Ref {
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
        Link(
            Ref {
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
        Link(
            Ref {
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
        Link(
            Regular {
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
        Link(
            Regular {
                text: "[foo][non-existent]",
                dest: None,
                title: None,
            },
        ),
        450..469,
    ),
    (
        Link(
            Regular {
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
        Link(
            Regular {
                text: "[foo]",
                dest: None,
                title: None,
            },
        ),
        537..542,
    ),
    (
        Link(
            Ref {
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
                    text: "#".to_string()
                }),
                0..1
            )]
        );
    }
}

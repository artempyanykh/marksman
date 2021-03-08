use std::ops::Range;

use pulldown_cmark::{Event, Parser, Tag};

use crate::note::Element;

pub type ElementWithLoc = (Element, Range<usize>);

pub fn scrape(text: &str) -> Vec<ElementWithLoc> {
    let parser = Parser::new(text);
    let mut elements = Vec::new();

    let mut cur_heading_start = None;

    for (event, span) in parser.into_offset_iter() {
        match event {
            Event::Start(Tag::Heading(level)) => cur_heading_start = Some((level, span)),
            Event::End(Tag::Heading(level)) => {
                let (start_level, start_span) = cur_heading_start
                    .clone()
                    .expect(&format!("Found END event for heading at {:?}", span));
                if start_level != level {
                    panic!(
                        "Mismatched heading levels {} != {} at {:?}",
                        start_level, level, span
                    );
                }

                let heading_text = &text[start_span.start..span.end].trim();
                let heading_span = start_span.start..span.end;
                let heading = Element::Heading {
                    level: level as u8,
                    text: heading_text.to_string(),
                };
                elements.push((heading, heading_span));
            }
            _ => (),
        }
    }

    elements
}

#[cfg(test)]
mod test {
    use anyhow::Result;

    use super::*;
    use std::{error::Error, fs, io, path::PathBuf};

    fn read_resource(name: &str) -> io::Result<String> {
        let mut root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        root.push("tests");
        root.push("resources");
        root.push(name);

        fs::read_to_string(&root)
    }

    #[test]
    fn scrape_headings() -> Result<(), Box<dyn Error>> {
        let text = read_resource("example1.md")?;
        let elements = scrape(&text);
        assert_eq!(elements.len(), 3);

        Ok(())
    }

    #[test]
    fn scrape_eof() {
        let elements = scrape("#");
        assert_eq!(
            elements,
            vec![(
                Element::Heading {
                    level: 1,
                    text: "#".to_string()
                },
                0..1
            )]
        );
    }
}

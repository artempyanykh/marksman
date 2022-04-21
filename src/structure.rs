use std::{fmt::Debug, sync::Arc};

use crate::parser::{Element, Heading, InternLink, Node};

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
    elements_hierarchy: Arc<[Element]>,
    elements_flat: Arc<[Element]>,
}

impl Structure {
    pub fn new(elements: Vec<Element>) -> Self {
        let elements_hierarchy: Arc<[Element]> = elements.into();
        let elements_flat: Arc<[Element]> = Self::flatten_elements(&elements_hierarchy).into();
        Self {
            elements_hierarchy,
            elements_flat,
        }
    }

    fn flatten_elements(initial: &[Element]) -> Vec<Element> {
        let mut els = Vec::with_capacity(initial.len());

        let mut to_process = Vec::with_capacity(initial.len());
        to_process.extend(Vec::from(initial));

        while let Some(el) = to_process.pop() {
            if let Element::Heading(h) = &el {
                to_process.extend(h.children.clone())
            }
            els.push(el)
        }

        els.sort_by_key(|e| e.span().start);
        els
    }

    pub fn element_ids(&self) -> Vec<ElementID> {
        let els_flat = &self.elements_flat;
        let mut ids = Vec::with_capacity(els_flat.len());

        for (idx, el) in els_flat.iter().enumerate() {
            match el {
                Element::Heading(..) => {
                    ids.push(ElementID::Heading(HeadingID(idx as u32)));
                }
                Element::InternLink(..) => {
                    ids.push(ElementID::InternLink(InternLinkID(idx as u32)))
                }
                Element::ExternLink(..) => (),
            }
        }

        ids
    }

    pub fn elements(&self) -> Vec<(ElementID, &Element)> {
        let mut els = Vec::with_capacity(self.elements_flat.len());
        for (idx, ewl) in self.elements_flat.iter().enumerate() {
            match ewl {
                Element::Heading(..) => els.push((ElementID::Heading(HeadingID(idx as u32)), ewl)),
                Element::InternLink(..) => {
                    els.push((ElementID::InternLink(InternLinkID(idx as u32)), ewl))
                }
                Element::ExternLink(..) => (),
            }
        }

        els
    }

    pub fn elements_with_ids<'a, 'b: 'a>(
        &'a self,
        ids: &'b [ElementID],
    ) -> impl Iterator<Item = &'a Element> {
        ids.iter().map(move |id| &self.elements_flat[id.to_usize()])
    }

    pub fn headings(&self) -> Vec<HeadingID> {
        let mut headings = Vec::new();
        for (idx, el) in self.elements_flat.iter().enumerate() {
            if let Element::Heading(..) = el {
                headings.push(HeadingID(idx as u32))
            }
        }

        headings
    }

    pub fn element_by_id(&self, id: ElementID) -> &Element {
        &self.elements_flat[id.to_usize()]
    }

    pub fn heading_by_id(&self, id: HeadingID) -> &Node<Heading> {
        let el = &self.elements_flat[id.0 as usize];
        if let Element::Heading(h) = el {
            h
        } else {
            panic!(
                "Expected a heading at idx {:?} in {:?}",
                id, self.elements_flat
            )
        }
    }

    pub fn headings_with_ids(&self, ids: &[HeadingID]) -> Vec<&Node<Heading>> {
        ids.iter().map(move |&id| self.heading_by_id(id)).collect()
    }

    pub fn intern_links(&self) -> Vec<InternLinkID> {
        let mut links = Vec::new();
        for (idx, el) in self.elements_flat.iter().enumerate() {
            if let Element::InternLink(..) = el {
                links.push(InternLinkID(idx as u32))
            }
        }

        links
    }

    pub fn intern_link_by_id(&self, id: InternLinkID) -> &Node<InternLink> {
        let el = &self.elements_flat[id.0 as usize];
        if let Element::InternLink(il) = el {
            il
        } else {
            panic!(
                "Expected an intern link at idx {:?} in {:?}",
                id, self.elements_flat
            )
        }
    }

    pub fn intern_links_with_ids(&self, ids: &[InternLinkID]) -> Vec<&Node<InternLink>> {
        ids.iter()
            .map(move |&id| self.intern_link_by_id(id))
            .collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HeadingID(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InternLinkID(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ElementID {
    Heading(HeadingID),
    InternLink(InternLinkID),
}

impl ElementID {
    pub fn to_u32(&self) -> u32 {
        match self {
            ElementID::Heading(HeadingID(id)) => *id,
            ElementID::InternLink(InternLinkID(id)) => *id,
        }
    }

    pub fn to_usize(&self) -> usize {
        self.to_u32() as usize
    }
}

#[cfg(test)]
mod tests {
    use crate::parser;

    use super::*;
    use lsp_document::{IndexedText, Pos};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_heading_scope() {
        let md = r#"
# Title

## Heading 2.1

Text inside heading 2.1

## Heading 2.2
## Heading 2.3
Text
inside
heading 2.3
"#;
        let index = IndexedText::new(md);
        let elements = parser::scrape(&index);
        let strukt = Structure::new(elements);
        let headings = strukt.headings_with_ids(&strukt.headings());
        let title = *headings.get(0).unwrap();
        assert_eq!(title.text, "# Title");
        assert_eq!(title.scope, Pos::new(1, 0)..Pos::new(11, 12));

        let h21 = *headings.get(1).unwrap();
        assert_eq!(h21.text, "## Heading 2.1");
        assert_eq!(h21.scope, Pos::new(3, 0)..Pos::new(7, 0));
    }
}

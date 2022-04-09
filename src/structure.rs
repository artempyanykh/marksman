use lsp_document::Pos;

use std::{
    fmt::Debug,
    ops::Range,
    sync::Arc,
};

use crate::parser::{Element, ElementWithLoc, Heading, InternLink};

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
                Element::InternLink(..) => {
                    els.push(ElementID::InternLink(InternLinkID(idx as u32)))
                }
                Element::ExternLink(..) => (),
            }
        }

        els
    }

    pub fn elements_with_loc(&self) -> Vec<(ElementID, &ElementWithLoc)> {
        let mut els = Vec::with_capacity(self.elements.len());
        for (idx, ewl) in self.elements.iter().enumerate() {
            match ewl.0 {
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

    pub fn intern_links(&self) -> Vec<InternLinkID> {
        let mut links = Vec::new();
        for (idx, (el, _)) in self.elements.iter().enumerate() {
            if let Element::InternLink(..) = el {
                links.push(InternLinkID(idx as u32))
            }
        }

        links
    }

    pub fn intern_link_by_id(&self, id: InternLinkID) -> (&InternLink, Range<Pos>) {
        let el = &self.elements[id.0 as usize];
        if let (Element::InternLink(lr), span) = el {
            (lr, span.clone())
        } else {
            panic!(
                "Expected an intern link at idx {:?} in {:?}",
                id, self.elements
            )
        }
    }

    pub fn intern_links_with_ids(&self, ids: &[InternLinkID]) -> Vec<(&InternLink, Range<Pos>)> {
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

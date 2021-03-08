pub type NoteID = String;

#[derive(Debug, PartialEq, Eq)]
pub enum Element {
    Heading {
        level: u8,
        text: String,
    },
    LinkDef {
        label: String,
        dest: String,
        title: Option<String>,
    },
    Link(Link),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Link {
    Regular {
        text: String,
        dest: String,
        title: Option<String>,
    },
    Ref {
        label: String,
        text: Option<String>,
    },
    Internal {
        note: NoteID,
        heading: Option<String>,
        title: Option<String>,
    },
}

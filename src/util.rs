/// Test the the text matches all characters in the query in order.
///
/// ```rust
/// use zeta_note::util::text_matches_query;
///
/// let text = "Hello World!";
/// assert_eq!(text_matches_query(&text, ""), true);
/// assert_eq!(text_matches_query(&text, "h"), true);
/// assert_eq!(text_matches_query(&text, "hel"), true);
/// assert_eq!(text_matches_query(&text, "hw"), true);
/// assert_eq!(text_matches_query(&text, "h!"), true);
/// assert_eq!(text_matches_query(&text, "hz"), false);
/// ```
pub fn text_matches_query(text: &str, query: &str) -> bool {
    if query.is_empty() {
        return true;
    }

    let text = text.to_lowercase();
    let query = query.to_lowercase();

    let mut start = 0;
    for c in query.chars() {
        let char_pos = text[start..].find(c);
        start = match char_pos {
            Some(pos) => start + pos + c.len_utf8(),
            _ => return false,
        };
    }

    true
}

/// Template expansion — a text-level preprocessor that monomorphizes
/// `template` blocks into concrete type-specific copies before parsing.
///
/// Syntax:
///   template T : i32 | f64 | i64 { <body using T> }
///   template (T, V) : (i32, f32) | (i64, f64) { <body using T and V> }

/// Expand all `template` blocks in `source`, returning the fully-expanded source.
pub fn expand_templates(source: &str) -> Result<String, String> {
    let mut out = String::with_capacity(source.len());
    let mut rest = source;

    while let Some(pos) = find_template_keyword(rest) {
        // Copy everything before the template block
        out.push_str(&rest[..pos]);
        rest = &rest[pos..];

        // Parse and expand this template block
        let (expanded, consumed) = expand_one(rest)?;
        out.push_str(&expanded);
        rest = &rest[consumed..];
    }

    // Remaining source (no more templates)
    out.push_str(rest);
    Ok(out)
}

/// Return only the expanded template output (no surrounding source).
pub fn expand_templates_only(source: &str) -> Result<String, String> {
    let mut out = String::new();
    let mut rest = source;

    while let Some(pos) = find_template_keyword(rest) {
        rest = &rest[pos..];
        let (expanded, consumed) = expand_one(rest)?;
        out.push_str(&expanded);
        rest = &rest[consumed..];
    }

    Ok(out)
}

// Easier to just use this instead of more sophisticated parsing
/// Find the byte offset of the next `template` keyword that appears as a whole word.
fn find_template_keyword(s: &str) -> Option<usize> {
    let keyword = "template";
    let mut search_from = 0;
    while let Some(idx) = s[search_from..].find(keyword) {
        let abs = search_from + idx;
        if is_word_boundary(s, abs, keyword.len()) {
            return Some(abs);
        }
        search_from = abs + 1;
    }
    None
}

/// Parse and expand a single template block starting at `s[0..]`.
/// Returns (expanded_text, bytes_consumed).
fn expand_one(s: &str) -> Result<(String, usize), String> {
    let after_keyword = skip_whitespace(&s["template".len()..]);

    // Parse parameter names
    let (params, after_params) = parse_params(after_keyword)?;

    // Expect ':'
    let after_colon = skip_whitespace(after_params);
    let after_colon = after_colon
        .strip_prefix(':')
        .ok_or_else(|| "template: expected ':' after parameter list".to_string())?;
    let after_colon = skip_whitespace(after_colon);

    // Parse type combinations (pipe-separated)
    let (type_combos, after_types) = parse_type_combos(after_colon, params.len())?;

    // Find the body block { ... }
    let after_types = skip_whitespace(after_types);
    let (body, after_body) = parse_brace_block(after_types)?;

    let consumed = s.len() - after_body.len();

    // Expand: for each type combination, substitute params in body
    let mut expanded = String::new();
    for combo in &type_combos {
        let mut instance = body.to_string();
        for (param, concrete_type) in params.iter().zip(combo.iter()) {
            instance = replace_whole_word(&instance, param, concrete_type);
        }
        expanded.push_str(&instance);
        expanded.push('\n');
    }

    Ok((expanded, consumed))
}

/// Parse parameter names: either `T` (single) or `(T, V, ...)` (multiple).
fn parse_params(s: &str) -> Result<(Vec<String>, &str), String> {
    if s.starts_with('(') {
        // Multi-param: (T, V, ...)
        let close = s
            .find(')')
            .ok_or_else(|| "template: unclosed '(' in parameter list".to_string())?;
        let inner = &s[1..close];
        let params: Vec<String> = inner.split(',').map(|p| p.trim().to_string()).collect();
        if params.iter().any(|p| p.is_empty()) {
            return Err("template: empty parameter name".to_string());
        }
        Ok((params, &s[close + 1..]))
    } else {
        // Single param: identifier
        let end = s
            .find(|c: char| !c.is_alphanumeric() && c != '_')
            .unwrap_or(s.len());
        if end == 0 {
            return Err("template: expected parameter name".to_string());
        }
        let param = s[..end].to_string();
        Ok((vec![param], &s[end..]))
    }
}

/// Parse pipe-separated type combinations.
/// Single param: `i32 | f64 | i64`
/// Multi param: `(i32, f32) | (i64, f64)`
fn parse_type_combos<'a>(
    s: &'a str,
    param_count: usize,
) -> Result<(Vec<Vec<String>>, &'a str), String> {
    // Collect everything up to the opening `{` of the body
    let brace_pos = find_top_level_brace(s)?;
    let types_str = s[..brace_pos].trim();
    let rest = &s[brace_pos..];

    let combos: Vec<Vec<String>> = if param_count == 1 {
        // Single param: types separated by |
        types_str
            .split('|')
            .map(|t| vec![t.trim().to_string()])
            .collect()
    } else {
        // Multi param: (t1, t2) | (t3, t4) | ...
        types_str
            .split('|')
            .map(|chunk| {
                let chunk = chunk.trim();
                let chunk = chunk
                    .strip_prefix('(')
                    .and_then(|c| c.strip_suffix(')'))
                    .ok_or_else(|| {
                        format!(
                            "template: expected parenthesized type tuple, got '{}'",
                            chunk
                        )
                    })?;
                let types: Vec<String> = chunk.split(',').map(|t| t.trim().to_string()).collect();
                if types.len() != param_count {
                    return Err(format!(
                        "template: expected {} types in tuple, got {}",
                        param_count,
                        types.len()
                    ));
                }
                Ok(types)
            })
            .collect::<Result<Vec<_>, _>>()?
    };

    if combos.is_empty() || combos.iter().any(|c| c.iter().any(|t| t.is_empty())) {
        return Err("template: empty type in type list".to_string());
    }

    Ok((combos, rest))
}

/// Find the position of the opening `{` that starts the template body,
/// skipping any `{` inside parenthesized type tuples.
fn find_top_level_brace(s: &str) -> Result<usize, String> {
    let mut paren_depth = 0;
    for (i, c) in s.char_indices() {
        match c {
            '(' => paren_depth += 1,
            ')' => paren_depth -= 1,
            '{' if paren_depth == 0 => return Ok(i),
            _ => {}
        }
    }
    Err("template: expected '{' to start body".to_string())
}

/// Extract the contents of a brace-delimited block (including nested braces).
/// `s` must start with `{`. Returns (body_content, rest_after_closing_brace).
fn parse_brace_block(s: &str) -> Result<(&str, &str), String> {
    if !s.starts_with('{') {
        return Err("template: expected '{' to start body".to_string());
    }
    let mut depth = 0;
    for (i, c) in s.char_indices() {
        match c {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    // body is between the outer braces
                    let body = &s[1..i];
                    let rest = &s[i + 1..];
                    return Ok((body, rest));
                }
            }
            _ => {}
        }
    }
    Err("template: unclosed '{'".to_string())
}

/// Replace all whole-word occurrences of `word` with `replacement` in `text`.
fn replace_whole_word(text: &str, word: &str, replacement: &str) -> String {
    let mut result = String::with_capacity(text.len());
    let mut search_from = 0;

    while let Some(idx) = text[search_from..].find(word) {
        let abs = search_from + idx;
        if is_word_boundary(text, abs, word.len()) {
            result.push_str(&text[search_from..abs]);
            result.push_str(replacement);
            search_from = abs + word.len();
        } else {
            result.push_str(&text[search_from..abs + 1]);
            search_from = abs + 1;
        }
    }
    result.push_str(&text[search_from..]);
    result
}

/// Check if the substring at `pos..pos+len` is a whole word
/// (bounded by non-alphanumeric/underscore on both sides).
fn is_word_boundary(s: &str, pos: usize, len: usize) -> bool {
    let before_ok = pos == 0 || {
        let c = s.as_bytes()[pos - 1] as char;
        !c.is_alphanumeric() && c != '_'
    };
    let after_ok = pos + len >= s.len() || {
        let c = s.as_bytes()[pos + len] as char;
        !c.is_alphanumeric() && c != '_'
    };
    before_ok && after_ok
}

fn skip_whitespace(s: &str) -> &str {
    s.trim_start()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_param() {
        let src = r#"
template T : i32 | f64 | i64 {
    fn add(a: T, b: T) -> T {
        return a + b;
    }
}
"#;
        let expanded = expand_templates(src).unwrap();
        assert!(expanded.contains("fn add(a: i32, b: i32) -> i32 {"));
        assert!(expanded.contains("fn add(a: f64, b: f64) -> f64 {"));
        assert!(expanded.contains("fn add(a: i64, b: i64) -> i64 {"));
        assert!(!expanded.contains("template"));
    }

    #[test]
    fn test_multi_param() {
        let src = r#"
template (T, V) : (i32, f32) | (i64, f64) {
    fn convert(a: T) -> V {
        return a;
    }
}
"#;
        let expanded = expand_templates(src).unwrap();
        assert!(expanded.contains("fn convert(a: i32) -> f32 {"));
        assert!(expanded.contains("fn convert(a: i64) -> f64 {"));
    }

    #[test]
    fn test_whole_word_replacement() {
        let src = r#"
template T : i32 | str {
    fn process(val: T) -> T {
        let Total: T = val;
        return Total;
    }
}
"#;
        let expanded = expand_templates(src).unwrap();
        // "Total" should NOT be mangled — only standalone "T" replaced
        assert!(expanded.contains("let Total: i32 = val;"));
        assert!(expanded.contains("let Total: str = val;"));
    }

    #[test]
    fn test_nested_braces() {
        let src = r#"
template T : i32 | f64 {
    fn check(x: T) -> bool {
        if (x > 0) {
            return true;
        }
        return false;
    }
}
"#;
        let expanded = expand_templates(src).unwrap();
        assert!(expanded.contains("fn check(x: i32) -> bool {"));
        assert!(expanded.contains("fn check(x: f64) -> bool {"));
        // Nested braces should be preserved
        assert!(expanded.contains("return true;"));
    }

    #[test]
    fn test_passthrough_no_templates() {
        let src = "fn main() -> void { putln(42); }";
        let expanded = expand_templates(src).unwrap();
        assert_eq!(expanded, src);
    }

    #[test]
    fn test_mixed_template_and_regular() {
        let src = r#"
template T : i32 | f64 {
    fn identity(x: T) -> T { return x; }
}

fn main() -> void {
    let a: i32 = identity(5);
    putln(a);
}
"#;
        let expanded = expand_templates(src).unwrap();
        assert!(expanded.contains("fn identity(x: i32) -> i32 { return x; }"));
        assert!(expanded.contains("fn identity(x: f64) -> f64 { return x; }"));
        assert!(expanded.contains("fn main() -> void {"));
    }

    #[test]
    fn test_error_unclosed_brace() {
        let src = "template T : i32 | f64 { fn broken() -> void {";
        assert!(expand_templates(src).is_err());
    }

    #[test]
    fn test_error_missing_colon() {
        let src = "template T i32 | f64 { }";
        assert!(expand_templates(src).is_err());
    }

    #[test]
    fn test_error_wrong_tuple_arity() {
        let src = "template (T, V) : (i32, f32, bool) | (i64, f64) { }";
        assert!(expand_templates(src).is_err());
    }
}

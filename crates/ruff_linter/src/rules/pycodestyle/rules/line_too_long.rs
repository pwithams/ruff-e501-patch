use regex::Regex;
use ruff_macros::{ViolationMetadata, derive_message_formats};
use ruff_python_index::Indexer;
use ruff_source_file::Line;
use ruff_text_size::{TextLen, TextRange, TextSize};

use crate::checkers::ast::LintContext;
use crate::line_width::{IndentWidth, LineLength, LineWidthBuilder};
use crate::rules::pycodestyle::overlong::Overlong;
use crate::settings::LinterSettings;
use crate::{Edit, Fix, FixAvailability, Violation};

/// ## What it does
/// Checks for lines that exceed the specified maximum character length.
///
/// ## Why is this bad?
/// Overlong lines can hurt readability. [PEP 8], for example, recommends
/// limiting lines to 79 characters. By default, this rule enforces a limit
/// of 88 characters for compatibility with Black and the Ruff formatter,
/// though that limit is configurable via the [`line-length`] setting.
///
/// In the interest of pragmatism, this rule makes a few exceptions when
/// determining whether a line is overlong. Namely, it:
///
/// 1. Ignores lines that consist of a single "word" (i.e., without any
///    whitespace between its characters).
/// 2. Ignores lines that end with a URL, as long as the URL starts before
///    the line-length threshold.
/// 3. Ignores line that end with a pragma comment (e.g., `# type: ignore`
///    or `# noqa`), as long as the pragma comment starts before the
///    line-length threshold. That is, a line will not be flagged as
///    overlong if a pragma comment _causes_ it to exceed the line length.
///    (This behavior aligns with that of the Ruff formatter.)
/// 4. Ignores SPDX license identifiers and copyright notices
///    (e.g., `# SPDX-License-Identifier: MIT`), which are machine-readable
///    and should _not_ wrap over multiple lines.
///
/// If [`lint.pycodestyle.ignore-overlong-task-comments`] is `true`, this rule will
/// also ignore comments that start with any of the specified [`lint.task-tags`]
/// (e.g., `# TODO:`).
///
/// ## Example
/// ```python
/// my_function(param1, param2, param3, param4, param5, param6, param7, param8, param9, param10)
/// ```
///
/// Use instead:
/// ```python
/// my_function(
///     param1, param2, param3, param4, param5,
///     param6, param7, param8, param9, param10
/// )
/// ```
///
/// ## Error suppression
/// Hint: when suppressing `E501` errors within multi-line strings (like
/// docstrings), the `noqa` directive should come at the end of the string
/// (after the closing triple quote), and will apply to the entire string, like
/// so:
///
/// ```python
/// """Lorem ipsum dolor sit amet.
///
/// Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor.
/// """  # noqa: E501
/// ```
///
/// ## Options
/// - `line-length`
/// - `lint.task-tags`
/// - `lint.pycodestyle.ignore-overlong-task-comments`
/// - `lint.pycodestyle.max-line-length`
///
/// [PEP 8]: https://peps.python.org/pep-0008/#maximum-line-length
#[derive(ViolationMetadata)]
pub(crate) struct LineTooLong(usize, usize);

impl Violation for LineTooLong {
    const FIX_AVAILABILITY: FixAvailability = FixAvailability::Sometimes;

    #[derive_message_formats]
    fn message(&self) -> String {
        let LineTooLong(width, limit) = self;
        format!("Line too long ({width} > {limit})")
    }

    fn fix_title(&self) -> Option<String> {
        Some("Break statement over multiple lines, or add # noqa flag".to_string())
    }
}

/// E501
pub(crate) fn line_too_long(
    line: &Line,
    prev_line: Option<&Line>,
    flagged_by_w505: bool,
    settings: &LinterSettings,
    context: &LintContext,
    indexer: &Indexer,
) {
    let limit = settings.pycodestyle.max_line_length;

    if let Some(overlong) = Overlong::try_from_line(
        line,
        indexer.comment_ranges(),
        limit,
        if settings.pycodestyle.ignore_overlong_task_comments {
            &settings.task_tags
        } else {
            &[]
        },
        settings.tab_size,
    ) {
        let mut diagnostic = context.report_diagnostic(
            LineTooLong(overlong.width(), limit.value() as usize),
            overlong.range(),
        );

        let length_limit = settings.pycodestyle.max_line_length;
        let line_type = classify_line(line, flagged_by_w505, prev_line);
        let break_result = break_line(
            line,
            &line_type,
            indexer,
            &BreakLineConfig {
                tab_size: settings.tab_size,
                line_limit: length_limit,
                break_chars: vec![' ', ','],
            },
        );

        if let Some(break_result) = break_result {
            if let Some((first_edit, additional_edits)) =
                get_line_length_edits(&break_result, &line_type)
            {
                diagnostic.set_fix(Fix::unsafe_edits(first_edit, additional_edits));
            }
        }
    }
}

enum LineTooLongType {
    CommentInline,
    CommentSeparateLine,
    StringSingleLine,
    StringSingleLineWithTrailingComma,
    StringSingleLineWithTrailingCommaParenthesis,
    StringMultiLine,
    StringSingleLineDocstring,
    StringMultiLineDocstring,
    StringParamAssignment,
    StringParamAssignmentTrailingComma,
    StringAdditionStart,
    StringDictionaryValue,
    StringDictionaryValueTrailingComma,
    Ignored,
    Unsupported,
}

impl LineTooLongType {
    fn is_single_line_string(&self) -> bool {
        matches!(
            self,
            LineTooLongType::StringSingleLine
                | LineTooLongType::StringSingleLineWithTrailingComma
                | LineTooLongType::StringSingleLineWithTrailingCommaParenthesis
                | LineTooLongType::StringAdditionStart
                | LineTooLongType::StringDictionaryValue
                | LineTooLongType::StringDictionaryValueTrailingComma
        )
    }
    fn has_trailing_comma(&self) -> bool {
        matches!(
            self,
            LineTooLongType::StringSingleLineWithTrailingComma
                | LineTooLongType::StringSingleLineWithTrailingCommaParenthesis
                | LineTooLongType::StringParamAssignmentTrailingComma
                | LineTooLongType::StringDictionaryValueTrailingComma,
        )
    }

    fn has_open_parenthesis(&self) -> bool {
        matches!(
            self,
            LineTooLongType::StringSingleLineWithTrailingCommaParenthesis
        )
    }

    fn is_dictionary(&self) -> bool {
        matches!(
            self,
            LineTooLongType::StringDictionaryValue
                | LineTooLongType::StringDictionaryValueTrailingComma
        )
    }
}

fn classify_line(line: &Line, flagged_by_w505: bool, prev_line: Option<&Line>) -> LineTooLongType {
    // note: assumes the following
    // 1. line is too long
    // 2. line has already been formatted

    // matches strings starting on new lines, ending with optional comma
    let string_pattern: Regex = Regex::new("^[ ]*[bfr]{0,1}\"[^\"]*\"[,]{0,1}$").unwrap();
    // matches lines ending with a comma
    let ends_with_comma_pattern = Regex::new(",$").unwrap();
    // matches lines containing a single opening parenthesis
    let open_parenthesis_pattern = Regex::new(r"^[ ]*\($").unwrap();
    // matches comments starting on new lines
    let newline_comment_pattern: Regex = Regex::new(r"^[ ]*#").unwrap();
    // matches any comment, including inline comments
    let comment_pattern: Regex = Regex::new(r".*#").unwrap();
    // matches parameter assignments, like param="some string",
    let parameter_assignment_pattern =
        Regex::new("^[ ]*[a-zA-Z0-9_]+=[bfr]{0,1}\".*\"[,]{0,1}$").unwrap();
    // matches chained string addition lines, like + "some string"
    let addition_start_pattern = Regex::new("^[ ]*\\+ \".*\"").unwrap();
    // attempt at matching multi-line strings
    let dictionary_value_pattern = Regex::new("^[ ]*\".+\": [bfr]{0,1}\".*\"[,]{0,1}$").unwrap();
    let multiline_string_pattern: Regex =
        Regex::new("^[ ]*[\"]{0,3}[a-zA-Z0-9\\-:._,()?!%' ]*[\"]{0,3}$").unwrap();
    // match docstring variations
    let docstring_start: Regex = Regex::new("^[ ]*[\"]{3}").unwrap();
    let docstring_end: Regex = Regex::new("[\"]{3}$").unwrap();
    // avoid changing any known comment-based directives
    let ignore_pattern: Regex = Regex::new(".*#[ ]*(noqa|ignore|ruff|type|isort):.*").unwrap();
    let tabs_pattern: Regex = Regex::new("\t").unwrap();

    if ignore_pattern.is_match(line) {
        LineTooLongType::Ignored
    } else if tabs_pattern.is_match(line) {
        LineTooLongType::Unsupported
    } else if flagged_by_w505 {
        // if w505 is enabled we can differentiate between multi-line and doc strings
        if docstring_end.is_match(line) {
            if docstring_start.is_match(line) {
                LineTooLongType::StringSingleLineDocstring
            } else {
                LineTooLongType::StringMultiLineDocstring
            }
        } else if newline_comment_pattern.is_match(line) {
            // comments on separate lines are marked as docstrings by doc_lines.rs
            LineTooLongType::CommentSeparateLine
        } else if docstring_start.is_match(line) {
            // see edge cases in fix_line_too_long.rs fixture
            LineTooLongType::StringMultiLineDocstring
        } else {
            LineTooLongType::StringMultiLineDocstring
        }
    } else if string_pattern.is_match(line) {
        if ends_with_comma_pattern.is_match(line) {
            if prev_line.is_some_and(|prev_line| open_parenthesis_pattern.is_match(prev_line)) {
                LineTooLongType::StringSingleLineWithTrailingCommaParenthesis
            } else {
                LineTooLongType::StringSingleLineWithTrailingComma
            }
        } else {
            LineTooLongType::StringSingleLine
        }
    } else if comment_pattern.is_match(line) {
        if newline_comment_pattern.is_match(line) {
            LineTooLongType::CommentSeparateLine
        } else {
            LineTooLongType::CommentInline
        }
    } else if multiline_string_pattern.is_match(line) {
        // if W505 is disabled, docstrings will be treated the same as multi-line strings
        LineTooLongType::StringMultiLine
    } else if parameter_assignment_pattern.is_match(line) {
        if ends_with_comma_pattern.is_match(line) {
            LineTooLongType::StringParamAssignmentTrailingComma
        } else {
            LineTooLongType::StringParamAssignment
        }
    } else if addition_start_pattern.is_match(line) {
        LineTooLongType::StringAdditionStart
    } else if dictionary_value_pattern.is_match(line) {
        if ends_with_comma_pattern.is_match(line) {
            LineTooLongType::StringDictionaryValueTrailingComma
        } else {
            LineTooLongType::StringDictionaryValue
        }
    } else {
        LineTooLongType::Unsupported
    }
}

struct BreakLineConfig {
    line_limit: LineLength,
    tab_size: IndentWidth,
    break_chars: Vec<char>,
}

struct BreakLineResult {
    line_has_fstring: bool,
    overflow: String,
    overflow_range: TextRange,
    overflow_has_fvar: bool,
    unchanged: String,
    unchanged_range: TextRange,
    unchanged_has_fvar: bool,
    base_indent_range: TextRange,
    base_indent: String,
    new_indent: String,
    indent_to_string_range: TextRange,
}

fn break_line(
    line: &Line,
    line_type: &LineTooLongType,
    indexer: &Indexer,
    config: &BreakLineConfig,
) -> Option<BreakLineResult> {
    let mut line_has_fstring = false;
    if indexer.fstring_ranges().intersects(line.range()) {
        line_has_fstring = true;
    }

    let mut start_offset = line.start();
    let mut last_text_break = line.start();
    let mut indent_end = line.start();
    let mut indent_end_found = false;
    let mut string_start = line.start();
    let mut string_start_found = false;
    let mut dict_key_found = false;
    let mut last_fstring_safe_offset = last_text_break;
    let mut start_width = LineWidthBuilder::new(config.tab_size);

    // account for new characters and indents that will be added after breaking line
    let new_indent = " ".repeat(config.tab_size.as_usize());
    let added_quote_length = 1;
    let added_comma_length = 1;
    let no_added_chars = 0;
    let added_char_count = match line_type {
        LineTooLongType::StringSingleLineWithTrailingComma => {
            new_indent.len() + added_quote_length + added_comma_length
        }
        LineTooLongType::StringSingleLineWithTrailingCommaParenthesis => {
            added_comma_length + added_quote_length
        }
        LineTooLongType::StringSingleLine => added_comma_length,
        LineTooLongType::StringAdditionStart => added_quote_length,
        _ => no_added_chars,
    };
    // fixed single line docstrings move leading """ to a new line
    let removed_char_count = match line_type {
        LineTooLongType::StringSingleLineDocstring => 3,
        _ => 0,
    };
    let shortened_line_limit = config.line_limit.value() - u16::try_from(added_char_count).unwrap()
        + u16::try_from(removed_char_count).unwrap();
    let line_limit = LineLength::try_from(shortened_line_limit).unwrap();

    let mut fstring_opened = false;
    for c in line.chars() {
        if start_width < line_limit {
            if c != ' ' {
                indent_end_found = true;
                if matches!(
                    line_type,
                    LineTooLongType::StringMultiLine | LineTooLongType::StringMultiLineDocstring,
                ) {
                    string_start_found = true;
                }
            }
            if c == ':' {
                dict_key_found = true;
            }
            if c == '"' {
                string_start_found = true;
                if line_type.is_dictionary() && !dict_key_found {
                    string_start_found = false;
                }
            }

            if line_has_fstring {
                if c == '{' {
                    fstring_opened = true;
                } else if c == '}' {
                    fstring_opened = false;
                }
            }

            start_offset += c.text_len();
            start_width = start_width.add_char(c);

            if !indent_end_found {
                indent_end = start_offset;
            }
            if !string_start_found {
                string_start = start_offset;
            }
            if !string_start_found || !indent_end_found {
                continue;
            }

            for break_char in &config.break_chars {
                if c == *break_char {
                    last_text_break = start_offset;
                    if !fstring_opened {
                        last_fstring_safe_offset = start_offset;
                    }
                    break;
                }
            }
        } else {
            // if no text break found, we can't break line further
            if !indent_end_found || last_text_break == line.start() {
                return None;
            }
            if fstring_opened {
                last_text_break = last_fstring_safe_offset;
            }
            break;
        }
    }
    let mut overflow_range = TextRange::new(last_text_break, line.end());
    let mut unchanged_range = TextRange::new(line.start(), overflow_range.start());

    if matches!(line_type, LineTooLongType::CommentInline) {
        let comment_ranges = indexer.comment_ranges();
        if let [comment_range] = comment_ranges.comments_in_range(line.range()) {
            // if a long inline comment, mark the whole comment as overflow
            overflow_range = *comment_range;
            unchanged_range = TextRange::new(line.start(), comment_range.start());
        }
    }

    let base_indent_range = TextRange::new(line.start(), indent_end);
    let indent_to_string_range = TextRange::new(indent_end, string_start);
    let indent_pattern: Regex = Regex::new(r"^[ ]*").unwrap();
    let base_indent = indent_pattern.find(line).unwrap().as_str().to_string();
    let unchanged_text = line.as_str()[unchanged_range - line.start()].to_string();
    let overflow_text = line.as_str()[overflow_range - line.start()].to_string();

    let mut overflow_has_fvar = false;
    let mut unchanged_has_fvar = false;
    if line_has_fstring {
        let fstring_var_pattern = Regex::new(r"\{.+\}").unwrap();
        if fstring_var_pattern.is_match(&unchanged_text) {
            unchanged_has_fvar = true;
        }
        if fstring_var_pattern.is_match(&overflow_text) {
            overflow_has_fvar = true;
        }
    }

    let empty_pattern = Regex::new("^[ ]*[#\"f]+ $").unwrap();
    if empty_pattern.is_match(unchanged_text.as_str()) {
        return None;
    }

    Some(BreakLineResult {
        line_has_fstring,
        overflow_range,
        base_indent_range,
        base_indent,
        unchanged: unchanged_text,
        unchanged_has_fvar,
        unchanged_range,
        overflow: overflow_text,
        overflow_has_fvar,
        new_indent,
        indent_to_string_range,
    })
}

fn get_line_length_edits(
    break_result: &BreakLineResult,
    line_type: &LineTooLongType,
) -> Option<(Edit, Vec<Edit>)> {
    match line_type {
        line_type if line_type.is_single_line_string() => {
            // delete line section that exceeds length
            let overflow_deletion = Edit::range_deletion(break_result.overflow_range);
            let mut edits = Vec::new();
            // delete unused f-string markers
            let string_start = break_result.indent_to_string_range.end();
            if break_result.line_has_fstring {
                if !break_result.unchanged_has_fvar {
                    edits.push(Edit::range_deletion(TextRange::new(
                        string_start,
                        (string_start.to_u32() + 1).into(),
                    )));
                }
            }
            let mut additional_indent = "";
            let mut fstring = "";
            let mut trailing_comma = "";
            let mut close_parenthesis = String::new();
            if line_type.has_trailing_comma() || line_type.is_dictionary() {
                trailing_comma = ",";
                if !line_type.has_open_parenthesis() {
                    edits.push(Edit::insertion(
                        format!(
                            "(\n{}{}",
                            break_result.base_indent, &break_result.new_indent
                        ),
                        string_start,
                    ));
                    additional_indent = &break_result.new_indent;
                    close_parenthesis = format!("\n{}),", break_result.base_indent);
                }
            }
            if break_result.overflow_has_fvar {
                fstring = "f";
            }
            edits.push(Edit::insertion(
                format!(
                    "\"{}\n{}{}{}\"{}{}",
                    trailing_comma,
                    break_result.base_indent,
                    additional_indent,
                    fstring,
                    break_result.overflow,
                    close_parenthesis,
                ),
                break_result.overflow_range.start(),
            ));

            Some((overflow_deletion, edits))
        }
        LineTooLongType::CommentSeparateLine => {
            // for comments we can remove trailing whitespace
            let trailing_whitespace = Regex::new(" $").unwrap();
            let delete_range = if trailing_whitespace.is_match(&break_result.unchanged) {
                TextRange::new(
                    (break_result.overflow_range.start().to_u32() - 1).into(),
                    break_result.overflow_range.end(),
                )
            } else {
                break_result.overflow_range
            };

            // delete overflow line section
            let overflow_deletion = Edit::range_deletion(delete_range);
            // insert overflow section on line below
            let newline_edit = Edit::insertion(
                format!("\n{}# {}", break_result.base_indent, break_result.overflow),
                delete_range.start(),
            );

            Some((overflow_deletion, [newline_edit].to_vec()))
        }
        LineTooLongType::StringSingleLineDocstring => {
            // delete overflow, plus any whitespace
            let trailing_whitespace = Regex::new(" $").unwrap();
            let delete_range = if trailing_whitespace.is_match(&break_result.unchanged) {
                TextRange::new(
                    (break_result.overflow_range.start().to_u32() - 1).into(),
                    break_result.overflow_range.end(),
                )
            } else {
                break_result.overflow_range
            };

            let overflow_deletion = Edit::range_deletion(delete_range);
            let mut edits = Vec::new();
            let docstring_quote_len = 3;
            // insert line break and indent after """
            edits.push(Edit::insertion(
                format!("\n{}", break_result.base_indent),
                (break_result.base_indent_range.end().to_u32() + docstring_quote_len).into(),
            ));
            // add overflow with line break and """ on final line
            let mut trimmed_overflow = break_result.overflow.clone();
            trimmed_overflow.truncate(break_result.overflow.len() - docstring_quote_len as usize);
            edits.push(Edit::insertion(
                format!(
                    "\n{}{}\n{}\"\"\"",
                    break_result.base_indent, trimmed_overflow, break_result.base_indent
                ),
                delete_range.start(),
            ));
            Some((overflow_deletion, edits))
        }
        LineTooLongType::StringMultiLine | LineTooLongType::StringMultiLineDocstring => {
            // note: this is probably the most unsafe edit and could potentially
            // be skipped
            // it could break:
            //  - code inside docstrings
            //  - new-line and space sensitive multi-line strings (queries, prints, etc.)
            //
            //  however, the edit is marked as unsafe, and these can be avoided by skipping
            //  formatting for relevant sections and/or increasing line length

            // if a multi-line string, just add a line break with matching indentation

            let trailing_whitespace = Regex::new(" $").unwrap();
            let delete_range = if trailing_whitespace.is_match(&break_result.unchanged) {
                TextRange::new(
                    (break_result.overflow_range.start().to_u32() - 1).into(),
                    break_result.overflow_range.end(),
                )
            } else {
                break_result.overflow_range
            };
            let overflow_deletion = Edit::range_deletion(delete_range);

            let mut additional_edits = Vec::new();
            let newline_edit = Edit::insertion(
                format!("\n{}{}", break_result.base_indent, break_result.overflow),
                delete_range.start(),
            );
            additional_edits.push(newline_edit);
            Some((overflow_deletion, additional_edits))
        }
        LineTooLongType::CommentInline => {
            // if the line matches a comment and is not a newline comment, assume it is inline
            let trailing_whitespace = Regex::new("  $").unwrap();
            let delete_range = if trailing_whitespace.is_match(&break_result.unchanged) {
                TextRange::new(
                    (break_result.overflow_range.start().to_u32() - 2).into(),
                    break_result.overflow_range.end(),
                )
            } else {
                break_result.overflow_range
            };
            let newline_edit = Edit::range_deletion(delete_range);
            let moved_comment_edit = Edit::insertion(
                format!("\n{}{}\n", break_result.base_indent, break_result.overflow),
                break_result.unchanged_range.start(),
            );
            Some((newline_edit, [moved_comment_edit].to_vec()))
        }
        LineTooLongType::StringParamAssignment
        | LineTooLongType::StringParamAssignmentTrailingComma => {
            // just break string onto separate line, and then let single line logic handle the rest
            let mut string_start = break_result.indent_to_string_range.end();
            let mut string_end = break_result.overflow_range.end();
            if break_result.line_has_fstring {
                string_start -= TextSize::from(1);
            }
            if line_type.has_trailing_comma() {
                string_end -= TextSize::from(1);
            }
            let delete_trailing_comma_edit =
                Edit::deletion(string_end, break_result.overflow_range.end());
            let line_break_start_edit = Edit::insertion(
                format!("(\n{}{}", break_result.base_indent, break_result.new_indent),
                string_start,
            );
            let line_break_end_edit =
                Edit::insertion(format!("\n{}),", break_result.base_indent), string_end);
            Some((
                delete_trailing_comma_edit,
                [line_break_start_edit, line_break_end_edit].to_vec(),
            ))
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use ruff_python_index::Indexer;
    use ruff_python_parser::parse_module;
    use ruff_source_file::Line;
    use ruff_text_size::{TextRange, TextSize};

    use crate::Edit;
    use crate::line_width::{IndentWidth, LineLength};
    use crate::locator::Locator;

    use super::{
        BreakLineConfig, BreakLineResult, LineTooLongType, break_line, classify_line,
        get_line_length_edits,
    };

    fn break_line_test_helper(
        line_str: &str,
        source: &str,
        line_type: &LineTooLongType,
        line_limit: u16,
    ) -> BreakLineResult {
        let line = Line::new(line_str, TextSize::from(0));
        let locator = Locator::new(source);
        let parsed = parse_module(source).unwrap();
        let indexer = Indexer::from_tokens(parsed.tokens(), locator.contents());

        break_line(
            &line,
            line_type,
            &indexer,
            &BreakLineConfig {
                tab_size: IndentWidth::default(),
                line_limit: LineLength::try_from(line_limit).unwrap(),
                break_chars: vec![' ', ','],
            },
        )
        .unwrap()
    }

    #[test]
    fn line_too_long_edits_break_line_comment() {
        let line_limit = 20;
        let line: &str = "    # this is a single line comment";
        let source = line;
        let result = break_line_test_helper(
            line,
            source,
            &LineTooLongType::CommentSeparateLine,
            line_limit,
        );
        assert_eq!(result.unchanged, "    # this is a ");
        assert_eq!(result.overflow, "single line comment");
    }

    #[test]
    fn line_too_long_edits_break_line_comment_inline() {
        let line_limit = 20;
        let line: &str = "x = 20  # this is an inline comment";
        let source = line;
        let result =
            break_line_test_helper(line, source, &LineTooLongType::CommentInline, line_limit);
        assert_eq!(result.unchanged, "x = 20  ");
        assert_eq!(result.overflow, "# this is an inline comment");
    }

    #[test]
    fn line_too_long_edits_break_line_string() {
        let line_limit = 20;
        let line: &str = "    \"this is a single line string\"";
        let source = "print(
    \"this is a single line string\"
)";
        let result =
            break_line_test_helper(line, source, &LineTooLongType::StringSingleLine, line_limit);
        assert!(!result.line_has_fstring);
        assert_eq!(result.unchanged, "    \"this is a ");
        assert_eq!(result.overflow, "single line string\"");
    }

    fn classify_line_test_helper(
        line: &str,
        prev_line: Option<&str>,
        is_docstring: bool,
    ) -> LineTooLongType {
        if let Some(prev_line) = prev_line {
            classify_line(
                &Line::new(line, TextSize::from(0)),
                is_docstring,
                Some(&Line::new(prev_line, TextSize::from(0))),
            )
        } else {
            classify_line(&Line::new(line, TextSize::from(0)), is_docstring, None)
        }
    }

    #[test]
    fn line_too_long_edits_classify_line_string() {
        assert!(matches!(
            classify_line_test_helper("  \"single line string\"", None, false),
            LineTooLongType::StringSingleLine
        ));
    }

    #[test]
    fn line_too_long_edits_classify_line_string_comma() {
        assert!(matches!(
            classify_line_test_helper("  \"single line string\",", None, false),
            LineTooLongType::StringSingleLineWithTrailingComma
        ));
    }

    fn get_line_length_edits_test_helper(
        break_result: &BreakLineResult,
        line_type: &LineTooLongType,
    ) -> Vec<Edit> {
        if let Some((first_edit, mut additional_edits)) =
            get_line_length_edits(break_result, line_type)
        {
            additional_edits.push(first_edit);
            additional_edits
        } else {
            Vec::new()
        }
    }

    fn break_result_test_helper(
        unchanged_text: &str,
        overflow_text: &str,
        unchanged_has_fvar: bool,
        overflow_has_fvar: bool,
    ) -> BreakLineResult {
        BreakLineResult {
            line_has_fstring: unchanged_has_fvar || overflow_has_fvar,
            unchanged_range: TextRange::new(
                TextSize::from(0),
                TextSize::try_from(unchanged_text.len()).unwrap(),
            ),
            unchanged: unchanged_text.to_string(),
            overflow: overflow_text.to_string(),
            overflow_range: TextRange::new(
                TextSize::try_from(unchanged_text.len()).unwrap(),
                TextSize::try_from(overflow_text.len()).unwrap(),
            ),
            base_indent_range: TextRange::new(TextSize::from(0), TextSize::from(4)),
            base_indent: String::from("    "),
            unchanged_has_fvar,
            overflow_has_fvar,
            new_indent: String::from("    "),
            indent_to_string_range: TextRange::new(
                TextSize::from(4),
                TextSize::try_from(overflow_text.len()).unwrap(),
            ),
        }
    }

    #[test]
    fn line_too_long_edits_get_edits() {
        let unchanged_text = "    \"a string ";
        let overflow_text = "overflow section\"";
        let break_result = break_result_test_helper(unchanged_text, overflow_text, false, false);
        let edits =
            get_line_length_edits_test_helper(&break_result, &LineTooLongType::StringSingleLine);
        let expected_edits = vec![
            Edit::insertion(
                String::from("\"\n    \"overflow section\""),
                break_result.unchanged_range.end(),
            ),
            Edit::range_deletion(break_result.overflow_range),
        ];
        assert_eq!(edits, expected_edits);
    }

    #[test]
    fn line_too_long_edits_get_edits_fstring() {
        let unchanged_text = "    f\"a string ";
        let overflow_text = "overflow {var} section\"";
        let break_result = break_result_test_helper(unchanged_text, overflow_text, false, true);
        let edits =
            get_line_length_edits_test_helper(&break_result, &LineTooLongType::StringSingleLine);
        let expected_edits = vec![
            Edit::range_deletion(TextRange::new(TextSize::from(4), TextSize::from(5))),
            Edit::insertion(
                String::from("\"\n    f\"overflow {var} section\""),
                break_result.unchanged_range.end(),
            ),
            Edit::range_deletion(break_result.overflow_range),
        ];
        assert_eq!(edits, expected_edits);
    }
}

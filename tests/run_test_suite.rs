use regex::Regex;
use std::{collections::HashSet, fs, path::Path, process::Command};
use test_generator::test_resources;

#[derive(Debug, Clone)]
struct ExpectedOutput<'src> {
    line: usize,
    content: &'src str,
}
impl<'src> ExpectedOutput<'src> {
    fn new(line: usize, content: &'src str) -> Self {
        Self { line, content }
    }
}

#[derive(Debug, Default, Clone)]
struct ExpectedResult<'src> {
    expected_output: Vec<ExpectedOutput<'src>>,
    expected_compile_errors: HashSet<String>,
    expected_runtime_error: Option<&'src str>,
    runtime_error_line: usize,
    expected_exit_code: i32,
}

fn parse_expected_result(source: &'_ str) -> ExpectedResult<'_> {
    let mut expected_result = ExpectedResult::default();

    let expected_output_pattern = Regex::new(r"// expect: ?(.*)").unwrap();
    let expected_error_pattern = Regex::new(r"// (Error.*)").unwrap();
    let error_line_pattern = Regex::new(r"// \[((java|c) )?line (\d+)\] (Error.*)").unwrap();
    let expected_runtime_error_pattern = Regex::new(r"// expect runtime error: (.+)").unwrap();

    for (idx, line) in source.lines().enumerate() {
        let line_num = idx + 1;

        if let Some(captured) = expected_output_pattern.captures(line) {
            expected_result.expected_output.push(ExpectedOutput::new(
                line_num,
                captured.get(1).unwrap().as_str(),
            ));
            continue;
        }

        if let Some(captured) = expected_error_pattern.captures(line) {
            expected_result.expected_compile_errors.insert(format!(
                "[{line_num}] {}",
                captured.get(1).unwrap().as_str()
            ));
            expected_result.expected_exit_code = 65;
            continue;
        }

        if let Some(captured) = error_line_pattern.captures(line) {
            let language = captured.get(2);
            if language.is_none() || language.unwrap().as_str() == "c" {
                expected_result.expected_compile_errors.insert(format!(
                    "[{}] {}",
                    captured.get(3).unwrap().as_str(),
                    captured.get(4).unwrap().as_str()
                ));
                expected_result.expected_exit_code = 65;
            }
            continue;
        }

        if let Some(captured) = expected_runtime_error_pattern.captures(line) {
            expected_result.runtime_error_line = line_num;
            expected_result.expected_runtime_error = Some(captured.get(1).unwrap().as_str());
            expected_result.expected_exit_code = 70;
        }
    }

    if !expected_result.expected_compile_errors.is_empty()
        && expected_result.expected_runtime_error.is_some()
    {
        panic!("Cannot expect both compile and runtime errors!");
    }

    expected_result
}

fn validate_runtime_error(
    error_lines: &[&str],
    expected_runtime_error: &str,
    runtime_error_line: usize,
) {
    assert!(
        error_lines.len() >= 2,
        "Expected runtime error '{expected_runtime_error}' and got none."
    );
    assert_eq!(
        error_lines[0], expected_runtime_error,
        "Expected runtime error '{expected_runtime_error}', and got: {}",
        error_lines[0]
    );

    // Make sure the stack trace has the right line.
    let stack_trace_pattern = Regex::new(r"\[line (\d+)\]").unwrap();
    let mut stack_trace_match = None;
    let stack_lines = &error_lines[1..];
    for line in stack_lines {
        stack_trace_match = stack_trace_pattern.captures(*line);
        if stack_trace_match.is_some() {
            break;
        }
    }

    assert!(
        stack_trace_match.is_some(),
        "Expected stack trace and got: {stack_lines:?}"
    );
    let stack_line = stack_trace_match.unwrap()[1].parse::<usize>().unwrap();
    assert_eq!(
        stack_line, runtime_error_line,
        "Expected runtime error on line {runtime_error_line} but was on line {stack_line}"
    );
}

fn validate_compile_errors(error_lines: &[&str], expected_compile_errors: &HashSet<String>) {
    // Validate that every compile error was expected.
    let syntax_error_pattern = Regex::new(r"\[.*line (\d+)\] (Error.+)").unwrap();
    let mut found_errors = HashSet::new();
    for line in error_lines {
        if let Some(captured) = syntax_error_pattern.captures(line) {
            let error = format!(
                "[{}] {}",
                captured.get(1).unwrap().as_str(),
                captured.get(2).unwrap().as_str()
            );
            assert!(
                expected_compile_errors.contains(&error),
                "Not an expected compile error: {error}"
            );
            found_errors.insert(error);
        }
    }

    let difference: HashSet<_> = expected_compile_errors.difference(&found_errors).collect();
    assert!(
        difference.is_empty(),
        "Missing expected errors: {:?}",
        difference
    );
}

fn validate_output(mut output_lines: &[&str], expected_output: &[ExpectedOutput]) {
    // Remove the trailing last empty line.
    if !output_lines.is_empty() && *output_lines.last().unwrap() == "" {
        output_lines = &output_lines[..output_lines.len() - 1];
    }

    for (idx, line) in output_lines.iter().enumerate() {
        assert!(
            idx < expected_output.len(),
            "Got Output '{line}' when none was expected."
        );

        let expected = &expected_output[idx];
        assert_eq!(
            expected.content, *line,
            "Expected output '{}' on line {} and got '{line}'.",
            expected.content, expected.line
        );
    }

    let mut idx = output_lines.len();
    let mut missing_outputs = vec![];
    while idx < expected_output.len() {
        let expected = &expected_output[idx];
        missing_outputs.push(format!(
            "Missing expected output '{}' on line {}.",
            expected.content, expected.line
        ));
        idx += 1;
    }
    assert!(missing_outputs.is_empty(), "{:?}", missing_outputs);
}

#[test_resources("tests/lox_test_suite/*/*.lox")]
fn test_file(resource: &str) {
    let file_path = Path::new(resource);
    let file_content =
        fs::read_to_string(file_path).expect(&format!("Failed to read file {resource}"));
    let expected_result = parse_expected_result(&file_content);

    let output = Command::new(env!("CARGO_BIN_EXE_lox_bytecode_vm"))
        .arg(file_path.to_str().unwrap())
        .output()
        .unwrap();

    let exit_status = output.status.code().unwrap();
    let stdout = String::from_utf8(output.stdout).unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    let error_lines = stderr.lines().collect::<Vec<&str>>();

    if let Some(runtime_error) = expected_result.expected_runtime_error {
        validate_runtime_error(
            &error_lines,
            runtime_error,
            expected_result.runtime_error_line,
        );
    } else {
        validate_compile_errors(&error_lines, &expected_result.expected_compile_errors);
    }

    assert_eq!(
        exit_status, expected_result.expected_exit_code,
        "Expected return code {} and got {exit_status}.",
        expected_result.expected_exit_code
    );

    validate_output(
        &stdout.lines().collect::<Vec<&str>>(),
        &expected_result.expected_output,
    );
}

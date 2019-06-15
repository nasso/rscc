use std::fs::File;
use std::io::Read;

use nom::error::VerboseError;

pub mod parse;

#[derive(Debug)]
pub enum PreprocessingError {
    IOError(std::io::Error),
    IncompleteParseError(nom::Needed),
    ParseError(String),
}

impl From<std::io::Error> for PreprocessingError {
    fn from(e: std::io::Error) -> PreprocessingError {
        PreprocessingError::IOError(e)
    }
}

impl From<(&str, nom::Err<VerboseError<&str>>)> for PreprocessingError {
    fn from((data, e): (&str, nom::Err<VerboseError<&str>>)) -> PreprocessingError {
        match e {
            nom::Err::Incomplete(n) => PreprocessingError::IncompleteParseError(n),
            nom::Err::Error(e) => {
                PreprocessingError::ParseError(nom::error::convert_error(data, e))
            }
            nom::Err::Failure(e) => {
                PreprocessingError::ParseError(nom::error::convert_error(data, e))
            }
        }
    }
}

// read the file content to a String
fn read_file_content(path: &str) -> std::io::Result<String> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    Ok(contents)
}

// makes all new line indicators (CRLF, LF or just CR) with a single LF
fn sanitize_newlines(source: &mut String) {
    // replace every occurence of CRLF with just LF
    *source = source.replace("\r\n", "\n");
    *source = source.replace("\r", "\n");
}

// replace every trigraph sequence
fn replace_trigraph_sequences(source: &mut String) {
    *source = source.replace("\\n", "");
    *source = source.replace("??=", "#");
    *source = source.replace("??(", "[");
    *source = source.replace("??/", "\\");
    *source = source.replace("??)", "]");
    *source = source.replace("??'", "^");
    *source = source.replace("??<", "{");
    *source = source.replace("??!", "|");
    *source = source.replace("??>", "}");
    *source = source.replace("??-", "~");
}

// escape newlines
fn escape_newlines(source: &mut String) {
    // remove every occurence of "\\n"
    *source = source.replace("\\\n", "");
}

// Goes through phases 1 to 4
pub fn process(unit_path: &str) -> Result<Vec<u8>, PreprocessingError> {
    let mut source = read_file_content(unit_path)?;

    // Phase 1
    sanitize_newlines(&mut source);
    replace_trigraph_sequences(&mut source);

    // Phase 2
    escape_newlines(&mut source);

    // Phase 3
    let pp_tokens = parse::parse_preprocessing_file::<VerboseError<&str>>(&source)
        // we need to wrap up a tuple containing the input data if there's any error
        .or_else(|e| Err((&source[..], e)))?;

    Ok(vec![])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sanitize_newlines() {
        let mut source = "abc\ndef\r\nhij\rklm\n\rnop\n".to_string();

        sanitize_newlines(&mut source);

        assert_eq!(source, "abc\ndef\nhij\nklm\n\nnop\n".to_string());
    }

    #[test]
    fn test_replace_trigraph_sequences() {
        let mut source = "a??=b??(c??/d??)e??'f??<g??!h??>i??-j".to_string();

        replace_trigraph_sequences(&mut source);

        assert_eq!(source, "a#b[c\\d]e^f{g|h}i~j".to_string());
    }

    #[test]
    fn test_escape_newlines() {
        let mut source = "abc\ndef\\\nhij\\".to_string();

        escape_newlines(&mut source);

        assert_eq!(source, "abc\ndefhij\\".to_string());
    }
}

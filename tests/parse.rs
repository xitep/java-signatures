use std::{
    ffi::OsStr,
    fmt::{Debug, Display},
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

use cafebabe::attributes::{AttributeData, AttributeInfo};
use java_signatures::{
    parse_class_signature, parse_field_signature, parse_method_signature, ParseError,
};

#[test]
fn parse_success() {
    let path = {
        let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        p.push("tests");
        p.push("parse");
        p
    };
    let files = match find_class_files(&path) {
        Ok(files) => files,
        Err(e) => panic!("[FAIL] Unable to find class files in {path:?}: {e}"),
    };

    let mut bytes = Vec::with_capacity(64 * 1024);
    for f in files {
        bytes.clear();
        match File::open(&f)
            .map_err(|e| format!("{e}"))
            .and_then(|mut f| f.read_to_end(&mut bytes).map_err(|e| format!("{e}")))
            .and_then(|_| cafebabe::parse_class(&bytes).map_err(|e| format!("{e}")))
        {
            Err(e) => {
                panic!("[FAIL] {f:?}: {e}");
            }
            Ok(class) => {
                if let Some(sig) = find_signature(&class.attributes) {
                    parse_signature(&f, "class", &class.this_class, sig, parse_class_signature);
                }
                for method in class.methods {
                    if let Some(sig) = find_signature(&method.attributes) {
                        parse_signature(&f, "method", &method.name, sig, parse_method_signature);
                    }
                }
                for field in class.fields {
                    if let Some(sig) = find_signature(&field.attributes) {
                        parse_signature(&f, "class field", &field.name, sig, parse_field_signature);
                    }
                }
                if let Some(components) = class
                    .attributes
                    .iter()
                    .filter_map(|attr| {
                        if let AttributeData::Record(ref component) = attr.data {
                            Some(component)
                        } else {
                            None
                        }
                    })
                    .next()
                {
                    for c in components {
                        if let Some(sig) = find_signature(&c.attributes) {
                            parse_signature(
                                &f,
                                "record component",
                                &c.name,
                                sig,
                                parse_field_signature,
                            );
                        }
                    }
                }
            }
        }
    }
}

fn parse_signature<'a, T: 'a + Display, F: Fn(&'a str) -> Result<T, ParseError<'a>>>(
    path: &Path,
    ctx_type: &str,
    ctx_name: &str,
    signature: &'a str,
    parser: F,
) {
    match parser(signature) {
        Ok(parsed) => {
            let rendered = format!("{}", parsed);
            if rendered != signature {
                panic!(
                    "[FAIL] Rendered signature differs from original:\noriginal: {:?}\nrendered: {:?}",
                    signature, rendered
                );
            }
            println!("[OK] {path:?}\n    ({ctx_type}#{ctx_name}): {signature:?}");
        }
        Err(e) => {
            panic!(
                "[FAIL] {path:?}\n      ({ctx_type}#{ctx_name}): {signature:?}: {e} (failed at … \"{}\")",
                &signature[e.position()..]
            );
        }
    }
}

fn find_signature<'a>(attributes: &'a [AttributeInfo<'a>]) -> Option<&'a str> {
    for attr in attributes {
        if let AttributeData::Signature(ref sig) = attr.data {
            return Some(sig);
        }
    }
    None
}

fn find_class_files<P: AsRef<Path> + Debug>(path: P) -> std::io::Result<Vec<PathBuf>> {
    fn rec<P: AsRef<Path> + Debug>(path: P, acc: &mut Vec<PathBuf>) -> std::io::Result<()> {
        for e in std::fs::read_dir(path)? {
            let e = e?;
            let file_type = e.file_type()?;
            if file_type.is_dir() {
                rec(e.path(), acc)?;
            } else if file_type.is_file() {
                let path = e.path();
                if path.extension() == Some(OsStr::new("class")) {
                    acc.push(path);
                }
            }
        }
        Ok(())
    }

    let mut files = Vec::new();
    rec(path, &mut files)?;
    Ok(files)
}

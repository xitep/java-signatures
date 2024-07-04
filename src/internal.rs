/// Implements the parsing of java signatures.
use std::{borrow::Cow, error::Error, fmt::Display, ops::Range};

// this one is published outside out crate
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BaseType {
    Byte,    // B
    Char,    // C
    Double,  // D
    Float,   // F
    Int,     // I
    Long,    // J
    Short,   // S
    Boolean, // Z
}

#[derive(Debug)]
pub enum JavaType {
    Base(BaseType),
    Reference(ReferenceType),
}

#[derive(Debug)]
pub enum TypeArgument {
    Unbounded,
    Default(ReferenceType),
    Extends(ReferenceType),
    Super(ReferenceType),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Slice {
    start: u16, // ~ inclusive
    end: u16,   // ~ exclusive
}

impl Slice {
    pub fn start(&self) -> usize {
        self.start as usize
    }

    /// Panics if `s` is too to be indexed by this slice.
    pub fn apply<'a>(&self, s: &'a str) -> &'a str {
        &s[self.start as usize..self.end as usize]
    }
}

/// Panics if the range includes boundaries greater than `u16::MAX`
/// or if the upper bounds bound is equal to or less the lower bound.
impl From<Range<usize>> for Slice {
    fn from(value: Range<usize>) -> Self {
        if value.start >= value.end {
            panic!("{} >= {}!", value.start, value.end);
        }
        if value.start >= u16::MAX as usize || value.end >= u16::MAX as usize {
            panic!("boundary/ies too big!");
        }
        Self {
            start: value.start as u16,
            end: value.end as u16,
        }
    }
}

impl From<Slice> for Range<usize> {
    fn from(val: Slice) -> Self {
        (val.start as usize)..(val.end as usize)
    }
}

#[derive(Debug)]
pub struct SimpleClassType(pub Slice, pub Vec<TypeArgument>);

#[derive(Debug)]
pub struct ClassType(pub SimpleClassType, pub Vec<SimpleClassType>);

#[derive(Debug)]
pub enum ReferenceType {
    ClassType(ClassType),
    TypeVariable(Slice),
    ArrayType { dimension: usize, ty: Box<JavaType> },
}

#[derive(Debug)]
pub struct TypeParameter {
    pub name: Slice,
    pub class_bound: Option<ReferenceType>,
    pub iface_bounds: Vec<ReferenceType>,
}

#[derive(Debug)]
pub enum ResultType {
    VoidType,
    ValueType(JavaType),
}

#[derive(Debug)]
pub enum ThrowsType {
    ClassType(ClassType),
    /// Specify the name (as a slice) of the type variable being referenced
    TypeVariable(Slice),
}

#[derive(Debug)]
pub struct ClassSignature {
    pub type_params: Vec<TypeParameter>,
    pub super_class: ClassType,
    pub super_ifaces: Vec<ClassType>,
}

#[derive(Debug)]
pub struct MethodSignature {
    pub type_params: Vec<TypeParameter>,
    pub parameters: Vec<JavaType>,
    pub result: ResultType,
    pub throws: Vec<ThrowsType>,
}

// ----------------------------------------------------------------

pub fn parse<'a, F, T, MI, I>(name: &'static str, parser: F, s: &'a str, make_iter: MI) -> Result<T>
where
    I: Iterator<Item = Character>,
    F: Fn(&mut I) -> Result<T>,
    MI: Fn(&'a str) -> I + 'a,
{
    if s.is_empty() {
        Err(ParseError::new(name, 0, "empty input"))
    } else {
        let mut chars = make_iter(s);
        parser(&mut chars)
            .map_err(|mut e| {
                if e.position == EOF_POSITION {
                    e.position = s.len();
                }
                e
            })
            .and_then(|value| {
                if let Some((pos, _)) = chars.next() {
                    Err(ParseError::new(
                        name,
                        pos,
                        "bad input; expected end of input",
                    ))
                } else {
                    Ok(value)
                }
            })
    }
}

// ----------------------------------------------------------------

// ~ constant used to denote the (fictive) position of an unexpected
// eof occurance. lower-level code can raise the `eof` error without
// knowing the exact position, and higher-level code can patch up the
// position based on the parsed input string's length.
const EOF_POSITION: usize = usize::MAX;

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub context: &'static str,
    pub position: usize,
    pub error: Cow<'static, str>,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: [position: {} / context: {}]",
            self.error, self.position, self.context
        )
    }
}

impl Error for ParseError {}

impl ParseError {
    fn new<E: Into<Cow<'static, str>>>(context: &'static str, position: usize, error: E) -> Self {
        Self {
            position,
            error: error.into(),
            context,
        }
    }
    fn eof(context: &'static str) -> Self {
        Self {
            position: EOF_POSITION,
            error: Cow::Borrowed("unexpected end of input"),
            context,
        }
    }
}

type Result<T> = std::result::Result<T, ParseError>;

/// Consumes the next available character or fails with an "unexpected
/// end of input" error if the `chars` stream is exhausted.
fn consume_next(
    context: &'static str,
    chars: &mut impl Iterator<Item = Character>,
) -> Result<Character> {
    match chars.next() {
        None => Err(ParseError::eof(context)),
        Some(c) => Ok(c),
    }
}

/// "Consumes" the given character (assumed to the next in the
/// consumed character stream) and fails if it doesn't match the
/// `expected` char.
fn consume_next_expected(
    context: &'static str,
    next: Option<Character>,
    expected: char,
) -> Result<()> {
    match next {
        None => Err(ParseError::eof(context)),
        Some((_, next)) if next == expected => Ok(()),
        Some((pos, _)) => Err(ParseError::new(
            context,
            pos,
            format!("bad input; expected: `{expected}`"),
        )),
    }
}

/// Consumes the next available character if any (if there is no more,
/// returns `Ok(None)`) and fails if it doesn't not match the
/// `expected` char. Returns `Ok(Some(()))` in the
/// positive case.
fn consume_next_eof_or_expected(
    context: &'static str,
    chars: &mut impl Iterator<Item = Character>,
    expected: char,
) -> Result<Option<()>> {
    match chars.next() {
        None => Ok(None),
        c => {
            consume_next_expected(context, c, expected)?;
            Ok(Some(()))
        }
    }
}

// ~ the type being passed around when consuming characters while
// parsing signatures; essentially the character together with its
// position in the input stream
type Character = (usize, char);

fn to_base_type(c: char) -> Option<BaseType> {
    match c {
        'B' => Some(BaseType::Byte),
        'C' => Some(BaseType::Char),
        'D' => Some(BaseType::Double),
        'F' => Some(BaseType::Float),
        'I' => Some(BaseType::Int),
        'J' => Some(BaseType::Long),
        'S' => Some(BaseType::Short),
        'Z' => Some(BaseType::Boolean),
        _ => None,
    }
}

pub fn consume_reference_type_signature(
    chars: &mut impl Iterator<Item = Character>,
) -> Result<ReferenceType> {
    consume_next("ReferenceTypeSignature", chars)
        .and_then(|c| consume_reference_type_signature_(c, chars))
}

fn consume_reference_type_signature_(
    mut c: Character,
    more_chars: &mut impl Iterator<Item = Character>,
) -> Result<ReferenceType> {
    fn maybe_as_array(dimension: usize, ty: ReferenceType) -> ReferenceType {
        if dimension == 0 {
            ty
        } else {
            ReferenceType::ArrayType {
                dimension,
                ty: Box::new(JavaType::Reference(ty)),
            }
        }
    }
    // ~ as soon as `>0` we're having an ArrayType; once an
    // `ArrayType` is started there can only be following more
    // `ArrayType` dimensions or a terminating ClassType or
    // TypeVariable
    let mut dimension = 0;
    loop {
        match c.1 {
            '[' => {
                dimension += 1;

                // ArrayTypeSignature
                let next = consume_next("ReferenceTypeSignature", more_chars)?;
                match to_base_type(next.1) {
                    Some(ty) => {
                        debug_assert!(dimension > 0);
                        return dbg!(Ok(ReferenceType::ArrayType {
                            dimension,
                            ty: Box::new(JavaType::Base(ty)),
                        }));
                    }
                    None => {
                        c = next; // ~ and recurse/repeat
                    }
                }
            }
            'L' => {
                // ClassTypeSignature
                return consume_class_type_signature(more_chars)
                    .map(|ty| maybe_as_array(dimension, ReferenceType::ClassType(ty)));
            }
            'T' => {
                // TypeVariableSignature
                let r = consume_unqualified_identifer(more_chars)?;
                return match r.1 {
                    None => Err(ParseError::eof("ReferenceTypeSignature")),
                    Some((_, ';')) => {
                        Ok(maybe_as_array(dimension, ReferenceType::TypeVariable(r.0)))
                    }
                    Some((pos, _)) => Err(ParseError::new(
                        "ReferenceTypeSignature",
                        pos,
                        "bad input; expected `;`",
                    )),
                };
            }
            _ => {
                return Err(ParseError::new(
                    "ReferenceTypeSignature",
                    c.0,
                    "bad input; expected `[`, `L`, or `T`",
                ));
            }
        }
    }
}

fn consume_class_type_signature(chars: &mut impl Iterator<Item = Character>) -> Result<ClassType> {
    let mut r = consume_unqualified_identifer(chars)?;
    let start_pos = r.0.start();
    loop {
        match r.1 {
            Some((end_pos, ';')) => {
                return Ok(ClassType(
                    SimpleClassType((start_pos..end_pos).into(), Vec::new()),
                    Vec::new(),
                ));
            }
            Some((_, '/')) => {
                // next there must be an unqualified identifier; repeat
            }
            Some((end_pos, '.')) => {
                return Ok(ClassType(
                    SimpleClassType((start_pos..end_pos).into(), Vec::new()),
                    consume_class_type_signature_suffixes(chars)?,
                ));
            }
            Some((end_pos, '<')) => {
                let class_type =
                    SimpleClassType((start_pos..end_pos).into(), consume_type_arguments(chars)?);
                return match consume_next("ClassTypeSignature", chars)? {
                    (_, '.') => Ok(ClassType(
                        class_type,
                        consume_class_type_signature_suffixes(chars)?,
                    )),
                    (_, ';') => Ok(ClassType(class_type, Vec::new())),
                    (pos, _) => Err(ParseError::new(
                        "ClassTypeSignature",
                        pos,
                        "bad input; expected `.` or `;`",
                    )),
                };
            }
            Some((pos, _)) => {
                return Err(ParseError::new(
                    "ClassTypeSignature",
                    pos,
                    "bad input; expected `;`, `/`, `.`, or `<`",
                ))
            }
            None => return Err(ParseError::eof("ClassTypeSignature")),
        }
        r = consume_unqualified_identifer(chars)?;
    }
}

fn consume_class_type_signature_suffixes(
    chars: &mut impl Iterator<Item = Character>,
) -> Result<Vec<SimpleClassType>> {
    let mut suffixes = Vec::new();
    loop {
        let r = consume_unqualified_identifer(chars)?;
        match r.1 {
            Some((_, ';')) => {
                suffixes.push(SimpleClassType(r.0, Vec::new()));
                return Ok(suffixes);
            }
            Some((_, '.')) => {
                suffixes.push(SimpleClassType(r.0, Vec::new()));
                // next there must be another suffix; repeat
            }
            Some((_, '<')) => {
                let args = consume_type_arguments(chars)?;
                match consume_next("ClassTypeSignatureSuffix", chars)? {
                    (_, '.') => {
                        suffixes.push(SimpleClassType(r.0, args));
                        // must be followed by another suffix; repeat
                    }
                    (_, ';') => {
                        suffixes.push(SimpleClassType(r.0, args));
                        return Ok(suffixes);
                    }
                    (pos, _) => {
                        return Err(ParseError::new(
                            "ClassTypeSignature",
                            pos,
                            "bad input; expected `.` or end of input",
                        ));
                    }
                }
            }
            Some((pos, _)) => {
                return Err(ParseError::new(
                    "ClassTypeSignatureSuffix",
                    pos,
                    "bad input; expected `;`, `.`, or `<`>",
                ))
            }
            None => return Err(ParseError::eof("ClassTypeSignatureSuffix")),
        }
    }
}

// Assumes a leading '<' was already consumed; parses `chars` until a
// matching '>' (while consuming it) as `{ TypeArgument }`.
fn consume_type_arguments(
    chars: &mut impl Iterator<Item = Character>,
) -> Result<Vec<TypeArgument>> {
    let mut args = Vec::new();
    loop {
        match consume_next("TypeArgument", chars)? {
            (pos, '>') => {
                if args.is_empty() {
                    return Err(ParseError::new(
                        "TypeArguments",
                        pos,
                        "type-argument missing",
                    ));
                } else {
                    return Ok(args);
                }
            }
            (_, '*') => {
                args.push(TypeArgument::Unbounded);
            }
            (_, '+') => {
                args.push(TypeArgument::Extends(consume_reference_type_signature(
                    chars,
                )?));
            }
            (_, '-') => {
                args.push(TypeArgument::Super(consume_reference_type_signature(
                    chars,
                )?));
            }
            c => {
                args.push(TypeArgument::Default(consume_reference_type_signature_(
                    c, chars,
                )?));
            }
        }
    }
}

/// Consumes an unqualified idenfier returning the slice representing
/// it and the next character (if there was one) which is _not_
/// considered part of that identifier any more.
fn consume_unqualified_identifer(
    chars: &mut impl Iterator<Item = Character>,
) -> Result<(Slice, Option<Character>)> {
    consume_unqualified_identifer_(consume_next("Identifier", chars)?, chars)
}

fn consume_unqualified_identifer_(
    mut c: Character,
    more_chars: &mut impl Iterator<Item = Character>,
) -> Result<(Slice, Option<Character>)> {
    let start_pos = c.0;
    let mut len = 0;
    loop {
        if matches!(c.1, '.' | ';' | '[' | '/' | '<' | '>' | ':') {
            if len == 0 {
                return Err(ParseError::new("Identifier", c.0, "empty identifier"));
            } else {
                return Ok(((start_pos..c.0).into(), Some(c)));
            }
        } else {
            // XXX validate the accepted characters (e.g. rejecting whitespace, non-printable, etc)
            // see also https://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html#jls-3.8
        }
        len += 1;
        match more_chars.next() {
            None => return Ok(((start_pos..start_pos + len).into(), None)),
            Some(next) => c = next,
        }
    }
}

pub fn consume_class_signature(
    chars: &mut impl Iterator<Item = Character>,
) -> Result<ClassSignature> {
    let (type_params, next) = consume_type_parameters(chars)?;
    consume_next_expected("ClassSignature", next, 'L')?;
    let super_class = consume_class_type_signature(chars)?;
    let mut super_ifaces = Vec::new();
    loop {
        match consume_next_eof_or_expected("ClassSignature", chars, 'L')? {
            None => {
                return Ok(ClassSignature {
                    type_params,
                    super_class,
                    super_ifaces,
                });
            }
            Some(_) => {
                super_ifaces.push(consume_class_type_signature(chars)?);
                // ~ and repeat
            }
        }
    }
}

pub fn consume_method_signature(
    chars: &mut impl Iterator<Item = Character>,
) -> Result<MethodSignature> {
    let (type_params, next) = consume_type_parameters(chars)?;
    consume_next_expected("MethodSignature", next, '(')?;
    let (parameters, next) = consume_method_parameters(chars)?;
    let result = match next {
        None => return Err(ParseError::eof("MethodSignature")),
        Some((_, 'V')) => ResultType::VoidType,
        Some(c) => ResultType::ValueType(consume_java_type_signature(c, chars)?),
    };
    let throws = consume_throws_signature(chars)?;
    Ok(MethodSignature {
        type_params,
        parameters,
        result,
        throws,
    })
}

fn consume_method_parameters(
    chars: &mut impl Iterator<Item = Character>,
) -> Result<(Vec<JavaType>, Option<Character>)> {
    let mut params = Vec::new();
    loop {
        match consume_next("MethodParameter", chars)? {
            (_, ')') => return Ok((params, chars.next())),
            c => params.push(consume_java_type_signature(c, chars)?),
        };
    }
}

fn consume_throws_signature(
    chars: &mut impl Iterator<Item = Character>,
) -> Result<Vec<ThrowsType>> {
    let mut throws = Vec::new();
    loop {
        match chars.next() {
            None => return Ok(throws),
            Some((_, '^')) => match consume_next("ThrowsSignature", chars)? {
                (_, 'T') => {
                    throws.push(consume_unqualified_identifer(chars).and_then(
                        |(identifier, next)| {
                            consume_next_expected("ThrowsSignature", next, ';')?;
                            Ok(ThrowsType::TypeVariable(identifier))
                        },
                    )?);
                }
                (_, 'L') => {
                    throws.push(consume_class_type_signature(chars).map(ThrowsType::ClassType)?);
                }
                (pos, _) => {
                    return Err(ParseError::new(
                        "ThrowsSignature",
                        pos,
                        "bad input; expected `T` or `L`",
                    ))
                }
            },
            Some((pos, _)) => {
                return Err(ParseError::new(
                    "ThrowsSignature",
                    pos,
                    "bad input; expected `^` or end of input",
                ))
            }
        }
    }
}

fn consume_java_type_signature(
    c: Character,
    more_chars: &mut impl Iterator<Item = Character>,
) -> Result<JavaType> {
    if let Some(ty) = to_base_type(c.1) {
        Ok(JavaType::Base(ty))
    } else {
        consume_reference_type_signature_(c, more_chars).map(JavaType::Reference)
    }
}

// Optionally matches: `'<' { Identifier ':' [ReferenceTypeSignature] { ':' ReferenceTypeSignature } }+ '>'`
// Returns the next character not part of `TypeParameters` anymore
fn consume_type_parameters(
    chars: &mut impl Iterator<Item = Character>,
) -> Result<(Vec<TypeParameter>, Option<Character>)> {
    fn consume_more_reference_type_signatures(
        mut c: Character,
        more_chars: &mut impl Iterator<Item = Character>,
        accumulator: &mut Vec<ReferenceType>,
    ) -> Result<Option<Character>> {
        loop {
            match c {
                (_, ':') => {
                    accumulator.push(consume_reference_type_signature(more_chars)?);
                    c = consume_next("TypeParameter", more_chars)?;
                    // ~ and repeat
                }
                c => {
                    return Ok(Some(c));
                }
            }
        }
    }

    let mut params = Vec::new();
    match chars.next() {
        Some((_, '<')) => {
            let mut next = consume_unqualified_identifer(chars)?;
            loop {
                match next.1 {
                    Some((_, ':')) => {
                        // ~ the occurance (of a reference-type-signature) after the first colon is optional
                        match consume_next("TypeParameter", chars)? {
                            (_, '>') => {
                                params.push(TypeParameter {
                                    name: next.0,
                                    class_bound: None,
                                    iface_bounds: Vec::new(),
                                });
                                return Ok((params, chars.next()));
                            }
                            c @ (_, ':') => {
                                let mut iface_bounds = Vec::new();
                                match consume_more_reference_type_signatures(
                                    c,
                                    chars,
                                    &mut iface_bounds,
                                )? {
                                    None => return Err(ParseError::eof("TypeParameter")),
                                    Some((_, '>')) => {
                                        params.push(TypeParameter {
                                            name: next.0,
                                            class_bound: None,
                                            iface_bounds,
                                        });
                                        return Ok((params, chars.next()));
                                    }
                                    Some(c) => {
                                        params.push(TypeParameter {
                                            name: next.0,
                                            class_bound: None,
                                            iface_bounds,
                                        });
                                        next = consume_unqualified_identifer_(c, chars)?;
                                        // ~ and repeat
                                    }
                                }
                            }
                            c => {
                                let class_bound = consume_reference_type_signature_(c, chars)?;
                                let mut iface_bounds = Vec::new();
                                match consume_more_reference_type_signatures(
                                    consume_next("TypeParameter", chars)?,
                                    chars,
                                    &mut iface_bounds,
                                )? {
                                    None => return Err(ParseError::eof("TypeParameter")),
                                    Some(c) => {
                                        params.push(TypeParameter {
                                            name: next.0,
                                            class_bound: Some(class_bound),
                                            iface_bounds,
                                        });
                                        if c.1 == '>' {
                                            return Ok((params, chars.next()));
                                        } else {
                                            next = consume_unqualified_identifer_(c, chars)?;
                                            // ~ and repeat
                                        }
                                    }
                                }
                            }
                        }
                    }
                    Some((pos, _)) => {
                        return Err(ParseError::new(
                            "TypeParameter",
                            pos,
                            "bad input; expected `:`",
                        ))
                    }
                    None => return Err(ParseError::eof("TypeParameter")),
                }
            }
        }
        c => Ok((params, c)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_consume_method_signature() {
        let s = "([Ljava/lang/String;Ljava/util/Hashtable<**>;)Ljava/lang/Object;";
        let mut chars = &mut s.char_indices();
        let r = consume_method_signature(&mut chars);
        assert_eq!(None, chars.next());
        let r = r.expect("signature not parsed successfully");
        assert!(r.type_params.is_empty());
        assert_eq!(2, r.parameters.len());
        assert!(matches!(
            r.parameters[0],
            JavaType::Reference(ReferenceType::ArrayType {
                dimension: 1,
                ty: _
            })
        ));
    }

    #[test]
    fn test_consume_type_parameters() {
        for s in &[
            "<T::Ljava/io/Serializable;>",
            "<T::Ljava/io/Serializable;:Ljava/lang/Comparable<TT;>;>",
            "<K:Ljava/lang/Object;V:Ljava/lang/Object;>",
            "<OP::Ljdk/incubator/vector/VectorOperators$Operator;T:Ljava/lang/Object;>",
        ] {
            let mut chars = &mut s.char_indices();
            assert_eq!(
                Ok(None),
                consume_type_parameters(&mut chars).map(|(_, next)| next),
                "failed to recognize: `{}`",
                s
            );
        }
    }

    #[test]
    fn test_structure_consume_type_parameters() {
        for s in &[
            "<Entity::Lfoo/bar/core/model/Identifiable<TId;>;Id::Ljava/io/Serializable;>",
            "<K:Ljava/lang/Object;V:Ljava/lang/Object;>",
        ] {
            let (params, next) =
                consume_type_parameters(&mut s.char_indices()).expect("invalid signature");
            assert_eq!(None, next, "failed on signature: {}", s);
            assert_eq!(2, params.len(), "on signature: {}", s);
        }
    }

    #[test]
    fn test_consume_unqualified_identifer() {
        let chars = &mut ".".char_indices();
        assert!(consume_unqualified_identifer(chars).is_err());

        let chars = &mut "foobar".char_indices();
        assert_eq!(
            Ok(((0..6).into(), None)),
            consume_unqualified_identifer(chars)
        );

        let chars = &mut "foo/bar".char_indices();
        assert_eq!(
            Ok(((0..3).into(), Some((3, '/')))),
            consume_unqualified_identifer(chars)
        );
        assert_eq!(Some((4, 'b')), chars.next());
    }
}

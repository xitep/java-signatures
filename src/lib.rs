/// Validates/Parses Java Type Signatures according to the syntax specified by:
/// https://docs.oracle.com/javase/specs/jvms/se21/html/jvms-4.html#jvms-4.7.9.1
mod display;
mod internal;

// --------------------------------------------------------------------

use std::fmt::Display;

/// A parsed field signature; encodes the (possibly parameterized)
/// type of a field, formal parameter, local variable, or record
/// component declaration.
///
/// See: https://docs.oracle.com/javase/specs/jvms/se21/html/jvms-4.html#jvms-4.7.9.1-610
pub struct FieldSignature<'a>(pub ReferenceType<'a>);

/// A parse class signature; encodes type information about a
/// (possibly generic) class or interface declaration. It describes
/// any type parameters of the class or interface, and lists its
/// (possibly parameterized) direct superclass and direct
/// superinterfaces, if any. A type parameter is described by its
/// name, followed by any class bound and interface bounds.
///
/// See: https://docs.oracle.com/javase/specs/jvms/se21/html/jvms-4.html#jvms-4.7.9.1-410
pub struct ClassSignature<'a> {
    pub type_params: Vec<TypeParameter<'a>>,
    pub super_class: ClassType<'a>,
    pub super_ifaces: Vec<ClassType<'a>>,
}

/// A parsed method signature; encodes type information about a
/// (possibly generic) method declaration. It describes any type
/// parameters of the method; the (possibly parameterized) types of
/// any formal parameters; the (possibly parameterized) return type,
/// if any; and the types of any exceptions declared in the method's
/// throws clause.
///
/// https://docs.oracle.com/javase/specs/jvms/se21/html/jvms-4.html#jvms-4.7.9.1-510
pub struct MethodSignature<'a> {
    pub type_params: Vec<TypeParameter<'a>>,
    pub parameters: Vec<JavaType<'a>>,
    pub result: ResultType<'a>,
    pub throws: Vec<ThrowsType<'a>>,
}

/// a primitive type
pub use internal::BaseType;

/// a primitive or reference type
pub enum JavaType<'a> {
    Base(BaseType),
    Reference(ReferenceType<'a>),
}

impl<'a> JavaType<'a> {
    fn from_internal(s: &'a str, internal: internal::JavaType) -> Self {
        match internal {
            internal::JavaType::Base(b) => JavaType::Base(b),
            internal::JavaType::Reference(r) => {
                JavaType::Reference(ReferenceType::from_internal(s, r))
            }
        }
    }
}

pub enum TypeArgument<'a> {
    /// *; `<?>`
    Unbounded,
    /// (empty); `<ReferenceType>`
    Default(ReferenceType<'a>),
    /// extends; `<? extends ReferenceType>`
    Extends(ReferenceType<'a>),
    /// super; `<? super ReferenceType>`
    Super(ReferenceType<'a>),
}

pub struct SimpleClassType<'a> {
    pub name: &'a str,
    pub type_args: Vec<TypeArgument<'a>>,
}

impl<'a> SimpleClassType<'a> {
    fn from_internal(s: &'a str, internal: internal::SimpleClassType) -> Self {
        Self {
            name: internal.0.apply(s),
            type_args: internal
                .1
                .into_iter()
                .map(|ta| match ta {
                    internal::TypeArgument::Unbounded => TypeArgument::Unbounded,
                    internal::TypeArgument::Default(ty) => {
                        TypeArgument::Default(ReferenceType::from_internal(s, ty))
                    }
                    internal::TypeArgument::Extends(ty) => {
                        TypeArgument::Extends(ReferenceType::from_internal(s, ty))
                    }
                    internal::TypeArgument::Super(ty) => {
                        TypeArgument::Super(ReferenceType::from_internal(s, ty))
                    }
                })
                .collect(),
        }
    }
}

/// Represents (a possibly nested) class type reference.  `base +
/// nesting` together denote the ultimate path of the described class.
/// This is, the last element of the concatenation of `base` and `nesting`
/// denotes the simple name of the described class.
///
/// `base` alone represents the top-leve class, while `nesting`
/// denotes the recursive nesting within it.
pub struct ClassType<'a> {
    pub base: SimpleClassType<'a>,
    pub nesting: Vec<SimpleClassType<'a>>,
}

impl<'a> ClassType<'a> {
    fn from_internal(s: &'a str, internal: internal::ClassType) -> Self {
        Self {
            base: SimpleClassType::from_internal(s, internal.0),
            nesting: internal
                .1
                .into_iter()
                .map(|ty| SimpleClassType::from_internal(s, ty))
                .collect(),
        }
    }
}

pub enum ReferenceType<'a> {
    /// a class type, potential nesting split up by class level nesting
    ClassType(ClassType<'a>),
    /// a type variable
    TypeVariable(&'a str),
    /// an array type
    ArrayType {
        dimension: usize,
        ty: Box<JavaType<'a>>,
    },
}

impl<'a> ReferenceType<'a> {
    fn from_internal(s: &'a str, internal: internal::ReferenceType) -> Self {
        match internal {
            internal::ReferenceType::ClassType(ty) => {
                ReferenceType::ClassType(ClassType::from_internal(s, ty))
            }
            internal::ReferenceType::TypeVariable(r) => ReferenceType::TypeVariable(r.apply(s)),
            internal::ReferenceType::ArrayType { dimension, ty } => ReferenceType::ArrayType {
                dimension,
                ty: Box::new(JavaType::from_internal(s, *ty)),
            },
        }
    }
}

pub struct TypeParameter<'a> {
    pub name: &'a str,
    pub class_bound: Option<ReferenceType<'a>>,
    pub iface_bounds: Vec<ReferenceType<'a>>,
}

impl<'a> TypeParameter<'a> {
    fn from_internal(s: &'a str, internal: internal::TypeParameter) -> Self {
        Self {
            name: internal.name.apply(s),
            class_bound: internal
                .class_bound
                .map(|bound| ReferenceType::from_internal(s, bound)),
            iface_bounds: internal
                .iface_bounds
                .into_iter()
                .map(|bound| ReferenceType::from_internal(s, bound))
                .collect(),
        }
    }
}

pub enum ResultType<'a> {
    VoidType,
    ValueType(JavaType<'a>),
}

impl<'a> ResultType<'a> {
    fn from_internal(s: &'a str, internal: internal::ResultType) -> Self {
        match internal {
            internal::ResultType::VoidType => ResultType::VoidType,
            internal::ResultType::ValueType(ty) => {
                ResultType::ValueType(JavaType::from_internal(s, ty))
            }
        }
    }
}

pub enum ThrowsType<'a> {
    ClassType(ClassType<'a>),
    TypeVariable(&'a str),
}

impl<'a> ThrowsType<'a> {
    fn from_internal(s: &'a str, internal: internal::ThrowsType) -> Self {
        match internal {
            internal::ThrowsType::ClassType(ty) => {
                ThrowsType::ClassType(ClassType::from_internal(s, ty))
            }
            internal::ThrowsType::TypeVariable(name) => ThrowsType::TypeVariable(name.apply(s)),
        }
    }
}

// --------------------------------------------------------------------

#[derive(Debug)]
pub struct ParseError<'a> {
    signature: &'a str,
    internal: internal::ParseError,
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.internal.error)
    }
}

impl<'a> ParseError<'a> {
    fn new(signature: &'a str, internal: internal::ParseError) -> Self {
        Self {
            signature,
            internal,
        }
    }

    /// Retrieves the error position in the parsed signature string.
    pub fn position(&self) -> usize {
        self.internal.position
    }

    /// Retrieves the parser error messages revealing what went wrong.
    ///
    /// The `Display` implementation of `ParseError` prints exactly
    /// this (and only this) string.
    pub fn message(&self) -> &str {
        &self.internal.error
    }

    /// Retrieves the (parse) context in which the error
    /// occurred. Useful only for debugging the parser.
    pub fn context(&self) -> &str {
        self.internal.context
    }

    /// Retrieves the original, full signature string which failed to
    /// parsed and led to this error.
    pub fn signature(&self) -> &'a str {
        self.signature
    }

    /// Returns the unconsumed portion of the parsed signature string
    /// at which the error occurred.
    pub fn unconsumed(&self) -> &'a str {
        &self.signature[self.internal.position..]
    }

    /// Retrieves the successfully consumed portion of the parsed
    /// signature string.
    pub fn consumed(&self) -> &'a str {
        &self.signature[..self.internal.position]
    }
}

pub type Result<'a, T> = std::result::Result<T, ParseError<'a>>;

// --------------------------------------------------------------------

/// Attempts to parse the given string as a [field signature.](FieldSignature)
pub fn parse_field_signature(s: &str) -> Result<'_, FieldSignature<'_>> {
    internal::parse(
        "FieldSignature",
        internal::consume_reference_type_signature,
        s,
        str::char_indices,
    )
    .map(|ty| FieldSignature(ReferenceType::from_internal(s, ty)))
    .map_err(|e| ParseError::new(s, e))
}

/// Convenience method to parse the given string as a [field
/// signature](FieldSignature) returning `true` upon success, `false`
/// otherwise.
pub fn is_field_signature(s: &str) -> bool {
    parse_field_signature(s).is_ok()
}

// --------------------------------------------------------------------

/// Attempts to parse the given string as a [class signature.](ClassSignature)
pub fn parse_class_signature(s: &str) -> Result<ClassSignature<'_>> {
    internal::parse(
        "ClassSignature",
        internal::consume_class_signature,
        s,
        str::char_indices,
    )
    .map(|parsed| ClassSignature {
        type_params: parsed
            .type_params
            .into_iter()
            .map(|p| TypeParameter::from_internal(s, p))
            .collect(),
        super_class: ClassType::from_internal(s, parsed.super_class),
        super_ifaces: parsed
            .super_ifaces
            .into_iter()
            .map(|ty| ClassType::from_internal(s, ty))
            .collect(),
    })
    .map_err(|e| ParseError::new(s, e))
}

pub fn is_class_signature(s: &str) -> bool {
    parse_class_signature(s).is_ok()
}

pub fn parse_method_signature(s: &str) -> Result<MethodSignature<'_>> {
    internal::parse(
        "MethodSignature",
        internal::consume_method_signature,
        s,
        str::char_indices,
    )
    .map(|parsed| MethodSignature {
        type_params: parsed
            .type_params
            .into_iter()
            .map(|p| TypeParameter::from_internal(s, p))
            .collect(),
        parameters: parsed
            .parameters
            .into_iter()
            .map(|p| JavaType::from_internal(s, p))
            .collect(),
        result: ResultType::from_internal(s, parsed.result),
        throws: parsed
            .throws
            .into_iter()
            .map(|ty| ThrowsType::from_internal(s, ty))
            .collect(),
    })
    .map_err(|e| ParseError::new(s, e))
}

pub fn is_method_signature(s: &str) -> bool {
    parse_method_signature(s).is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_field_signature() {
        assert!(!is_field_signature(""));
        assert!(!is_field_signature(" "));
        assert!(!is_field_signature(";"));
        assert!(!is_field_signature("<TT;>"));
        // ~ byte; base-type to be rejected; field signatures parse only reference-types
        assert!(!is_field_signature("B"));
        for s in &[
            "TT;",   // T
            "[[TT;", // T[][]
        ] {
            assert!(!is_field_signature(&s[..s.len() - 1]));
            assert!(!is_field_signature(&format!("{} ", s)));
            assert!(
                is_field_signature(s),
                "expected valid signature (but failed): {}",
                s
            );
        }
    }

    #[test]
    fn test_is_class_signature() {
        for s in &[
            "Ljava/lang/Enum<Lcom/google/common/base/CaseFormat;>;", // com.google.common.base.CaseFormat (abstract enum)
            "<T::Ljava/io/Serializable;:Ljava/lang/Comparable<TT;>;>Ljava/lang/Object;", // class Bar<T extends Serializable & Comparable<T>> {..}
            "<K:Ljava/lang/Object;V:Ljava/lang/Object;>Ljava/lang/Object;", // com/sun/org/apache/xalan/internal/xsltc/compiler/util/MultiHashtable.class (openjdk21)
            "Ljava/lang/Object;Ljava/util/Map<Ljava/lang/String;Ljava/util/List<Ljava/lang/String;>;>;", // jdk.httpserver/com/sun/net/httpserver/Headers class signature (openjdk16)
            "<D:Ljava/lang/Object;N::Lcom/sun/tools/javac/util/GraphUtils$DottableNode<TD;TN;>;>Lcom/sun/tools/javac/util/GraphUtils$NodeVisitor<TD;TN;Ljava/lang/StringBuilder;>;", // jdk.compiler/classes/com/sun/tools/javac/util/GraphUtils$DotVisitor.class (openjdk21)
            "<OP::Ljdk/incubator/vector/VectorOperators$Operator;T:Ljava/lang/Object;>Ljava/lang/Object;", // jdk.incubator.vector/classes/jdk/incubator/vector/VectorOperators$ImplCache.class (openjdk21)
            "<K:Ljava/lang/Object;>Ljdk/internal/loader/AbstractClassLoaderValue<Ljdk/internal/loader/AbstractClassLoaderValue<TCLV;TV;>.Sub<TK;>;TV;>;", // java.base/classes/jdk/internal/loader/AbstractClassLoaderValue$Sub.class (openjdk21)
            "Ljava/lang/invoke/ClassSpecializer<Ljava/lang/invoke/BoundMethodHandle;Ljava/lang/String;Ljava/lang/invoke/BoundMethodHandle$SpeciesData;>.Factory;", // java.base/classes/java/lang/invoke/BoundMethodHandle$Specializer$Factory.class (openjdk21)
        ] {
            {
                let s = &s[..s.len() - 1];
                assert!(!is_class_signature(s), "failed to reject: `{}`", s);
            }
            {
                let s = &format!("{} ", s);
                assert!(!is_class_signature(s), "failed to reject: `{}`", s);
            }
            if let Err(e) = parse_class_signature(s) {
                panic!("failed to recognize `{s}` as class signature: {e}");
            }
        }
    }

    #[test]
    fn test_is_method_signature() {
        for s in &[
            "()TE;",                                  // CopyOnWriteArrayList$COWSubListIterator#E next()
            "(TE;)V", // CopyOnWriteArrayList$COWSubListIterator#add(E);
            "(Ljava/util/function/Consumer<-TE;>;)V", // CopyOnWriteArrayList$COWSubListIterator#forEachRemaining(java.util.function.Consumer<? super E>)
            "<T:Ljava/lang/Object;>([TT;)[TT;", // ArrayList#void sort(java.util.Comparator<? super E>);
        ] {
            {
                let s = &s[..s.len() - 1];
                assert!(!is_method_signature(s), "failed to reject: `{}`", s);
            }
            {
                let s = &format!("{} ", s);
                assert!(!is_method_signature(s), "failed to reject: `{}`", s);
            }
            assert!(is_method_signature(s), "failed to recognize: `{}`", s);
        }
    }
}

## java-signatures
A parser for Java's class / method / field signatures as described by
the [class file format](https://docs.oracle.com/javase/specs/jvms/se21/html/jvms-4.html#jvms-4.7.9.1)
allowing access to generic type information.

There is a number of [rust crates](https://crates.io/search?q=%23classfile%20%23java)
to parse Java's classfiles and give access to the signatures, albeit,
merely as raw strings.  This small library attempts to fill the gap.

## example
```rust
// ~ a signature corresponding to a `class Bar<T extends Serializable & Comparable<T>> {..}`
let s = "<T::Ljava/io/Serializable;:Ljava/lang/Comparable<TT;>;>Ljava/lang/Object;";
match parse_class_signature(s) {
    Ok(parsed) => {
        // ~ access to the individual parts of the signature
        assert_eq!(1, parsed.type_params.len());
        assert_eq!("T", parsed.type_params[0].name);
        assert!(parsed.type_params[0].class_bound.is_none());
        assert_eq!(2, parsed.type_params[0].iface_bounds.len());
        assert!(matches!(
            &parsed.type_params[0].iface_bounds[0],
            ReferenceType::ClassType(ClassType {
                base: SimpleClassType {
                    name: "java/io/Serializable",
                    ..
                },
                ..
            })
        ));
        // ...

        // ~ the `Display` implementation of the parsed
        // signature will produce the original signature
        // string again
        assert_eq!(s, format!("{parsed}"));
    }
    Err(e) => {
        eprintln!("invalid class signature:");
        eprintln!("> {}", e.signature());
        eprintln!("> {}^-- {e}", " ".repeat(e.position()));
    }
}
```

## status
All of OpenJDK's 21 class, method, and field signatures parse
correctly.  This is, the parsed form serialized again to a signature
yields the same string.

At this stage, the library does the right thing at providing
structured access to the encoded signature data.  While avoiding
copies of the parsed string, no focused effort has been spent on
performance yet, though.

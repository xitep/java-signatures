## java-signatures
A parser for Java's class / method / field signatures as described by
the [class file format](https://docs.oracle.com/javase/specs/jvms/se21/html/jvms-4.html#jvms-4.7.9.1)
allowing access to generic type information.

There is a number of [rust crates](https://crates.io/search?q=%23classfile%20%23java)
to parse Java's classfiles and give access to the signatures, albeit,
merely as raw strings.  This small library attempts to fill the gap.

## example
```
        let s = "<K:Ljava/lang/Object;V:Ljava/lang/Object;>Ljava/lang/Object;";
        match parse_class_signature(s) {
            Ok(parsed) => {
                assert_eq!("K", parsed.type_params[0].name);
                // ...
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
copies of the parsed string, no focused effort has been spend on
performance (yet.)

## java-signatures

A parser for Java's class / method / field signatures as described by
the [class file format](https://docs.oracle.com/javase/specs/jvms/se21/html/jvms-4.html#jvms-4.7.9.1)
allowing access to generic type information.

There is a number of [rust crates](https://crates.io/search?q=%23classfile%20%23java)
to parse Java's classfiles and give access to the signatures, albeit,
merely as raw strings.  This small library attempts to fill the gap.


## status

All of OpenJDK's 21 class, method, and field signatures parse
correctly.  This is, the parsed form serialized again to a signature
yields the same string.

At this stage, the library does the right thing at providing
structured access to the encoded signature data.  While avoiding
copies of the parsed string, no focused effort has been spent on
performance yet, though.


## testing

Apart of the embedded unit tests, `test/parse.rs` will pick up any
`.class` file below `test/parse/**`, extract their class / method /
field signature strings using
[`cafebabe`](https://github.com/staktrace/cafebabe), parse them using
`java-signatures` and validate that the parsed signatures correctly
serialize back to their original form.  To keep the repository small,
we do not host any class files there permanently.


## alternatives

- the [cfsp](https://crates.io/crates/cfsp) project provides parsed
  signatures as well. Unlike `java-signatures` the parsed model owns
  the type information / names, hence, allocates, but may be easier to
  work with as it retains no reference to the originally parsed
  string.

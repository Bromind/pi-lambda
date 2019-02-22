# prerelease α-0.ε - A babbling prerelease

- [✓] Concurrent interpreter for πλ

# release α-0.1 - A public release

- [✓] Concurrent interpreter of πλ-t (type term, then interpret untyped one)
- [✓] Typer for πλ-t
    + [✓] Accept arguments to be channel: \a -> \b -> a[b].a
    + [✓] Check depth order.
    + [✓] NotImplemented: #c. #c'. c[c'].c
    + [✓] Channel should not leak: #c. #c'. c, x > c'[x].x
- [✓] Change version name
- [✓] Made available to the public

# release α-0.2 - An extended release

- [ ] User defined types
    + [✓] UnificationError: type church: ((a -> a) -> (a -> a)). \a -> \b -> ((church) (a)) (b)
    + [✓] Single constructor, easy types
    + [ ] All types
      * [ ] channel type: `type typ: (<a^0, b^1> -> b^1)`, used like #u. \v -> ((typ) (u)) (v)
      * [ ] parallel type: `type typ: [ a^0 x b^0 ]`
    + [✓] Multiple constructors
    + [ ] Detect when a result type match a defined type
    + [ ] Merge Tdot & Tchan together ??
    + [ ] Pattern matching
- [ ] Recursive functions
- [ ] Add option : `--show-ast`
- [ ] Change version name

# release α-0.3 - An named release

- [ ] Find a *nice* and *meaningful* name
- [ ] Have a logotype
- [ ] Change version name

# release α-0.4 - A extensible release

- [ ] Multiple files support
- [ ] Library support
- [ ] Standard library
- [ ] Change version name

# release 0.1 - A usable release

- [ ] Fulfill α-0.* requirements
- [ ] Standard data types and standard functions
- [ ] Generate LLVM IR
- [ ] Documentation
- [ ] Change version name

# release 1.0 - A trustworthy release

- [ ] Formal specification of correctness (Coq)
- [ ] Formal proof of correctness (Coq)
- [ ] Change version name

# Bonus track

- [✓] read/eval/print loop
- [ ] documentation (typing rules, etc)

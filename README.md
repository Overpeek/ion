# Ion

Simple type-inferred statically-typed language

```rust
print("Hello") // printing and other built-in functions are WIP

a = fn() {
    b = fn() {
        return 4
    }
    return b
}
x = 4 + 4
return (a())() + x
```

I like to call it "autogeneric" because everything is generic/templated by default.

# Ion

Simple type-inferred statically-typed language

```rust
// currently working:
fn add(l: i32, r: i32): i32 {
    return l + r;
}

fn main(): none {
    print(add(4, 5));
    let x = 6;
    print(x);
    x *= 2;
    print(x);
}

// todo:
// optionally fully type-inferred
fn add(l, r) {
    // optional semicolons
    l + r
}

// statements in module level
// str sum (concat)
print(add("Hello, ", "world!"))
print(add(4, 5))

// format strings
print(f"4+5={add(4, 5)}")

// anon.functions / lamdas / closures
let sub = fn(a, b) {
    a - b
}

let x = 5
fn captures() {
    print(x)
}
captures()

// undecided:

// partial eval using captures
// not very readable?
let add_5 = add(5)
```
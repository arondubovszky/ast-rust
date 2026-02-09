# Pavo

A statically-structured, Rust-like programming language with a tree-walking interpreter, built in Rust.

## Quick Start

```bash
cargo build --release
./target/release/pavo program.pv
```

The interpreter looks for a `main()` function and executes it.

## Hello World

```pavo
fn main() -> void {
    putln "Hello, world!";
}
```

Or with the standard library:

```pavo
import "std.pv";

fn main() -> void {
    println("Hello, world!");
}
```

## Types

### Primitives

| Type   | Alias | Description              |
|--------|-------|--------------------------|
| `i32`  | `int` | 32-bit signed integer    |
| `i64`  |       | 64-bit signed integer    |
| `f32`  |       | 32-bit float             |
| `f64`  |       | 64-bit float             |
| `bool` |       | Boolean (`true`/`false`) |
| `str`  |       | String                   |
| `void` |       | No return value          |

### Arrays

```pavo
let nums: [i32] = [1, 2, 3, 4, 5];
```

Create arrays of a given size with a fill value using the `[size; value]` syntax:

```pavo
let zeros: [i32] = [10; 0];     // 10 zeros
let grid: [str] = [5; "empty"]; // 5 copies of "empty"
```

Get the length of an array (or string) with `len`:

```pavo
let n: i32 = len nums;       // 5
let s: i32 = len "hello";    // 5
```

`len` is a built-in operator, so both `len arr` and `len(arr)` work.

### Structs

```pavo
struct Point {
    x: f64,
    y: f64,
}

let p: Point = Point { x: 3.0, y: 4.0 };
println(p.x);    // 3
p.x = 10.0;
```

## Variables

Declare with `let` and a type annotation:

```pavo
let x: i32 = 10;
let name: str = "Alice";
let pi: f64 = 3.14159;
```

Shorthand with `:=` (infers as `i32`):

```pavo
x := 5;
```

Reassignment:

```pavo
x = 20;
```

## Operators

### Arithmetic

| Op   | Description | Example   |
|------|-------------|-----------|
| `+`  | Add         | `a + b`   |
| `-`  | Subtract    | `a - b`   |
| `*`  | Multiply    | `a * b`   |
| `/`  | Divide      | `a / b`   |
| `%`  | Modulo      | `a % b`   |
| `**` | Power       | `2 ** 10` |

### Comparison

`==`, `!=`, `<`, `>`, `<=`, `>=`

### Logical

`&&`, `||`, `!`

### Bitwise

`&`, `|`, `^`, `~`

### Postfix

`++` (increment), `--` (decrement) — mutate in place.

### String concatenation

`+` concatenates strings and coerces other types:

```pavo
putln "value: " + 42;       // "value: 42"
putln "pi is " + 3.14;      // "pi is 3.14"
```

## Control Flow

### If / Else

```pavo
if x > 0 {
    putln "positive";
} else if x < 0 {
    putln "negative";
} else {
    putln "zero";
}
```

### While

```pavo
let i: i32 = 0;
while i < 10 {
    putln i;
    i++;
}
```

### For

C-style for loops:

```pavo
for (let i: i32 = 0; i < 10; i++) {
    putln i;
}

// shorthand init
for (i := 0; i < 5; i++) {
    putln i;
}
```

### Break and Continue

```pavo
while true {
    if done {
        break;
    }
    continue;
}
```

## Functions

Functions require typed parameters and a return type:

```pavo
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

fn greet(name: str) -> void {
    putln "Hello, " + name + "!";
}
```

Recursion works:

```pavo
fn factorial(n: i32) -> i32 {
    if n <= 1 {
        return 1;
    }
    return n * factorial(n - 1);
}
```

### Function Overloading

Functions can be overloaded by parameter types. The interpreter picks the right version based on the argument types at runtime:

```pavo
fn describe(x: i32) -> str {
    return "an integer: " + x;
}

fn describe(x: str) -> str {
    return "a string: " + x;
}

fn main() -> void {
    putln describe(42);       // "an integer: 42"
    putln describe("hello");  // "a string: hello"
}
```

### Method Syntax (UFCS)

Any function can be called with dot syntax. `x.foo(y)` is identical to `foo(x, y)`. This is called Uniform Function Call Syntax:

```pavo
fn double(x: i32) -> i32 {
    return x + x;
}

fn main() -> void {
    putln double(5);     // 10
    putln 5.double();    // 10 — same thing

    // chaining
    5.double().double(); // 20
}
```

This works with any function — the receiver becomes the first argument. Combined with overloading, you get method-like ergonomics without a class system:

```pavo
import "std.pv";

fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

fn main() -> void {
    3.add(4).println();  // 7
}
```

## Modules and Imports

Split code across files with `import`:

```pavo
import "math.pv";
import "../lib/utils.pv";
```

Imports are resolved relative to the importing file's directory. All functions and structs from the imported file are merged into the global scope (like Python's `from module import *`).

### Every file can have `main()`

Each `.pv` file can define its own `main()` function. It only runs when that file is the entry point:

```pavo
// utils.pv
fn greet(name: str) -> void {
    putln "Hello, " + name;
}

fn main() -> void {
    // only runs via: pavo utils.pv
    greet("from utils");
}
```

```pavo
// app.pv
import "utils.pv";

fn main() -> void {
    // utils.pv's main() is ignored here
    greet("from app");
}
```

```bash
pavo app.pv    # prints "Hello, from app"
pavo utils.pv  # prints "Hello, from utils"
```

Circular imports are handled automatically — files are only loaded once.

## Standard Library

Import the standard library with:

```pavo
import "std.pv";
```

It provides:

| Function                            | Description                    |
|-------------------------------------|--------------------------------|
| `println(x: i32/i64/f32/f64/str/bool)` | Print with newline         |
| `print(x: i32/i64/f32/f64/str/bool)`   | Print without newline      |
| `array_new(size: i32, fill: i32)`   | Create a sized `Array` struct  |
| `array_get(arr: Array, idx: i32)`   | Get element from `Array`       |

The stdlib also defines the `Array` struct (data + size pair):

```pavo
struct Array {
    data: [i32],
    size: i32,
}
```

### Built-in vs stdlib

Some operations are built into the language itself (not part of `std.pv`):

| Built-in   | Description                    | Example          |
|------------|--------------------------------|------------------|
| `put`      | Print without newline          | `put "hi";`      |
| `putln`    | Print with newline             | `putln 42;`      |
| `len`      | Length of array or string      | `len arr`        |

The `println`/`print` functions in the stdlib are wrappers around `put`/`putln` that support function call syntax and UFCS (`42.println()`).

## Print

There are two layers of printing:

**Built-in statements** — always available, no import needed:

```pavo
put "no newline";
putln "with newline";
```

**Stdlib functions** — require `import "std.pv"`, support UFCS:

```pavo
import "std.pv";

print("no newline");
println("with newline");
"hello".println();
42.println();
```

## Complete Example

```pavo
import "std.pv";

struct Vec2 {
    x: f64,
    y: f64,
}

fn vec2(x: f64, y: f64) -> Vec2 {
    return Vec2 { x: x, y: y };
}

fn add(a: Vec2, b: Vec2) -> Vec2 {
    return vec2(a.x + b.x, a.y + b.y);
}

fn scale(v: Vec2, s: f64) -> Vec2 {
    return vec2(v.x * s, v.y * s);
}

fn dot(a: Vec2, b: Vec2) -> f64 {
    return a.x * b.x + a.y * b.y;
}

fn display(v: Vec2) -> void {
    println("(" + v.x + ", " + v.y + ")");
}

fn is_prime(n: i32) -> bool {
    if n < 2 { return false; }
    for (i := 2; i * i <= n; i++) {
        if n % i == 0 { return false; }
    }
    return true;
}

fn sort(arr: [i32]) -> [i32] {
    let n: i32 = len arr;
    for (i := 0; i < n - 1; i++) {
        for (j := 0; j < n - 1 - i; j++) {
            if arr[j] > arr[j + 1] {
                let tmp: i32 = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = tmp;
            }
        }
    }
    return arr;
}

fn print_arr(arr: [i32]) -> void {
    print("[");
    for (i := 0; i < len arr; i++) {
        if i > 0 { print(", "); }
        print(arr[i]);
    }
    println("]");
}

fn main() -> void {
    // Vec2 with UFCS
    let a: Vec2 = vec2(3.0, 4.0);
    let b: Vec2 = vec2(1.0, 2.0);
    a.add(b).display();           // (4, 6)
    a.scale(2.0).display();       // (6, 8)
    a.dot(b).println();           // 11

    // Primes
    for (i := 2; i <= 30; i++) {
        if is_prime(i) {
            print(i);
            print(" ");
        }
    }
    println("");

    // Array ops
    let nums: [i32] = [5, 3, 8, 1, 9, 2, 7, 4, 6];
    nums.sort().print_arr();      // [1, 2, 3, 4, 5, 6, 7, 8, 9]
}
```

## Notes

Feature annotations that wrap top-level items:

```pavo
#[note("pure")]
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
```

## Architecture

The implementation lives in `src/`:

| File            | Description                                           |
|-----------------|-------------------------------------------------------|
| `grammar.pest`  | PEG grammar (parsed by [pest](https://pest.rs))      |
| `parser.rs`     | Pest parser, Pratt expression parser, AST construction|
| `ast.rs`        | AST node types, tree-walking execution engine         |
| `types.rs`      | `Type` enum (Int32, Str, Array, Object, etc.)         |
| `functions.rs`  | Function representation and calling convention        |
| `emit.rs`       | AST-to-S-expression serialization                     |
| `main.rs`       | CLI entry point, module loader                        |

## Building and Testing

```bash
cargo build --release    # binary at target/release/pavo
cargo test               # run all tests
```

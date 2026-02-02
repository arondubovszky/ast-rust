# Pavo

A statically-structured, Rust-like programming language with a tree-walking interpreter, built in Rust.

## Quick Start

```bash
cargo build --release
./target/release/pavo program.pv
```

The interpreter looks for a `main()` function and executes it.

## Syntax Overview

Pavo uses Rust-like syntax with some simplifications. Comments use `//`.

```pavo
// This is a comment

fn main() -> void {
    println("Hello, world!");
}
```

## Types

### Primitives

| Type   | Description              |
|--------|--------------------------|
| `i32`  | 32-bit signed integer    |
| `i64`  | 64-bit signed integer    |
| `f32`  | 32-bit float             |
| `f64`  | 64-bit float             |
| `bool` | Boolean (`true`/`false`) |
| `str`  | String                   |
| `void` | No return value          |

### Arrays

```pavo
let nums: [i32] = [1, 2, 3, 4, 5];
let nested: [[i32]] = [[1, 2], [3, 4]];
```

### Optional types

Append `!` to any type to mark it as optional (nullable):

```pavo
let maybe: i32! = null;
```

### User-defined structs

```pavo
struct Point {
    x: f64,
    y: f64,
}
```

## Literals

```pavo
42          // i32
-7          // negative integer
3.14        // f64
"hello"     // str
true        // bool
false       // bool
null        // null
[1, 2, 3]  // array
```

## Variables

Variables are declared with `let` and require a type annotation:

```pavo
let x: i32 = 10;
let name: str = "Alice";
let pi: f64 = 3.14159;
```

Assignment to existing variables:

```pavo
x = 20;
x = x + 5;
```

## Operators

### Arithmetic

| Operator | Description    | Example     |
|----------|----------------|-------------|
| `+`      | Add            | `a + b`     |
| `-`      | Subtract       | `a - b`     |
| `*`      | Multiply       | `a * b`     |
| `/`      | Divide         | `a / b`     |
| `%`      | Modulo         | `a % b`     |
| `**`     | Power          | `2 ** 10`   |

### Comparison

| Operator | Description      |
|----------|------------------|
| `==`     | Equal            |
| `!=`     | Not equal        |
| `<`      | Less than        |
| `>`      | Greater than     |
| `<=`     | Less or equal    |
| `>=`     | Greater or equal |

### Logical

| Operator | Description |
|----------|-------------|
| `&&`     | And         |
| `\|\|`   | Or          |
| `!`      | Not         |

### Bitwise

| Operator | Description |
|----------|-------------|
| `&`      | And         |
| `\|`     | Or          |
| `^`      | Xor         |
| `~`      | Not         |

### Prefix

| Operator | Description  | Example |
|----------|--------------|---------|
| `-`      | Negate       | `-x`    |
| `!`      | Logical not  | `!flag` |
| `~`      | Bitwise not  | `~bits` |

### Postfix

| Operator | Description | Example    |
|----------|-------------|------------|
| `++`     | Increment   | `counter++` |
| `--`     | Decrement   | `counter--` |

`++` and `--` mutate the variable in place.

### String concatenation

The `+` operator concatenates strings and coerces other types to strings:

```pavo
println("value: " + 42);       // "value: 42"
println("pi is " + 3.14);      // "pi is 3.14"
println("hello" + " " + "world");
```

## Control Flow

### If / Else

```pavo
if x > 0 {
    println("positive");
} else {
    if x < 0 {
        println("negative");
    } else {
        println("zero");
    }
}
```

### While

```pavo
let i: i32 = 0;
while i < 10 {
    println(i);
    i++;
}
```

### C-style For

```pavo
for (let i: i32 = 0; i < 10; i++) {
    println(i);
}
```

The init clause accepts `let` declarations or assignments. The update clause accepts assignments or expressions (like `i++`):

```pavo
let k: i32 = 0;
for (k = 0; k < 5; k = k + 1) {
    println(k);
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

Functions are defined with `fn`, require typed parameters, and a return type:

```pavo
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

fn greet(name: str) -> str {
    return "Hello, " + name + "!";
}

fn main() -> void {
    println(add(3, 4));
    println(greet("Pavo"));
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

fn fibonacci(n: i32) -> i32 {
    if n <= 0 { return 0; }
    if n == 1 { return 1; }
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

## Structs

### Definition

```pavo
struct User {
    id: i32,
    name: str,
    score: f64,
}
```

### Construction

```pavo
let u: User = User { id: 1, name: "Alice", score: 95.5 };
```

### Field access and assignment

```pavo
println(u.name);    // Alice
u.name = "Bob";
u.score = 88.0;
```

## Arrays

### Literals and indexing

```pavo
let arr: [i32] = [10, 20, 30, 40, 50];
println(arr[0]);    // 10
println(arr[2]);    // 30

let sum: i32 = arr[1] + arr[3];
```

## Print

Two built-in print functions:

```pavo
print("no newline");
println("with newline");
```

Strings print without quotes. Other types are formatted automatically.

## Notes

Notes are feature annotations that wrap top-level items:

```pavo
#[note("recursion")]
fn fibonacci(n: i32) -> i32 {
    if n <= 1 { return n; }
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

## Complete Example

```pavo
struct Point {
    x: f64,
    y: f64,
}

fn make_point(x: f64, y: f64) -> Point {
    return Point { x: x, y: y };
}

fn describe(p: Point) -> str {
    return "(" + p.x + ", " + p.y + ")";
}

fn main() -> void {
    // Variables and arithmetic
    let a: i32 = 10;
    let b: i32 = 20;
    println(a + b);

    // Strings
    println("Hello, " + "Pavo!");

    // Control flow
    for (let i: i32 = 0; i < 5; i++) {
        println(i);
    }

    // Structs
    let p: Point = make_point(3.0, 4.0);
    println(describe(p));

    // Recursion
    println(factorial(10));

    // Arrays
    let nums: [i32] = [1, 2, 3];
    println(nums[0] + nums[1] + nums[2]);
}

fn factorial(n: i32) -> i32 {
    if n <= 1 { return 1; }
    return n * factorial(n - 1);
}
```

## Architecture

The implementation lives in `src/` and is structured as both a library and a CLI binary:

- `grammar.pest` -- PEG grammar (parsed by [pest](https://pest.rs))
- `parser.rs` -- pest parser, Pratt expression parser, AST construction
- `ast.rs` -- AST node types, tree-walking execution engine
- `types.rs` -- `Type` enum (Int32, Int64, Float32, Float64, Str, Bool, Array, Struct, etc.)
- `functions.rs` -- function representation and calling convention
- `emit.rs` -- AST-to-source serialization
- `main.rs` -- CLI entry point (`pavo <file.pv>`)

## Building

```bash
cargo build --release
```

The binary is placed at `target/release/pavo`.

## Running Tests

```bash
cargo test
```

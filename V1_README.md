# V1 Language Syntax Specification

## 1. Literals

42              // i32

3.14            // f64

"hello"         // str

true / false    // bool

null            // null

my_var          // identifier

---

## 2. Types

```
i32  i64  f32  f64  bool  str
(array i32)           // array of i32
i32!                  // optional (nullable) i32
User                  // struct type
```

---

## 3. Variables

### Define
```
(let x i32 10)
(let name str "alice")
(let nums (array i32) (list 1 2 3))
```

### Assign
```
(assign x 20)
(assign count (+ count 1))
```

### Reference (just use the name)
```
x
count
my_variable
```

---

## 4. Expressions

### Binary operations (prefix notation)
```
(+ 1 2)           // arithmetic: + - * / % **
(< x 10)          // comparison: < > <= >= == !=
(&& a b)          // logical: && ||
```

### Unary operations
```
(! true)          // logical not
(- x)             // negate
```

### Array indexing
```
(index arr 0)
(index arr (+ i 1))
```

### Field access
```
(field user name)
(field point x)
```

---

## 5. Control Flow

### If / Elif / Else
```
(if (< x 0)
  (return "negative")
)

(if (< x 0)
  (return "negative")
  (return "non-negative")
)

// elif is nested if in else
(if (< x 0)
  (return "negative")
  (if (== x 0)
    (return "zero")
    (return "positive")
  )
)
```

### While loop
```
(while (< i 10)
  (assign i (+ i 1))
)
```

---

## 6. Functions

### Definition
```
(fn add
  (params
    (a i32)
    (b i32)
  )
  (returns i32)
  (body
    (return (+ a b))
  )
)
```

### Call
```
(call add (1 2))
(call greet ("hello"))
(call process ())
```

### Return
```
(return)
(return 42)
(return (+ a b))
```

---

## 7. Structs

### Definition
```
(struct User
  (fields
    (id i32)
    (name str)
    (email str!)
  )
)
```

### Instantiation
```
(struct User
  (id 1)
  (name "Alice")
  (email null)
)
```

### Field access
```
(field user name)
(field user id)
```

### Field assignment
```
(assign_field user name "Bob")
```

---

## 8. Methods

Methods are functions where the first parameter is `self`.

```
(struct Point
  (fields
    (x f32)
    (y f32)
  )
)

// Method on Point (self is first param)
(fn distance
  (params
    (self Point)
    (other Point)
  )
  (returns f32)
  (body
    (let dx f32 (- (field other x) (field self x)))
    (let dy f32 (- (field other y) (field self y)))
    (return (call sqrt ((+ (* dx dx) (* dy dy)))))
  )
)

// Calling a method
(call distance (p1 p2))
```

---

## 9. Complete Example

```
(struct Counter
  (fields
    (value i32)
  )
)

// Constructor (free function)
(fn new_counter
  (params
    (start i32)
  )
  (returns Counter)
  (body
    (return
      (struct Counter
        (value start)
      )
    )
  )
)

// Method: increment
(fn increment
  (params
    (self Counter)
  )
  (returns i32)
  (body
    (assign_field self value (+ (field self value) 1))
    (return (field self value))
  )
)

// Method: get value
(fn get
  (params
    (self Counter)
  )
  (returns i32)
  (body
    (return (field self value))
  )
)

// Main
(fn main
  (params)
  (returns i32)
  (body
    (let c Counter (call new_counter (0)))
    
    (while (< (call get (c)) 5)
      (call increment (c))
    )
    
    (return (call get (c)))
  )
)
```

---

## 10. Notes

Annotations that wrap any expression or statement. The note is stored in the AST but has no effect at runtime.

```
// Wrap any expression
(note "await" (call fetch_data ()))

// Wrap a statement
(note "unsafe" (assign ptr (+ ptr 1)))

// Nested notes
(note "async" 
  (note "cached"
    (call expensive_computation ())
  )
)
```

---

## Key Rules

1. **Prefix notation**: `(op arg1 arg2)` not `arg1 op arg2`
2. **Explicit types**: All variables and params have types
3. **Methods**: First param named `self` makes it a method
4. **One way only**: No alternative syntax for same thing
5. **Parentheses**: Everything is explicitly delimited

*syntax*
```
================================================================================
S-EXPRESSION LANGUAGE SYNTAX SPECIFICATION
================================================================================
A deterministic, LLM-friendly S-expression based language for AST serialization.
All syntax uses parentheses for explicit structure. No special operator precedence.


================================================================================
1. BASIC LITERALS
================================================================================

Numbers:
  42
  -123
  3.14
  -0.5

Strings:
  "hello"
  "multi word string"
  ""

Booleans:
  true
  false

Null:
  null

Identifiers (variable/function/type names):
  x
  my_variable
  snake_case_name
  _internal


================================================================================
2. TYPES
================================================================================

Primitive types:
  i32
  i64
  f32
  f64
  bool
  str

Array type:
  (array i32)
  (array str)
  (array (array f32))      ; nested arrays

Optional type (nullable):
  i32!
  str!
  (array i32)!

Named types (user-defined):
  User
  Matrix
  MyCustomType

Examples:
  (array i32)           ; array of 32-bit integers
  (array str)!          ; optional array of strings
  (array (array i32))   ; 2D array


================================================================================
3. EXPRESSIONS
================================================================================

3.1 LITERALS & VARIABLES
  42
  "hello"
  true
  null
  my_var

3.2 BINARY OPERATIONS (prefix notation)
  Arithmetic: +, -, *, /, %, **
    (+ 1 2)           ; 1 + 2
    (* 3 4)           ; 3 * 4
    (/ 10 (+ 2 3))    ; 10 / (2 + 3)
    (** 2 8)          ; 2 ** 8

  Comparison: <, >, <=, >=, ==, !=
    (< x 10)
    (== name "alice")
    (!= age 0)

  Logical: &&, ||
    (&& (> x 0) (< x 10))
    (|| a b)

  Bitwise: &, |, ^, <<, >>
    (& mask 0xFF)
    (| flags option)
    (<< x 2)

  General form:
    (OPERATOR operand1 operand2)

3.3 UNARY OPERATIONS
  !, -, ~
    (! true)           ; logical not
    (- x)              ; negate
    (~ bits)           ; bitwise not

3.4 FUNCTION CALLS
  (call function_name (arg1 arg2 arg3))
  (call fibonacci (10))
  (call add ((+ 1 2) (+ 3 4)))
  (call process_user (user data))

  Note: Arguments are expressions, can be nested

3.5 ARRAY/STRUCT FIELD ACCESS
  Index (array element):
    (index arr 0)
    (index arr (+ i 1))
    (index matrix (+ (* row cols) col))

  Field (struct member):
    (field user name)
    (field person.address zip_code)

3.6 CONDITIONAL EXPRESSION (if-then-else)
  (if condition
    then_expr
  )

  (if condition
    then_expr
    else_expr
  )

  Nested conditionals:
    (if (< x 0)
      (return "negative")
      (if (== x 0)
        (return "zero")
        (return "positive")
      )
    )

3.7 MATCH EXPRESSION
  (match value
    (pattern1 -> expr1)
    (pattern2 -> expr2)
    (_ -> default_expr)
  )

  Example:
    (match status
      (200 -> "OK")
      (404 -> "NotFound")
      (500 -> "ServerError")
      (_ -> "Unknown")
    )

3.8 LET BINDING
  (let variable_name type value_expr)

  Examples:
    (let x i32 10)
    (let name str "alice")
    (let arr (array i32) (call create_array ()))
    (let sum i32 (+ a b c))

3.9 STRUCT LITERAL
  (struct StructName
    (field_name field_value)
    (field_name2 field_value2)
  )

  Example:
    (struct User
      (id 1)
      (name "Alice")
      (email "alice@example.com")
    )

3.10 BLOCK EXPRESSION
  (block
    statement1
    statement2
    expr
  )

  Returns the value of the last expression


================================================================================
4. STATEMENTS
================================================================================

4.1 EXPRESSION STATEMENT
  Any expression can be a statement:
    (+ 1 2)
    (call print (x))
    (let x i32 5)

4.2 ASSIGNMENT
  (assign variable_name new_value)

  Examples:
    (assign x 10)
    (assign count (+ count 1))
    (assign arr[0] 42)

4.3 FIELD ASSIGNMENT
  (assign_field struct_expr field_name new_value)

  Examples:
    (assign_field user name "Bob")
    (assign_field (field person address) zip_code "12345")  ; nested field access

4.4 INDEX ASSIGNMENT (array element)
  (set_index array_expr index_expr value_expr)

  Examples:
    (set_index arr 0 100)
    (set_index matrix (+ (* row cols) col) 3.14)
    (set_index arr i (+ x y))

4.5 RETURN STATEMENT
  (return)           ; return null/void
  (return expr)      ; return value

  Examples:
    (return)
    (return 42)
    (return (+ a b))
    (return (call get_user ()))

4.6 WHILE LOOP
  (while condition
    statement1
    statement2
  )

  Example:
    (while (< i 10)
      (
        (assign result (+ result i))
        (assign i (+ i 1))
      )
    )

4.7 FOR LOOP
  (for variable in iterable
    statement1
    statement2
  )

  Example:
    (for item in items
      (
        (assign count (+ count 1))
        (call process (item))
      )
    )

4.8 BREAK & CONTINUE
  (break)
  (continue)


================================================================================
5. TOP-LEVEL DEFINITIONS
================================================================================

Note: Top-level items are: Functions (including methods), Structs, Enums, 
Type Aliases, and Notes. There are no separate trait or impl constructs.
Methods are defined as functions with "self" as the first parameter.

5.1 FUNCTION DEFINITION
  (fn function_name
    (params
      (param_name param_type)
      (param_name2 param_type2)
    )
    (returns return_type)
    (body
      statement1
      statement2
    )
  )

  FREE FUNCTION (no receiver):
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

  METHOD (has self as first parameter):
    A function is a method if its first parameter is named "self".
    The type of "self" determines which type this method belongs to.

    (fn display
      (params
        (self User)
      )
      (returns str)
      (body
        (return (+ self.name " (ID: " self.id ")"))
      )
    )

    (fn update_name
      (params
        (self User)
        (new_name str)
      )
      (returns null)
      (body
        (assign_field self name new_name)
      )
    )

    (fn distance
      (params
        (self Point)
        (other Point)
      )
      (returns f32)
      (body
        (let dx (- other.x self.x))
        (let dy (- other.y self.y))
        (return (sqrt (+ (* dx dx) (* dy dy))))
      )
    )

  KEY RULE: If params[0] is named "self", this is a method.
  The "self" parameter type indicates which struct/enum this method belongs to.

5.2 STRUCT DEFINITION
  (struct struct_name
    (fields
      (field_name field_type)
      (field_name2 field_type2)
    )
  )

  Example:
    (struct User
      (fields
        (id i32)
        (name str)
        (email str!)
      )
    )

5.3 ENUM DEFINITION
  (enum enum_name
    (variants
      (Variant1)
      (Variant2 (i32))
      (Variant3 (str i32))
    )
  )

  Example:
    (enum Result
      (variants
        (Ok (str))
        (Err (str))
        (Pending)
      )
    )

5.4 TYPE ALIAS
  (type alias_name existing_type)

  Examples:
    (type UserId i32)
    (type Coordinates (array f32))

5.5 NOTES (feature annotations)

  (note (feature_name "feature_string")
    top_level_item
  )

  Examples:
    (note (feature_name "exceptions")
      (fn divide_safe
        (params
          (a i32)
          (b i32)
        )
        (returns i32!)
        (body
          (if (== b 0)
            (return null)
            (return (/ a b))
          )
        )
      )
    )

    (note (feature_name "async")
      (fn fetch_data
        (params
          (url str)
        )
        (returns str)
        (body
          (return "data")
        )
      )
    )

    (note (feature_name "generics")
      (fn swap
        (params
          (a str)
          (b str)
        )
        (returns (array str))
        (body
          (return (list b a))
        )
      )
    )


================================================================================
6. COMPLETE PROGRAM STRUCTURE
================================================================================

A program is a sequence of top-level definitions (functions, structs, enums, 
type aliases, and notes).

Example with methods:

(struct User
  (fields
    (id i32)
    (name str)
  )
)

; Free function (no self parameter)
(fn create_user
  (params
    (id i32)
    (name str)
  )
  (returns User)
  (body
    (return
      (struct User
        (id id)
        (name name)
      )
    )
  )
)

; Method on User (has self parameter)
(fn display
  (params
    (self User)
  )
  (returns str)
  (body
    (return (+ self.name " (ID: " self.id ")"))
  )
)

; Another method on User
(fn update_name
  (params
    (self User)
    (new_name str)
  )
  (returns null)
  (body
    (assign_field self name new_name)
  )
)

; Free function
(fn main
  (params)
  (returns i32)
  (body
    (let user User (call create_user (1 "Alice")))
    (let greeting str (call display (user)))
    (call println (greeting))
    (return 0)
  )
)

; Note wrapping a method
(note (feature_name "logging")
  (fn debug_user
    (params
      (self User)
    )
    (returns str)
    (body
      (return (+ "User: " self.name))
    )
  )
)


================================================================================
7. KEY DESIGN PRINCIPLES
================================================================================

1. UNAMBIGUOUS: One feature, one way to express it
   - No trait definitions AND impl blocks
   - No optional syntax variants
   - No "two ways to do the same thing"
   - Methods are functions with "self" parameter (deterministic rule)

2. DETERMINISTIC: One AST = One text representation
   - Field order is fixed
   - Operator precedence is explicit (all prefix)
   - No optional syntax
   - Method detection: if params[0].name == "self", it's a method

3. EXPLICIT STRUCTURE: Parentheses delimit everything
   - (operator arg1 arg2)
   - (function_name params returns body)
   - (type name fields)

4. FLAT DEFINITION ORDER: No nesting of definitions
   - Functions don't contain other functions
   - Structs don't contain methods (use self parameter instead)
   - No separate impl blocks (use self parameter instead)

5. NO OPERATOR PRECEDENCE AMBIGUITY:
   - All binary operators are prefix: (+ 1 2) not 1 + 2
   - Precedence is handled by nesting: (+ (* 2 3) 4) = (2*3) + 4

6. MANDATORY TYPE ANNOTATIONS:
   - Top-level items must have explicit types
   - Parameters must have types
   - Returns must be specified
   - "self" parameter type indicates the owning type

7. NOTES FOR EXTENSIONS:
   - Features outside core language wrapped in (note ...)
   - Makes it easy to filter for training
   - Preserves full information for codegen


================================================================================
8. WHITESPACE & COMMENTS
================================================================================

Whitespace: Ignored (spaces, tabs, newlines)
Comments: ; to end of line

Examples:
  ; This is a comment
  (fn add (params (a i32) (b i32)) ; inline comment
    (returns i32)
    (body
      ; Return the sum
      (return (+ a b))
    )
  )


================================================================================
9. ROUNDTRIP GUARANTEE
================================================================================

The language is designed so:
  Code -> Parse -> AST -> Serialize -> Code'
  
Will always produce identical Code and Code' (byte-for-byte).

This means:
- No choice in formatting
- No choice in field order
- No choice in operator representation
- LLMs always see the canonical form


================================================================================
10. EXAMPLE: FIBONACCI WITH NOTES
================================================================================

(note (feature_name "recursion")
  (fn fibonacci
    (params
      (n i32)
    )
    (returns i32)
    (body
      (if (<= n 1)
        (return n)
        (return
          (+
            (call fibonacci ((- n 1)))
            (call fibonacci ((- n 2)))
          )
        )
      )
    )
  )
)


================================================================================
11. EXAMPLE: STRUCT WITH METHODS
================================================================================

(struct Point
  (fields
    (x f32)
    (y f32)
  )
)

; Free function constructor
(fn create_point
  (params
    (x f32)
    (y f32)
  )
  (returns Point)
  (body
    (return
      (struct Point
        (x x)
        (y y)
      )
    )
  )
)

; Method: distance to another point
(fn distance
  (params
    (self Point)
    (other Point)
  )
  (returns f32)
  (body
    (let dx (- other.x self.x))
    (let dy (- other.y self.y))
    (return (sqrt (+ (* dx dx) (* dy dy))))
  )
)

; Method: translate by offset
(fn translate
  (params
    (self Point)
    (dx f32)
    (dy f32)
  )
  (returns Point)
  (body
    (return
      (struct Point
        (x (+ self.x dx))
        (y (+ self.y dy))
      )
    )
  )
)

; Usage:
; (let p1 Point (call create_point (0.0 0.0)))
; (let p2 Point (call create_point (3.0 4.0)))
; (let dist f32 (call distance (p1 p2)))  ; distance method on p1
```

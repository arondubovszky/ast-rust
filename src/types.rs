use std::{
    collections::BTreeMap,
    ops::{Add, AddAssign, Div, Mul, Sub},
};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum TypeKind {
    Int32,
    Int64,
    Float32,
    Float64,
    Str,
    Bool,
    Object,
    VarRef,
    Ref,
    Void,
    Array,
    Null,
    Break,
    Continue,
    Optional,
}

pub trait Castable {
    fn lowkey_cast(self) -> Result<bool, String>;

    fn cast_up(self) -> Result<Type, String>;
    fn cast_down(self) -> Result<Type, String>;
}

/// Struct definition
#[derive(Clone, Debug)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, TypeKind)>, // (field_name, expected_type)
}

impl StructDef {
    pub fn new(name: &str) -> Self {
        StructDef {
            name: name.to_string(),
            fields: Vec::new(),
        }
    }

    pub fn with_field(mut self, name: &str, kind: TypeKind) -> Self {
        self.fields.push((name.to_string(), kind));
        self
    }

    pub fn add_field(&mut self, name: &str, kind: TypeKind) {
        self.fields.push((name.to_string(), kind));
    }

    // Validate that provided fields match the struct definition
    pub fn validate(&self, provided: &[(String, Type)]) -> Result<(), String> {
        if provided.len() != self.fields.len() {
            return Err(format!(
                "struct '{}' expects {} fields, got {}",
                self.name,
                self.fields.len(),
                provided.len()
            ));
        }

        for (expected_name, expected_kind) in &self.fields {
            let found = provided.iter().find(|(name, _)| name == expected_name);
            match found {
                Some((_, val)) => {
                    if val.get_kind() != *expected_kind {
                        return Err(format!(
                            "field '{}' expects type {:?}, got {:?}",
                            expected_name,
                            expected_kind,
                            val.get_kind()
                        ));
                    }
                }
                None => {
                    return Err(format!(
                        "missing field '{}' in struct '{}'",
                        expected_name, self.name
                    ));
                }
            }
        }

        Ok(())
    }

    // Create an Object instance from provided field values
    pub fn instantiate(&self, provided: Vec<(String, Type)>) -> Result<Type, String> {
        self.validate(&provided)?;

        let mut fields = std::collections::BTreeMap::new();
        for (name, val) in provided {
            fields.insert(name, val);
        }

        Ok(Type::Object {
            type_name: self.name.clone(),
            fields,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int32(i32),
    Int64(i64),
    Float32(f32),
    Float64(f64),
    Str(String),
    VarRef(String), // variable reference (lookup by name)
    Ref(Box<Type>), // & reference (pointer to a value)
    Bool(bool),
    Void,
    Object {
        type_name: String,
        fields: BTreeMap<String, Type>,
    }, // struct instance
    Array(Vec<Type>),
    Null,
    Break,
    Continue,
    Optional(Box<Type>), // Some(val) or None represented as Optional(Box::new(Null)),
}

impl Castable for Type {
    /// casting to bool
    fn lowkey_cast(self) -> Result<bool, String> {
        match self {
            Type::Bool(b) => Ok(b),
            Type::Int32(0) => Ok(false),
            Type::Int32(_) => Ok(true),
            Type::Int64(0) => Ok(false),
            Type::Int64(_) => Ok(true),
            Type::Str(s) => {
                if s.is_empty() {
                    Ok(false)
                } else {
                    Ok(true)
                }
            }
            Type::Float32(f) if f == 0.0 => Ok(false),
            Type::Float32(_) => Ok(true),
            Type::Float64(f) if f == 0.0 => Ok(false),
            Type::Float64(_) => Ok(true),
            Type::Null => Ok(false),
            Type::Optional(inner) => inner.lowkey_cast(),
            _ => Err(format!("cannot cast to bool")),
        }
    }

    fn cast_up(self) -> Result<Type, String> {
        match self {
            Type::Bool(b) => Ok(Type::Int32(b as i32)),
            Type::Int32(x) => Ok(Type::Int64(x as i64)),
            Type::Int64(x) => Ok(Type::Int64(x)),
            Type::Float32(f) => Ok(Type::Float64(f as f64)),
            Type::Float64(f) => Ok(Type::Float64(f)),
            _ => Err(format!("cannot be upcasted")),
        }
    }

    fn cast_down(self) -> Result<Type, String> {
        match self {
            Type::Int32(x) => Ok(Type::Int32(x)),
            Type::Int64(x) => Ok(Type::Int32(x as i32)),
            Type::Float32(f) => Ok(Type::Float32(f)),
            Type::Float64(f) => Ok(Type::Float32(f as f32)),
            _ => Err(format!("cannot be downcasted")),
        }
    }
}

impl Add for Type {
    type Output = Result<Type, String>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Type::Int32(a), Type::Int32(b)) => Ok(Type::Int32(a + b)),
            (Type::Int64(a), Type::Int64(b)) => Ok(Type::Int64(a + b)),
            (Type::Int32(a), Type::Int64(b)) => Ok(Type::Int64(a as i64 + b)),
            (Type::Int64(a), Type::Int32(b)) => Ok(Type::Int64(a + b as i64)),
            (Type::Float32(a), Type::Float32(b)) => Ok(Type::Float32(a + b)),
            (Type::Float64(a), Type::Float64(b)) => Ok(Type::Float64(a + b)),
            (Type::Float32(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 + b)),
            (Type::Float64(a), Type::Float32(b)) => Ok(Type::Float64(a + b as f64)),
            (Type::Int32(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 + b)),
            (Type::Float64(a), Type::Int32(b)) => Ok(Type::Float64(a + b as f64)),
            (Type::Int64(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 + b)),
            (Type::Float64(a), Type::Int64(b)) => Ok(Type::Float64(a + b as f64)),
            (Type::Str(a), Type::Str(b)) => Ok(Type::Str(a + &b)),
            (Type::Str(a), Type::Int32(b)) => Ok(Type::Str(a + &b.to_string())),
            (Type::Str(a), Type::Int64(b)) => Ok(Type::Str(a + &b.to_string())),
            (Type::Str(a), Type::Float32(b)) => Ok(Type::Str(a + &b.to_string())),
            (Type::Str(a), Type::Float64(b)) => Ok(Type::Str(a + &b.to_string())),
            (Type::Str(a), Type::Bool(b)) => Ok(Type::Str(a + &b.to_string())),
            (Type::Bool(a), Type::Bool(b)) => Ok(Type::Bool(a | b)),
            (a, b) => Err(format!("Cannot add {:?} and {:?}", a, b)),
        }
    }
}

impl Sub for Type {
    type Output = Result<Type, String>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Type::Int32(a), Type::Int32(b)) => Ok(Type::Int32(a - b)),
            (Type::Int64(a), Type::Int64(b)) => Ok(Type::Int64(a - b)),
            (Type::Int32(a), Type::Int64(b)) => Ok(Type::Int64(a as i64 - b)),
            (Type::Int64(a), Type::Int32(b)) => Ok(Type::Int64(a - b as i64)),
            (Type::Float32(a), Type::Float32(b)) => Ok(Type::Float32(a - b)),
            (Type::Float64(a), Type::Float64(b)) => Ok(Type::Float64(a - b)),
            (Type::Float32(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 - b)),
            (Type::Float64(a), Type::Float32(b)) => Ok(Type::Float64(a - b as f64)),
            (Type::Int32(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 - b)),
            (Type::Float64(a), Type::Int32(b)) => Ok(Type::Float64(a - b as f64)),
            (Type::Int64(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 - b)),
            (Type::Float64(a), Type::Int64(b)) => Ok(Type::Float64(a - b as f64)),
            (a, b) => Err(format!("Cannot subtract {:?} and {:?}", a, b)),
        }
    }
}

impl Mul for Type {
    type Output = Result<Type, String>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Type::Int32(a), Type::Int32(b)) => Ok(Type::Int32(a * b)),
            (Type::Int64(a), Type::Int64(b)) => Ok(Type::Int64(a * b)),
            (Type::Int32(a), Type::Int64(b)) => Ok(Type::Int64(a as i64 * b)),
            (Type::Int64(a), Type::Int32(b)) => Ok(Type::Int64(a * b as i64)),
            (Type::Float32(a), Type::Float32(b)) => Ok(Type::Float32(a * b)),
            (Type::Float64(a), Type::Float64(b)) => Ok(Type::Float64(a * b)),
            (Type::Float32(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 * b)),
            (Type::Float64(a), Type::Float32(b)) => Ok(Type::Float64(a * b as f64)),
            (Type::Int32(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 * b)),
            (Type::Float64(a), Type::Int32(b)) => Ok(Type::Float64(a * b as f64)),
            (Type::Int64(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 * b)),
            (Type::Float64(a), Type::Int64(b)) => Ok(Type::Float64(a * b as f64)),
            (a, b) => Err(format!("Cannot multiply {:?} and {:?}", a, b)),
        }
    }
}

//floating point only
impl Div for Type {
    type Output = Result<Type, String>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Type::Int32(a), Type::Int32(b)) => Ok(Type::Float64(a as f64 / b as f64)),
            (Type::Int64(a), Type::Int64(b)) => Ok(Type::Float64(a as f64 / b as f64)),
            (Type::Int32(a), Type::Int64(b)) => Ok(Type::Float64(a as f64 / b as f64)),
            (Type::Int64(a), Type::Int32(b)) => Ok(Type::Float64(a as f64 / b as f64)),
            (Type::Float32(a), Type::Float32(b)) => Ok(Type::Float32(a / b)),
            (Type::Float64(a), Type::Float64(b)) => Ok(Type::Float64(a / b)),
            (Type::Float32(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 / b)),
            (Type::Float64(a), Type::Float32(b)) => Ok(Type::Float64(a / b as f64)),
            (Type::Int32(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 / b)),
            (Type::Float64(a), Type::Int32(b)) => Ok(Type::Float64(a / b as f64)),
            (Type::Int64(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 / b)),
            (Type::Float64(a), Type::Int64(b)) => Ok(Type::Float64(a / b as f64)),
            (a, b) => Err(format!("Cannot divide {:?} and {:?}", a, b)),
        }
    }
}

impl AddAssign for Type {
    fn add_assign(&mut self, rhs: Self) {
        match self {
            Type::Int32(a) => match rhs {
                Type::Int32(b) => *a += b,
                Type::Int64(b) => *self = Type::Int64(*a as i64 + b),
                Type::Float64(b) => *self = Type::Float64(*a as f64 + b),
                _ => panic!("Cannot += {:?} to Int32", rhs),
            },
            Type::Int64(a) => match rhs {
                Type::Int32(b) => *a += b as i64,
                Type::Int64(b) => *a += b,
                Type::Float64(b) => *self = Type::Float64(*a as f64 + b),
                _ => panic!("Cannot += {:?} to Int64", rhs),
            },
            Type::Float32(a) => match rhs {
                Type::Float32(b) => *a += b,
                Type::Float64(b) => *self = Type::Float64(*a as f64 + b),
                Type::Int32(b) => *self = Type::Float64(*a as f64 + b as f64),
                _ => panic!("Cannot += {:?} to Float32", rhs),
            },
            Type::Float64(a) => match rhs {
                Type::Float32(b) => *a += b as f64,
                Type::Float64(b) => *a += b,
                Type::Int32(b) => *a += b as f64,
                Type::Int64(b) => *a += b as f64,
                _ => panic!("Cannot += {:?} to Float64", rhs),
            },
            Type::Str(s) => match rhs {
                Type::Str(b) => s.push_str(&b),
                Type::Int32(b) => s.push_str(&b.to_string()),
                Type::Int64(b) => s.push_str(&b.to_string()),
                Type::Float32(b) => s.push_str(&b.to_string()),
                Type::Float64(b) => s.push_str(&b.to_string()),
                _ => panic!("Cannot += {:?} to Str", rhs),
            },
            _ => panic!("Cannot += on {:?}", self),
        }
    }
}

impl Type {
    pub fn get_kind(&self) -> TypeKind {
        match self {
            Type::Int32(_) => TypeKind::Int32,
            Type::Int64(_) => TypeKind::Int64,
            Type::Float32(_) => TypeKind::Float32,
            Type::Float64(_) => TypeKind::Float64,
            Type::Str(_) => TypeKind::Str,
            Type::Bool(_) => TypeKind::Bool,
            Type::VarRef(_) => TypeKind::VarRef,
            Type::Ref(_) => TypeKind::Ref,
            Type::Void => TypeKind::Void,
            Type::Object { .. } => TypeKind::Object,
            Type::Array(_) => TypeKind::Array,
            Type::Null => TypeKind::Null,
            Type::Break => TypeKind::Break,
            Type::Continue => TypeKind::Continue,
            Type::Optional(_) => TypeKind::Optional,
        }
    }

    pub fn increment(&mut self) -> Result<(), String> {
        match self {
            Type::Int32(a) => {
                *a += 1;
                Ok(())
            }
            Type::Int64(a) => {
                *a += 1;
                Ok(())
            }
            Type::Float32(a) => {
                *a += 1.0;
                Ok(())
            }
            Type::Float64(a) => {
                *a += 1.0;
                Ok(())
            }
            _ => Err(format!("Cannot increment {:?}", self._display())),
        }
    }

    pub fn decrement(&mut self) -> Result<(), String> {
        match self {
            Type::Int32(a) => {
                *a -= 1;
                Ok(())
            }
            Type::Int64(a) => {
                *a -= 1;
                Ok(())
            }
            Type::Float32(a) => {
                *a -= 1.0;
                Ok(())
            }
            Type::Float64(a) => {
                *a -= 1.0;
                Ok(())
            }
            _ => Err(format!("Cannot decrement {:?}", self._display())),
        }
    }

    pub fn push(&mut self, val: Type) -> Result<(), String> {
        match self {
            Type::Array(vec) => {
                vec.push(val);
                Ok(())
            }
            _ => Err(format!("cannot push to type {:?}", self._display())),
        }
    }

    pub fn copy(&self) -> Type {
        self.clone()
    }

    pub fn get_field(&self, field_name: &str) -> Result<Type, String> {
        match self {
            Type::Object { fields, .. } => fields
                .get(field_name)
                .cloned()
                .ok_or_else(|| format!("field '{}' not found", field_name)),
            _ => Err(format!(
                "cannot use field access on type {:?}",
                self._display()
            )),
        }
    }

    pub fn set_field(&mut self, field_name: &str, value: Type) -> Result<(), String> {
        match self {
            Type::Object { fields, .. } => {
                if fields.contains_key(field_name) {
                    fields.insert(field_name.to_string(), value);
                    Ok(())
                } else {
                    Err(format!("field '{}' not found", field_name))
                }
            }
            _ => Err(format!(
                "cannot use field assignment on type {:?}",
                self._display()
            )),
        }
    }

    pub fn set_index(&mut self, index: usize, value: Type) -> Result<(), String> {
        match self {
            Type::Array(vec) => {
                if index >= vec.len() {
                    Err(format!("index out of bounds: {} >= {}", index, vec.len()))
                } else {
                    vec[index] = value;
                    Ok(())
                }
            }
            _ => Err(format!(
                "cannot use index assignment on type {:?}",
                self._display()
            )),
        }
    }

    pub fn get_type_name(&self) -> Option<&str> {
        match self {
            Type::Object { type_name, .. } => Some(type_name),
            _ => None,
        }
    }
}

impl Type {
    fn _display(&self) -> String {
        match self {
            Type::Int32(i) => i.to_string(),
            Type::Int64(i) => i.to_string(),
            Type::Float32(f) => f.to_string(),
            Type::Float64(f) => f.to_string(),
            Type::Str(s) => s.clone(),
            Type::Bool(b) => b.to_string(),
            Type::Void => "<VOID>".to_string(),
            Type::VarRef(r) => format!("<VARREF>({})", r),
            Type::Ref(inner) => format!("&{}", inner._display()),
            Type::Object { type_name, .. } => format!("<OBJECT>({})", type_name),
            Type::Array(_) => "<ARRAY>".to_string(),
            Type::Null => "<NULL>".to_string(),
            Type::Break => "<BREAK>".to_string(),
            Type::Continue => "<CONTINUE>".to_string(),

            Type::Optional(inner) => format!("<OPTIONAL>({})", inner._display()),
        }
    }
}

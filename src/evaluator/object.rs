use crate::parser::Node;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Function {
        parameters: Vec<String>,
        body: Box<Node>,
    },
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(val) => write!(f, "{}", val),
            Object::Boolean(val) => write!(f, "{}", val),
            Object::Null => write!(f, "null"),
            Object::Function {
                parameters: params,
                body,
            } => write!(
                f,
                "fn({}) {}",
                params
                    .iter()
                    .map(|obj| obj.to_string())
                    .reduce(|acc, elem| acc + ", " + &elem)
                    .unwrap_or(String::new()),
                body,
            ),
        }
    }
}

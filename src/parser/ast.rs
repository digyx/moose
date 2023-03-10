use std::fmt::Display;

// Program
#[derive(Debug)]
pub struct Program(Vec<Node>);

impl Program {
    pub fn new(vec: Vec<Node>) -> Program {
        Program(vec)
    }

    pub fn program(self) -> Vec<Node> {
        self.0
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|stmt| stmt.to_string())
                .reduce(|acc, elem| acc + "\n" + &elem)
                .unwrap_or(String::new())
        )
    }
}

// Statements
#[derive(Debug, Clone)]
pub enum Node {
    Let(String, Expression),
    Return(Expression),
    Block(Vec<Node>),
    Expression(Expression),
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Let(name, val) => write!(f, "let {} be {}", name, val),
            Node::Return(val) => write!(f, "returning {}", val),
            Node::Block(vec) => write!(
                f,
                "{{ {} }}",
                vec.iter()
                    .map(|node| node.to_string())
                    .reduce(|acc, elem| acc + "\n" + &elem)
                    .unwrap_or(String::new())
            ),
            Node::Expression(val) => write!(f, "{}", val),
        }
    }
}

// Expressions
#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(String),

    // Literals
    Integer(i64),
    Bool(bool),

    // Prefix Operators
    Not(Box<Expression>),
    Negative(Box<Expression>),

    // Infix Operators
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),

    Equal(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),

    If {
        condition: Box<Expression>,
        consequence: Box<Node>,
        alternative: Option<Box<Node>>,
    },

    Function {
        parameters: Vec<String>,
        body: Box<Node>, // block statement
    },

    Call {
        function: Box<Expression>, // ident or function
        args: Vec<Expression>,
    },
}

impl Expression {
    pub fn is_bool(&self) -> bool {
        match *self {
            Expression::Identifier(_)
            | Expression::Not(_)
            | Expression::Bool(_)
            | Expression::Equal(_, _)
            | Expression::NotEqual(_, _)
            | Expression::LessThan(_, _)
            | Expression::GreaterThan(_, _) => true,
            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        match *self {
            Expression::Identifier(_)
            | Expression::Negative(_)
            | Expression::Integer(_)
            | Expression::Add(_, _)
            | Expression::Subtract(_, _)
            | Expression::Multiply(_, _)
            | Expression::Divide(_, _) => true,
            _ => false,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(val) => write!(f, "{}", val),
            Expression::Integer(val) => write!(f, "{}", val),
            Expression::Bool(val) => write!(f, "{}", val),

            Expression::Not(expr) => write!(f, "(!{})", expr),
            Expression::Negative(expr) => write!(f, "(-{})", expr),

            Expression::Add(rhs, lhs) => write!(f, "({} + {})", rhs, lhs),
            Expression::Subtract(rhs, lhs) => write!(f, "({} - {})", rhs, lhs),
            Expression::Multiply(rhs, lhs) => write!(f, "({} * {})", rhs, lhs),
            Expression::Divide(rhs, lhs) => write!(f, "({} / {})", rhs, lhs),

            Expression::Equal(rhs, lhs) => write!(f, "({} == {})", rhs, lhs),
            Expression::NotEqual(rhs, lhs) => write!(f, "({} != {})", rhs, lhs),
            Expression::LessThan(rhs, lhs) => write!(f, "({} < {})", rhs, lhs),
            Expression::GreaterThan(rhs, lhs) => write!(f, "({} > {})", rhs, lhs),

            Expression::If {
                condition,
                consequence,
                alternative,
            } => write!(
                f,
                "if {} then {} else {}",
                condition,
                consequence,
                match alternative {
                    Some(expr) => expr.to_string(),
                    None => "N/A".to_string(),
                }
            ),

            Expression::Function { parameters, body } => {
                write!(f, "fn({}) {}", parameters.join(", "), body)
            }

            Expression::Call { function, args } => write!(
                f,
                "{}({})",
                function,
                args.iter()
                    .map(|expr| expr.to_string())
                    .reduce(|acc, elem| acc + ", " + &elem)
                    .unwrap_or(String::new())
            ),
        }
    }
}

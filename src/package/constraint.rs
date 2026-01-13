use serde::de::{self, Deserialize, Deserializer};

use super::*;

#[derive(serde::Deserialize, Debug, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum Arch {
    X86,
    X86_64,
    Aarch64,
    Riscv32,
    Riscv64,
}

#[derive(serde::Deserialize, Debug, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum Os {
    Linux,
    Windows,
}

#[derive(serde::Deserialize, Debug, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum OsFamily {
    Unix,
}

#[derive(serde::Deserialize, Debug, PartialEq, Eq)]
pub enum Cap {
    SSE3,
    AVX1,
    AVX512,
}

#[derive(serde::Deserialize, Debug, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum CompilerOption {
    NoGc,
}

/*
    expr        := or_expr
    or_expr     := and_expr ( ',' and_expr )*
    and_expr    := unary ( '+' unary )*
    unary       := '!' unary | primary
    primary     := '(' expr ')' | atom
    atom        := IDENT ':' VALUE
*/

#[derive(Debug, PartialEq, Eq)]
pub enum Constraint {
    Arch(Arch),
    Os(Os),
    OsFamily(OsFamily),
    Cap(Cap),
    CompilerOption(CompilerOption),
    Not(Box<Constraint>),
    And(Box<Constraint>, Box<Constraint>),
    Or(Box<Constraint>, Box<Constraint>),
    None,
}

impl<'de> Deserialize<'de> for Constraint {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let mut parser = Parser::new(&s);
        let expr = parser.parse_expr().map_err(de::Error::custom)?;

        if parser.peek().is_some() {
            return Err(de::Error::custom("unexpected trailing input"));
        }

        Ok(expr)
    }
}

impl Constraint {
    #[allow(clippy::only_used_in_recursion)]
    pub fn applies(&self, arg: ()) -> bool {
        match self {
            Constraint::Not(inner) => !inner.applies(arg),
            Constraint::And(lhs, rhs) => lhs.applies(arg) && rhs.applies(arg),
            Constraint::Or(lhs, rhs) => lhs.applies(arg) || rhs.applies(arg),
            Constraint::None => true,
            // TODO: above constraints
            _ => true,
        }
    }

    pub fn parse(&self, s: &str) -> Result<Self, String> {
        parse(s)
    }
}

struct Parser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn next_if(&mut self, ch: char) -> bool {
        if self.peek() == Some(ch) {
            self.next();
            true
        } else {
            false
        }
    }

    fn next(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn expect(&mut self, expected: char) -> Result<(), String> {
        match self.next() {
            Some(c) if c == expected => Ok(()),
            _ => Err(format!("expected '{expected}'")),
        }
    }

    fn parse_expr(&mut self) -> Result<Constraint, String> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Constraint, String> {
        let mut lhs = self.parse_and()?;
        while self.next_if(',') {
            let rhs = self.parse_and()?;
            lhs = Constraint::Or(Box::new(lhs), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_and(&mut self) -> Result<Constraint, String> {
        let mut lhs = self.parse_unary()?;
        while self.next_if('+') {
            let rhs = self.parse_unary()?;
            lhs = Constraint::And(Box::new(lhs), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn parse_unary(&mut self) -> Result<Constraint, String> {
        if self.next_if('!') {
            Ok(Constraint::Not(Box::new(self.parse_unary()?)))
        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Result<Constraint, String> {
        match self.peek() {
            Some('(') => {
                self.next();
                let expr = self.parse_expr()?;
                self.expect(')')?;
                Ok(expr)
            }
            Some(_) => self.parse_atom(),
            None => Ok(Constraint::None),
        }
    }

    fn parse_atom(&mut self) -> Result<Constraint, String> {
        let key = self.parse_ident()?;
        self.expect(':')?;
        let value = self.parse_ident()?;
        match key {
            "arch" => Ok(Constraint::Arch(parse(value)?)),
            "os" => Ok(Constraint::Os(parse(value)?)),
            "os-family" => Ok(Constraint::OsFamily(parse(value)?)),
            "cap" => Ok(Constraint::Cap(parse(value)?)),
            "ctl" => Ok(Constraint::CompilerOption(parse(value)?)),
            _ => Err(format!("unknown constraint key '{key}'")),
        }
    }

    fn parse_ident(&mut self) -> Result<&'a str, String> {
        let start = self.pos;
        while let Some(c) = self.peek()
            && (c.is_alphanumeric() || c == '-')
        {
            self.next();
        }

        if self.pos == start {
            return Err("expected identifier".into());
        }

        Ok(&self.input[start..self.pos])
    }
}

fn parse<T>(s: &str) -> Result<T, String>
where
    T: for<'de> Deserialize<'de>,
{
    serde_plain::from_str(s).map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_constraint() -> Result<(), String> {
        assert_eq!(
            Constraint::And(
                Box::new(Constraint::And(
                    Box::new(Constraint::Arch(Arch::X86_64)),
                    Box::new(Constraint::Not(Box::new(Constraint::Os(Os::Windows)))),
                )),
                Box::new(Constraint::Or(
                    Box::new(Constraint::Cap(Cap::SSE3)),
                    Box::new(Constraint::Cap(Cap::AVX1))
                )),
            ),
            parse("arch:x86-64+!os:windows+(cap:SSE3,cap:AVX1)")?
        );

        Ok(())
    }

    #[test]
    fn precedence() -> Result<(), String> {
        assert_eq!(
            Constraint::Or(
                Box::new(Constraint::And(
                    Box::new(Constraint::Arch(Arch::X86_64)),
                    Box::new(Constraint::Not(Box::new(Constraint::Os(Os::Windows)))),
                )),
                Box::new(Constraint::Os(Os::Linux)),
            ),
            parse("arch:x86-64+!os:windows,os:linux")?
        );

        Ok(())
    }
}

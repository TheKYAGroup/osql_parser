use crate::token::{Token, ident_map};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    pos: usize,
    peek_pos: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut out = Self {
            input,
            pos: 0,
            peek_pos: 0,
            ch: '\0',
        };

        out.advance();

        out
    }

    fn advance(&mut self) {
        self.pos = self.peek_pos;
        self.peek_pos += 1;
        if self.pos < self.input.len() {
            self.ch = self
                .input
                .chars()
                .nth(self.pos)
                .expect("Failed to get char in range");
        } else {
            self.ch = '\0';
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let out = match self.ch {
            '(' => Token::LParen,
            ')' => Token::RParen,

            ',' => Token::Comma,
            '.' => Token::Period,

            '=' => Token::Eq,
            '*' => Token::Asterisk,
            '\0' => None?,
            '"' | '\'' => {
                let string = self.collect_string()?;
                self.advance();
                return Some(Token::String(string));
            }

            '/' if (self.peek_char_is('*')) => {
                return Some(Token::Comment(self.collect_comment()?));
            }

            _ if (Self::is_letter(self.ch)) => return Some(self.collect_ident()),

            _ => Token::Unkown(self.ch),
        };

        self.advance();

        Some(out)
    }

    fn peek_char_is(&self, ch: char) -> bool {
        self.input.chars().nth(self.peek_pos) == Some(ch)
    }

    fn collect_ident(&mut self) -> Token {
        let cur_pos = self.pos;
        while Self::is_letter(self.ch) {
            self.advance();
        }

        ident_map(self.input[cur_pos..self.pos].to_string())
    }

    fn collect_string(&mut self) -> Option<String> {
        let cur_pos = self.peek_pos;
        let str_char = self.ch;
        self.advance();
        while self.ch != str_char && self.ch != '\0' {
            self.advance();
        }

        if self.ch == '\0' {
            return None;
        }

        Some(self.input[cur_pos..self.pos].to_string())
    }

    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic()
    }

    fn collect_comment(&mut self) -> Option<String> {
        self.advance();
        self.advance();
        let start = self.pos;
        while !(self.ch == '*' && self.peek_char_is('/')) && self.ch != '\0' {
            self.advance();
        }
        if self.ch == '\0' {
            return None;
        }

        let out = self.input[start..self.pos].to_string();

        self.advance();
        self.advance();
        Some(out)
    }

    fn skip_whitespace(&mut self) {
        while self.is_whitespace() {
            self.advance();
        }
    }

    fn is_whitespace(&self) -> bool {
        return self.ch.is_whitespace();
    }
}

#[cfg(test)]
mod test {
    use crate::token::Token;

    use super::Lexer;

    #[test]
    fn whole_test() {
        let input = include_str!("test.sql");

        let tests = vec![
            Token::Comment(" TEST PM Picklist ".to_string()),
            Token::LParen,
            Token::Select,
            Token::ident("OrderEntry"),
            Token::Period,
            Token::ident("ProjID"),
            Token::ident("OrderEntryProjID"),
            Token::Comma,
            Token::ident("OrderEntry"),
            Token::Period,
            Token::ident("ItemID"),
            Token::ident("OrderEntryItemID"),
            Token::Comma,
            Token::ident("OrderEntry"),
            Token::Period,
            Token::ident("Memo"),
            Token::ident("OrderEntryMemo"),
            Token::Comma,
            Token::ident("OrderEntry"),
            Token::Period,
            Token::ident("Unit"),
            Token::ident("OrderEntryUnit"),
            Token::Comma,
            Token::ident("OrderEntry"),
            Token::Period,
            Token::ident("DocID"),
            Token::ident("OrderEntryDocID"),
            Token::Comma,
            Token::ident("OrderEntry"),
            Token::Period,
            Token::ident("DocNO"),
            Token::ident("OrderEntryDocNO"),
            Token::Comma,
            Token::ident("OrderEntry"),
            Token::Period,
            Token::ident("DocParID"),
            Token::ident("OrderEntryDocParID"),
            Token::Comma,
            Token::ident("PO"),
            Token::Period,
            Token::ident("ItemID"),
            Token::ident("POItemID"),
            Token::Comma,
            Token::ident("PO"),
            Token::Period,
            Token::ident("ItemDesc"),
            Token::ident("POItemDesc"),
            Token::Comma,
            Token::ident("PO"),
            Token::Period,
            Token::ident("SourceDocID"),
            Token::ident("POSourceDocID"),
            Token::Comma,
            Token::ident("PO"),
            Token::Period,
            Token::ident("Unit"),
            Token::ident("POUnit"),
            Token::Comma,
            Token::ident("PO"),
            Token::Period,
            Token::ident("DocID"),
            Token::ident("PODocID"),
            Token::Comma,
            Token::ident("PO"),
            Token::Period,
            Token::ident("QTY"),
            Token::ident("POQTY"),
            Token::Comma,
            Token::ident("PO"),
            Token::Period,
            Token::ident("Price"),
            Token::ident("POPrice"),
            Token::From,
            Token::LParen,
            Token::Select,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Transaction detail Attributes"),
            Token::Period,
            Token::string("ITEMDESC"),
            Token::ident("ItemDesc"),
            Token::Comma,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Transaction detail Attributes"),
            Token::Period,
            Token::string("ITEMID"),
            Token::ident("ItemID"),
            Token::Comma,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Transaction detail Attributes"),
            Token::Period,
            Token::string("MEMO"),
            Token::ident("Memo"),
            Token::Comma,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Transaction detail Attributes"),
            Token::Period,
            Token::string("PROJECTID"),
            Token::ident("ProjID"),
            Token::Comma,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Transaction detail Attributes"),
            Token::Period,
            Token::string("PROJECTNAME"),
            Token::ident("ProjName"),
            Token::Comma,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Transaction detail Attributes"),
            Token::Period,
            Token::string("UNIT"),
            Token::ident("Unit"),
            Token::Comma,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Transaction"),
            Token::Period,
            Token::string("DOCID"),
            Token::ident("DocID"),
            Token::Comma,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Transaction"),
            Token::Period,
            Token::string("DOCNO"),
            Token::ident("DocNO"),
            Token::Comma,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Transaction"),
            Token::Period,
            Token::string("DOCPARID"),
            Token::ident("DocParID"),
            Token::Comma,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Transaction"),
            Token::Period,
            Token::string("WHENCREATED"),
            Token::ident("WhenCreated"),
            Token::Comma,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Warehouse"),
            Token::Period,
            Token::string("WAREHOUSEID"),
            Token::ident("WhareHouseID"),
            Token::Comma,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Transaction detail Measures"),
            Token::Period,
            Token::string("PRICE_CONVERTED"),
            Token::ident("Price"),
            Token::Comma,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Transaction detail Measures"),
            Token::Period,
            Token::string("UIQTY"),
            Token::ident("QTY"),
            Token::From,
            Token::string("so:Order Entry"),
            Token::Where,
            Token::LParen,
            Token::string("so:Order Entry"),
            Token::Period,
            Token::string("Transaction"),
            Token::Period,
            Token::string("DOCPARID"),
            Token::In,
            Token::LParen,
            Token::string("Inventory Order Shipper"),
            Token::RParen,
            Token::RParen,
            Token::RParen,
            Token::ident("OrderEntry"),
            Token::Inner,
            Token::Join,
            Token::LParen,
            Token::Select,
            Token::string("po:Purchase Order"),
            Token::Period,
            Token::string("Transaction detail Attributes"),
            Token::Period,
            Token::string("ITEMDESC"),
            Token::ident("ItemDesc"),
            Token::Comma,
            Token::string("po:Purchase Order"),
            Token::Period,
            Token::string("Transaction detail Attributes"),
            Token::Period,
            Token::string("ITEMID"),
            Token::ident("ItemID"),
            Token::Comma,
            Token::string("po:Purchase Order"),
            Token::Period,
            Token::string("Transaction detail Attributes"),
            Token::Period,
            Token::string("PROJECTID"),
            Token::ident("ProjID"),
            Token::Comma,
            Token::string("po:Purchase Order"),
            Token::Period,
            Token::string("Transaction detail Attributes"),
            Token::Period,
            Token::string("SOURCEDOCID"),
            Token::ident("SourceDocID"),
            Token::Comma,
            Token::string("po:Purchase Order"),
            Token::Period,
            Token::string("Transaction detail Attributes"),
            Token::Period,
            Token::string("UNIT"),
            Token::ident("Unit"),
            Token::Comma,
            Token::string("po:Purchase Order"),
            Token::Period,
            Token::string("Transaction"),
            Token::Period,
            Token::string("DOCID"),
            Token::ident("DocID"),
            Token::Comma,
            Token::string("po:Purchase Order"),
            Token::Period,
            Token::string("Transaction detail Measures"),
            Token::Period,
            Token::string("UIPRICE"),
            Token::ident("Price"),
            Token::Comma,
            Token::string("po:Purchase Order"),
            Token::Period,
            Token::string("Transaction detail Measures"),
            Token::Period,
            Token::string("UIQTY"),
            Token::ident("QTY"),
            Token::From,
            Token::string("po:Purchase Order"),
            Token::RParen,
            Token::ident("PO"),
            Token::On,
            Token::ident("OrderEntry"),
            Token::Period,
            Token::ident("ProjID"),
            Token::Eq,
            Token::ident("PO"),
            Token::Period,
            Token::ident("ProjID"),
            Token::RParen,
            Token::ident("M"),
        ];

        let mut lexer = Lexer::new(input.to_string());

        for tt in tests {
            assert_eq!(tt, lexer.next_token().unwrap());
        }

        assert_eq!(None, lexer.next_token());
    }
}

use crate::token::{Loc, Token, TokenKind, ident_map};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    pos: usize,
    peek_pos: usize,
    ch: char,
    line: usize,
    col: usize,
}

struct CollectedStr {
    string: String,
    start: Loc,
    end: Loc,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut out = Self {
            input,
            pos: 0,
            peek_pos: 0,
            ch: '\0',
            line: 0,
            col: 0,
        };

        out.advance();

        out
    }

    fn advance(&mut self) {
        if self.ch == '\n' {
            self.col = 0;
            self.line += 1;
        } else {
            self.col += 1;
        }
        self.pos = self.peek_pos;
        self.peek_pos += 1;
        if self.pos == 0 {
            self.col = 0;
        }
        if self.pos < self.input.len() {
            self.ch = self
                .input
                .chars()
                .nth(self.pos)
                .expect("Failed to get char in range");
        } else {
            self.ch = '\0';
        }

        if self.line == 0 {
            assert_eq!(self.pos, self.col)
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        let start = self.cur_loc();
        let out = match self.ch {
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,

            ',' => TokenKind::Comma,
            '.' => TokenKind::Period,

            '=' => TokenKind::Eq,
            '*' => TokenKind::Asterisk,
            ';' => TokenKind::Semicolon,
            '\0' => None?,
            '"' | '\'' => {
                let string = self.collect_string()?;
                self.advance();
                return Some(Token {
                    kind: TokenKind::String(string.string),
                    start: string.start,
                    end: string.end,
                });
            }

            '/' if (self.peek_char_is('*')) => {
                let collected = self.collect_comment()?;
                return Some(Token {
                    kind: TokenKind::Comment(collected.string),
                    start: collected.start,
                    end: collected.end,
                });
            }

            _ if (Self::is_letter(self.ch)) => return Some(self.collect_ident()),
            _ if (Self::is_whitespace(self.ch)) => return Some(self.collect_whitespace()),

            _ => TokenKind::Unkown(self.ch),
        };

        self.advance();

        let end = self.cur_loc();

        Some(Token {
            kind: out,
            start,
            end,
        })
    }

    fn collect_while<F>(&mut self, cont: F) -> Option<CollectedStr>
    where
        F: Fn(char) -> bool,
    {
        let collected = self.collect_while_or_eof(cont);
        if self.ch == '\0' {
            None
        } else {
            Some(collected)
        }
    }

    fn collect_while_or_eof<F>(&mut self, cont: F) -> CollectedStr
    where
        F: Fn(char) -> bool,
    {
        let start = self.cur_loc();

        while cont(self.ch) && self.ch != '\0' {
            self.advance();
        }

        let end = self.cur_loc();

        let string = self.input[start.idx..end.idx].to_string();

        CollectedStr { string, start, end }
    }

    fn peek_char_is(&self, ch: char) -> bool {
        self.input.chars().nth(self.peek_pos) == Some(ch)
    }

    fn collect_ident(&mut self) -> Token {
        let collected = self.collect_while_or_eof(Self::is_letter);
        let kind = ident_map(collected.string);
        Token {
            kind,
            start: collected.start,
            end: collected.end,
        }
    }

    fn collect_string(&mut self) -> Option<CollectedStr> {
        let str_char = self.ch;
        self.advance();

        self.collect_while(|ch| ch != str_char)
    }

    fn cur_loc(&self) -> Loc {
        Loc {
            line: self.line,
            col: self.col,
            idx: self.pos,
        }
    }

    fn is_letter(ch: char) -> bool {
        ch.is_alphabetic()
    }

    fn collect_comment(&mut self) -> Option<CollectedStr> {
        let start = self.cur_loc();
        self.advance();
        self.advance();
        while !(self.ch == '*' && self.peek_char_is('/')) && self.ch != '\0' {
            self.advance();
        }
        if self.ch == '\0' {
            return None;
        }

        let out = self.input[start.idx + 2..self.pos].to_string();

        self.advance();
        self.advance();

        let end = self.cur_loc();
        Some(CollectedStr {
            string: out,
            start,
            end,
        })
    }

    #[allow(unused)]
    fn skip_whitespace(&mut self) {
        _ = self.collect_while_or_eof(Self::is_whitespace)
    }

    fn collect_whitespace(&mut self) -> Token {
        let collected = self.collect_while_or_eof(Self::is_whitespace);

        Token {
            kind: TokenKind::Whitespace(collected.string),
            start: collected.start,
            end: collected.end,
        }
    }

    fn is_whitespace(ch: char) -> bool {
        ch.is_whitespace()
    }

    pub fn recreate(&self, tokens: Vec<Token>) -> String {
        let mut out = self.input.clone();

        for tok in tokens.iter().rev() {
            let range = tok.start.idx..tok.end.idx;
            out.replace_range(range, &tok.kind.to_string());
        }

        out
    }
}

#[cfg(test)]
mod test {
    use crate::token::{Loc, Token, TokenKind};

    use super::Lexer;

    #[test]
    fn simple_select() {
        let input = r#"SELECT * FROM Test;"#;

        let tests = vec![
            Token {
                kind: TokenKind::Select,
                start: Loc {
                    line: 0,
                    col: 0,
                    idx: 0,
                },
                end: Loc {
                    line: 0,
                    col: 6,
                    idx: 6,
                },
            },
            Token {
                kind: TokenKind::Asterisk,
                start: Loc {
                    line: 0,
                    col: 7,
                    idx: 7,
                },
                end: Loc {
                    line: 0,
                    col: 8,
                    idx: 8,
                },
            },
            Token {
                kind: TokenKind::From,
                start: Loc {
                    line: 0,
                    col: 9,
                    idx: 9,
                },
                end: Loc {
                    line: 0,
                    col: 13,
                    idx: 13,
                },
            },
            Token {
                kind: TokenKind::ident("Test"),
                start: Loc {
                    line: 0,
                    col: 14,
                    idx: 14,
                },
                end: Loc {
                    line: 0,
                    col: 18,
                    idx: 18,
                },
            },
            Token {
                kind: TokenKind::Semicolon,
                start: Loc {
                    line: 0,
                    col: 18,
                    idx: 18,
                },
                end: Loc {
                    line: 0,
                    col: 19,
                    idx: 19,
                },
            },
        ];

        let mut lexer = Lexer::new(input.to_string());

        let mut toks = vec![];

        for tt in tests {
            let tok = lexer.next_token().unwrap();
            assert_eq!(tt, tok);
            toks.push(tok);
        }

        assert_eq!(None, lexer.next_token());

        assert_eq!(input, lexer.recreate(toks))
    }

    #[test]
    fn whole_test() {
        let input = include_str!("test.sql");

        let tests = vec![
            TokenKind::Comment(" TEST PM Picklist ".to_string()),
            TokenKind::LParen,
            TokenKind::Select,
            TokenKind::ident("OrderEntry"),
            TokenKind::Period,
            TokenKind::ident("ProjID"),
            TokenKind::ident("OrderEntryProjID"),
            TokenKind::Comma,
            TokenKind::ident("OrderEntry"),
            TokenKind::Period,
            TokenKind::ident("ItemID"),
            TokenKind::ident("OrderEntryItemID"),
            TokenKind::Comma,
            TokenKind::ident("OrderEntry"),
            TokenKind::Period,
            TokenKind::ident("Memo"),
            TokenKind::ident("OrderEntryMemo"),
            TokenKind::Comma,
            TokenKind::ident("OrderEntry"),
            TokenKind::Period,
            TokenKind::ident("Unit"),
            TokenKind::ident("OrderEntryUnit"),
            TokenKind::Comma,
            TokenKind::ident("OrderEntry"),
            TokenKind::Period,
            TokenKind::ident("DocID"),
            TokenKind::ident("OrderEntryDocID"),
            TokenKind::Comma,
            TokenKind::ident("OrderEntry"),
            TokenKind::Period,
            TokenKind::ident("DocNO"),
            TokenKind::ident("OrderEntryDocNO"),
            TokenKind::Comma,
            TokenKind::ident("OrderEntry"),
            TokenKind::Period,
            TokenKind::ident("DocParID"),
            TokenKind::ident("OrderEntryDocParID"),
            TokenKind::Comma,
            TokenKind::ident("PO"),
            TokenKind::Period,
            TokenKind::ident("ItemID"),
            TokenKind::ident("POItemID"),
            TokenKind::Comma,
            TokenKind::ident("PO"),
            TokenKind::Period,
            TokenKind::ident("ItemDesc"),
            TokenKind::ident("POItemDesc"),
            TokenKind::Comma,
            TokenKind::ident("PO"),
            TokenKind::Period,
            TokenKind::ident("SourceDocID"),
            TokenKind::ident("POSourceDocID"),
            TokenKind::Comma,
            TokenKind::ident("PO"),
            TokenKind::Period,
            TokenKind::ident("Unit"),
            TokenKind::ident("POUnit"),
            TokenKind::Comma,
            TokenKind::ident("PO"),
            TokenKind::Period,
            TokenKind::ident("DocID"),
            TokenKind::ident("PODocID"),
            TokenKind::Comma,
            TokenKind::ident("PO"),
            TokenKind::Period,
            TokenKind::ident("QTY"),
            TokenKind::ident("POQTY"),
            TokenKind::Comma,
            TokenKind::ident("PO"),
            TokenKind::Period,
            TokenKind::ident("Price"),
            TokenKind::ident("POPrice"),
            TokenKind::From,
            TokenKind::LParen,
            TokenKind::Select,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Attributes"),
            TokenKind::Period,
            TokenKind::string("ITEMDESC"),
            TokenKind::ident("ItemDesc"),
            TokenKind::Comma,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Attributes"),
            TokenKind::Period,
            TokenKind::string("ITEMID"),
            TokenKind::ident("ItemID"),
            TokenKind::Comma,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Attributes"),
            TokenKind::Period,
            TokenKind::string("MEMO"),
            TokenKind::ident("Memo"),
            TokenKind::Comma,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Attributes"),
            TokenKind::Period,
            TokenKind::string("PROJECTID"),
            TokenKind::ident("ProjID"),
            TokenKind::Comma,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Attributes"),
            TokenKind::Period,
            TokenKind::string("PROJECTNAME"),
            TokenKind::ident("ProjName"),
            TokenKind::Comma,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Attributes"),
            TokenKind::Period,
            TokenKind::string("UNIT"),
            TokenKind::ident("Unit"),
            TokenKind::Comma,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Transaction"),
            TokenKind::Period,
            TokenKind::string("DOCID"),
            TokenKind::ident("DocID"),
            TokenKind::Comma,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Transaction"),
            TokenKind::Period,
            TokenKind::string("DOCNO"),
            TokenKind::ident("DocNO"),
            TokenKind::Comma,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Transaction"),
            TokenKind::Period,
            TokenKind::string("DOCPARID"),
            TokenKind::ident("DocParID"),
            TokenKind::Comma,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Transaction"),
            TokenKind::Period,
            TokenKind::string("WHENCREATED"),
            TokenKind::ident("WhenCreated"),
            TokenKind::Comma,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Warehouse"),
            TokenKind::Period,
            TokenKind::string("WAREHOUSEID"),
            TokenKind::ident("WhareHouseID"),
            TokenKind::Comma,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Measures"),
            TokenKind::Period,
            TokenKind::string("PRICE_CONVERTED"),
            TokenKind::ident("Price"),
            TokenKind::Comma,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Measures"),
            TokenKind::Period,
            TokenKind::string("UIQTY"),
            TokenKind::ident("QTY"),
            TokenKind::From,
            TokenKind::string("so:Order Entry"),
            TokenKind::Where,
            TokenKind::LParen,
            TokenKind::string("so:Order Entry"),
            TokenKind::Period,
            TokenKind::string("Transaction"),
            TokenKind::Period,
            TokenKind::string("DOCPARID"),
            TokenKind::In,
            TokenKind::LParen,
            TokenKind::string("Inventory Order Shipper"),
            TokenKind::RParen,
            TokenKind::RParen,
            TokenKind::RParen,
            TokenKind::ident("OrderEntry"),
            TokenKind::Inner,
            TokenKind::Join,
            TokenKind::LParen,
            TokenKind::Select,
            TokenKind::string("po:Purchase Order"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Attributes"),
            TokenKind::Period,
            TokenKind::string("ITEMDESC"),
            TokenKind::ident("ItemDesc"),
            TokenKind::Comma,
            TokenKind::string("po:Purchase Order"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Attributes"),
            TokenKind::Period,
            TokenKind::string("ITEMID"),
            TokenKind::ident("ItemID"),
            TokenKind::Comma,
            TokenKind::string("po:Purchase Order"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Attributes"),
            TokenKind::Period,
            TokenKind::string("PROJECTID"),
            TokenKind::ident("ProjID"),
            TokenKind::Comma,
            TokenKind::string("po:Purchase Order"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Attributes"),
            TokenKind::Period,
            TokenKind::string("SOURCEDOCID"),
            TokenKind::ident("SourceDocID"),
            TokenKind::Comma,
            TokenKind::string("po:Purchase Order"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Attributes"),
            TokenKind::Period,
            TokenKind::string("UNIT"),
            TokenKind::ident("Unit"),
            TokenKind::Comma,
            TokenKind::string("po:Purchase Order"),
            TokenKind::Period,
            TokenKind::string("Transaction"),
            TokenKind::Period,
            TokenKind::string("DOCID"),
            TokenKind::ident("DocID"),
            TokenKind::Comma,
            TokenKind::string("po:Purchase Order"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Measures"),
            TokenKind::Period,
            TokenKind::string("UIPRICE"),
            TokenKind::ident("Price"),
            TokenKind::Comma,
            TokenKind::string("po:Purchase Order"),
            TokenKind::Period,
            TokenKind::string("Transaction detail Measures"),
            TokenKind::Period,
            TokenKind::string("UIQTY"),
            TokenKind::ident("QTY"),
            TokenKind::From,
            TokenKind::string("po:Purchase Order"),
            TokenKind::RParen,
            TokenKind::ident("PO"),
            TokenKind::On,
            TokenKind::ident("OrderEntry"),
            TokenKind::Period,
            TokenKind::ident("ProjID"),
            TokenKind::Eq,
            TokenKind::ident("PO"),
            TokenKind::Period,
            TokenKind::ident("ProjID"),
            TokenKind::RParen,
            TokenKind::ident("M"),
        ];

        let mut lexer = Lexer::new(input.to_string());

        let mut toks = vec![];

        for tt in tests {
            let tok = lexer.next_token().unwrap();
            match &tok.kind {
                TokenKind::Whitespace(_) => continue,
                kind => assert_eq!(&tt, kind),
            }
            toks.push(tok);
        }

        assert_eq!(None, lexer.next_token());

        assert_eq!(input, lexer.recreate(toks));
    }

    #[test]
    fn multi_line_select() {
        let input = r#"SELECT
*
    FROM
Test;"#;

        let tests = vec![
            Token {
                kind: TokenKind::Select,
                start: Loc {
                    line: 0,
                    col: 0,
                    idx: 0,
                },
                end: Loc {
                    line: 0,
                    col: 6,
                    idx: 6,
                },
            },
            Token {
                kind: TokenKind::Asterisk,
                start: Loc {
                    line: 1,
                    col: 0,
                    idx: 7,
                },
                end: Loc {
                    line: 1,
                    col: 1,
                    idx: 8,
                },
            },
            Token {
                kind: TokenKind::From,
                start: Loc {
                    line: 2,
                    col: 4,
                    idx: 13,
                },
                end: Loc {
                    line: 2,
                    col: 8,
                    idx: 17,
                },
            },
            Token {
                kind: TokenKind::ident("Test"),
                start: Loc {
                    line: 3,
                    col: 0,
                    idx: 18,
                },
                end: Loc {
                    line: 3,
                    col: 4,
                    idx: 22,
                },
            },
            Token {
                kind: TokenKind::Semicolon,
                start: Loc {
                    line: 3,
                    col: 4,
                    idx: 22,
                },
                end: Loc {
                    line: 3,
                    col: 5,
                    idx: 23,
                },
            },
        ];

        let mut lexer = Lexer::new(input.to_string());

        for tt in tests {
            assert_eq!(tt, lexer.next_token().unwrap());
        }

        assert_eq!(None, lexer.next_token());
    }
}

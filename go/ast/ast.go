package ast

import "monkey-lang/token"

type Node interface {
	TokenLiteral() string
}

type Statement interface {
	Node
	statementNode() // dummy method
}

type Expression interface {
	Node
	expressionNode() // dummy method
}

type Program struct {
	Statements []Statement
}

type Identifier struct {
	Token token.Token // the token.IDENT token
	Value string
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	} else {
		return ""
	}
}

type LetStatement struct {
	Value Expression
	Name  *Identifier
	Token token.Token
}

func (ls *LetStatement) statementNode() {}

func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }

func (i *Identifier) expressionNode() {}

func (i *Identifier) TokenLiteral() string { return i.Token.Literal }

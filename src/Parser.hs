--
-- Copyright (c)2007 Chris Pressey, Cat's Eye Technologies.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
--   1. Redistributions of source code must retain the above copyright
--      notices, this list of conditions and the following disclaimer.
--   2. Redistributions in binary form must reproduce the above copyright
--      notices, this list of conditions, and the following disclaimer in
--      the documentation and/or other materials provided with the
--      distribution.
--   3. Neither the names of the copyright holders nor the names of their
--      contributors may be used to endorse or promote products derived
--      from this software without specific prior written permission. 
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
-- FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
-- COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
-- BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--

-----------------------------------------------------------------------
-- ============================== Parser =========================== --
-----------------------------------------------------------------------

module Parser where

import Scanner
import Primitive
import AST

--
-- Utility
--

expect [] l = l
expect (x:xs) (y:ys)
    | x == y =
        expect xs ys

--
-- Statement ::= "if" BoolExpr "then" Statement "else" Statement
--             | "while" BoolExpr "do" Statement
--             | "begin" Statement {";" Statement} "end"
--             | "var" VarName "in" Statement
--             | "print" NumExpr
--             | VarName ":=" NumExpr
--

parseStatement (IfToken:tokens) =
    let
        (tokens2, be) = parseBoolExpr tokens
        tokens3 = expect [ThenToken] tokens2
        (tokens4, s1) = parseStatement tokens3
        tokens5 = expect [ElseToken] tokens4
        (tokens6, s2) = parseStatement tokens5
    in
        (tokens6, IfStmt be s1 s2)

parseStatement (VarToken:tokens) =
    let
        ((Ident ident):tokens2) = tokens
        v = VarName ident
        tokens3 = expect [InToken] tokens2
        (tokens4, s) = parseStatement tokens3
    in
        (tokens4, Var v s)

parseStatement (WhileToken:tokens) =
    let
        (tokens2, be) = parseBoolExpr tokens
        tokens3 = expect [DoToken] tokens2
        (tokens4, s) = parseStatement tokens3
    in
        (tokens4, While be s)

parseStatement (PrintToken:tokens) =
    let
        (tokens2, ne) = parseNumExpr tokens
    in
        (tokens2, Print ne)

parseStatement ((Ident s):tokens) =
    let
        v = VarName s
        tokens2 = expect [BecomesToken] tokens
        (tokens3, ne) = parseNumExpr tokens2
    in
        (tokens3, Assign v ne)

parseStatement (BeginToken:tokens) =
    let
        (tokens2, stmtList) = parseStmtList tokens []
    in
        (tokens2, Block (reverse stmtList))

parseStmtList tokens acc =
    let
        (tokens2, s) = parseStatement tokens
    in
        case tokens2 of
            (StmtSepToken:rest) ->
                parseStmtList rest (s : acc)
            (EndToken:rest) ->
                (rest, (s:acc))

--
-- NumExpr ::= AddExpr.
--

parseNumExpr tokens = parseAddExpr tokens

--
-- AddExpr ::= MulExpr {("+" | "-") MulExpr}.
--

parseAddExpr tokens =
    let
        (tokens2, lhs) = parseMulExpr tokens
    in
        parseAddExprTail tokens2 lhs

parseAddExprTail (AddToken:tokens) lhs =
    let
        (tokens2, rhs) = parseMulExpr tokens
        newLhs = NumOp Add lhs rhs
    in
        parseAddExprTail tokens2 newLhs

parseAddExprTail (SubtractToken:tokens) lhs =
    let
        (tokens2, rhs) = parseMulExpr tokens
        newLhs = NumOp Subtract lhs rhs
    in
        parseAddExprTail tokens2 newLhs

parseAddExprTail tokens e = (tokens, e)

--
-- MulExpr ::= Primitive {("*" | "/") Primitive}.
--

parseMulExpr tokens =
    let
        (tokens2, lhs) = parsePrimitive tokens
    in
        parseMulExprTail tokens2 lhs

parseMulExprTail (MultiplyToken:tokens) lhs =
    let
        (tokens2, rhs) = parsePrimitive tokens
        newLhs = NumOp Multiply lhs rhs
    in
        parseMulExprTail tokens2 newLhs

parseMulExprTail (DivideToken:tokens) lhs =
    let
        (tokens2, rhs) = parsePrimitive tokens
        newLhs = NumOp Divide lhs rhs
    in
        parseMulExprTail tokens2 newLhs

parseMulExprTail tokens e = (tokens, e)

--
-- Primitive ::= "(" NumExpr ")"
--             | "if" BoolExpr "then" NumExpr "else" NumExpr
--             | "let" VarName "=" NumExpr "in" NumExpr
--             | "valueof" VarName "in" Statement
--             | "loop" NumExpr
--             | "repeat"
--             | "input" VarName "in" NumExpr
--             | VarName
--             | NumConst.
--

parsePrimitive (OpenParenToken:tokens) =
    let
        (tokens2, ne) = parseNumExpr tokens
        tokens3 = expect [CloseParenToken] tokens2
    in
        (tokens3, ne)

parsePrimitive (IfToken:tokens) =
    let
        (tokens2, be) = parseBoolExpr tokens
        tokens3 = expect [ThenToken] tokens2
        (tokens4, e1) = parseNumExpr tokens3
        tokens5 = expect [ElseToken] tokens4
        (tokens6, e2) = parseNumExpr tokens5
    in
        (tokens6, IfExpr be e1 e2)

parsePrimitive (LetToken:tokens) =
    let
        ((Ident ident):tokens2) = tokens
        v = VarName ident
        tokens3 = expect [EqualToken] tokens2
        (tokens4, e1) = parseNumExpr tokens3
        tokens5 = expect [InToken] tokens4
        (tokens6, e2) = parseNumExpr tokens5
    in
        (tokens6, Let v e1 e2)

parsePrimitive (ValueOfToken:tokens) =
    let
        ((Ident ident):tokens2) = tokens
        v = VarName ident
        tokens3 = expect [InToken] tokens2
        (tokens4, s) = parseStatement tokens3
    in
        (tokens4, ValueOf v s)

parsePrimitive (LoopToken:tokens) =
    let
        (tokens2, e) = parseNumExpr tokens
    in
        (tokens2, Loop e)

parsePrimitive (RepeatToken:tokens) = (tokens, Repeat)

parsePrimitive (InputToken:tokens) =
    let
        ((Ident ident):tokens2) = tokens
        v = VarName ident
        tokens3 = expect [InToken] tokens2
        (tokens4, ne) = parseNumExpr tokens3
    in
        (tokens4, Input v ne)

parsePrimitive ((IntLit i):tokens) = (tokens, NumConst i)

parsePrimitive ((Ident s):tokens) = (tokens, (VarRef (VarName s)))

--
-- BoolExpr ::= RelExpr {("&" | "|") RelExpr}
--            | "not" BoolExpr
--            | "(" BoolExpr ")".
--

parseBoolExpr (NotToken:tokens) =
    let
        (tokens2, be) = parseBoolExpr tokens
    in
        (tokens2, Not be)

parseBoolExpr (OpenParenToken:tokens) =
    let
        (tokens2, be) = parseBoolExpr tokens
        tokens3 = expect [CloseParenToken] tokens2
    in
        (tokens3, be)

parseBoolExpr tokens =
    let
        (tokens2, lhs) = parseRelExpr tokens
    in
        parseBoolExprTail tokens2 lhs

parseBoolExprTail (AndToken:tokens) lhs =
    let
        (tokens2, rhs) = parseRelExpr tokens
        newLhs = BoolOp And lhs rhs
    in
        parseBoolExprTail tokens2 newLhs

parseBoolExprTail (OrToken:tokens) lhs =
    let
        (tokens2, rhs) = parseRelExpr tokens
        newLhs = BoolOp Or lhs rhs
    in
        parseBoolExprTail tokens2 newLhs

parseBoolExprTail tokens be = (tokens, be)

--
-- RelExpr ::= NumExpr (">" | "<" | ">=" | "<=" | "=" | "/=") NumExpr.
--

parseRelExpr tokens =
    let
        (tokens2, lhs) = parseNumExpr tokens
        (tokens3, relOp) = relOpForSym tokens2
        (tokens4, rhs) = parseNumExpr tokens3
    in
        (tokens4, RelOp relOp lhs rhs)

relOpForSym (GreaterThanToken:tokens)        = (tokens, GreaterThan)
relOpForSym (GreaterThanOrEqualToken:tokens) = (tokens, GreaterThanOrEqual)
relOpForSym (EqualToken:tokens)              = (tokens, Equal)
relOpForSym (NotEqualToken:tokens)           = (tokens, NotEqual)
relOpForSym (LessThanToken:tokens)           = (tokens, LessThan)
relOpForSym (LessThanOrEqualToken:tokens)    = (tokens, LessThanOrEqual)

--
-- Driver
--

parse string = parseStatement (tokenize string)

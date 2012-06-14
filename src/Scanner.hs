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
-- ============================= Scanner =========================== --
-----------------------------------------------------------------------

module Scanner where

import Char

data Token = Ident String
           | IntLit Integer
           | OpenCommentToken
           | CloseCommentToken
           | BecomesToken
           | GreaterThanToken
           | GreaterThanOrEqualToken
           | EqualToken
           | NotEqualToken
           | LessThanOrEqualToken
           | LessThanToken
           | StmtSepToken
           | AndToken
           | OrToken
           | NotToken
           | AddToken
           | SubtractToken
           | MultiplyToken
           | DivideToken
           | OpenParenToken
           | CloseParenToken
           | IfToken
           | ThenToken
           | ElseToken
           | WhileToken
           | DoToken
           | BeginToken
           | EndToken
           | InputToken
           | PrintToken
           | LetToken
           | InToken
           | VarToken
           | LoopToken
           | RepeatToken
           | ValueOfToken
           | TokenizerError
           deriving (Show, Read, Eq)

digitVal '0' = 0
digitVal '1' = 1
digitVal '2' = 2
digitVal '3' = 3
digitVal '4' = 4
digitVal '5' = 5
digitVal '6' = 6
digitVal '7' = 7
digitVal '8' = 8
digitVal '9' = 9

tokens = [("(*", OpenCommentToken),
          ("*)", CloseCommentToken),
          (":=", BecomesToken),
          (">=", GreaterThanOrEqualToken),
          ("<=", LessThanOrEqualToken),
          (">", GreaterThanToken),
          ("<", LessThanToken),
          ("=", EqualToken),
          ("/=", NotEqualToken),
          (";", StmtSepToken),
          ("&", AndToken),
          ("|", OrToken),
          ("!", NotToken),
          ("+", AddToken),
          ("-", SubtractToken),
          ("*", MultiplyToken),
          ("/", DivideToken),
          ("(", OpenParenToken),
          (")", CloseParenToken),
          ("if", IfToken),
          ("then", ThenToken),
          ("else", ElseToken),
          ("while", WhileToken),
          ("do", DoToken),
          ("begin", BeginToken),
          ("end", EndToken),
          ("input", InputToken),
          ("print", PrintToken),
          ("let", LetToken),
          ("in", InToken),
          ("var", VarToken),
          ("loop", LoopToken),
          ("repeat", RepeatToken),
          ("valueof", ValueOfToken)]

findToken string [] =
    (Nothing, string)
findToken string ((tokenString, token):rest)
    | (take len string) == tokenString =
        (Just token, (drop len string))
    | otherwise =
        findToken string rest
    where
        len = length tokenString

tokenize [] = []
tokenize string@(char:chars)
    | isSpace char =
        tokenize chars
    | isDigit char =
        tokenizeIntLit string 0
    | foundToken == Just OpenCommentToken =
        let
            newRestOfString = gobble CloseCommentToken restOfString
        in
            tokenize newRestOfString
    | foundToken /= Nothing =
        let
            (Just token) = foundToken
        in
            token:(tokenize restOfString)
    | isAlpha char =
        tokenizeIdent string ""
    | otherwise =
        [TokenizerError]
    where
        (foundToken, restOfString) = findToken string tokens

gobble token [] = []
gobble token string@(char:chars)
    | foundToken == Just token =
        restOfString
    | otherwise =
        gobble token chars
    where
        (foundToken, restOfString) = findToken string tokens

tokenizeIntLit [] num = [IntLit num]
tokenizeIntLit string@(char:chars) num
    | isDigit char =
        tokenizeIntLit chars (num * 10 + digitVal char)
    | otherwise =
        IntLit num:(tokenize string)

tokenizeIdent [] id = [Ident (reverse id)]
tokenizeIdent string@(char:chars) id
    | isAlpha char =
        tokenizeIdent chars (char:id)
    | otherwise =
        Ident (reverse id):(tokenize string)


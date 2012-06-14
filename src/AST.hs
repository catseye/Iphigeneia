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
-- ============================== AST ============================== --
-----------------------------------------------------------------------

module AST where

import Primitive

data VarName = VarName String
              deriving (Eq, Ord)

instance Show VarName where
    show (VarName s) = s

data BoolExpr = BoolOp BoolOp BoolExpr BoolExpr
              | RelOp RelOp NumExpr NumExpr
              | Not BoolExpr
              | BoolConst Bool
              deriving (Eq, Ord, Show)

data NumExpr = NumOp NumOp NumExpr NumExpr
             | NumConst Integer
             | IfExpr BoolExpr NumExpr NumExpr
             | VarRef VarName
             | ValueOf VarName Statement
             | Let VarName NumExpr NumExpr
             | Loop NumExpr
             | Repeat
             | Input VarName NumExpr
             deriving (Eq, Ord, Show)

data Statement = Block [Statement]
               | Var VarName Statement
               | Assign VarName NumExpr
               | IfStmt BoolExpr Statement Statement
               | While BoolExpr Statement
               | Print NumExpr
               deriving (Eq, Ord, Show)

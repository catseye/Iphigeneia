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
-- ===================== Primitive Operations ====================== --
-----------------------------------------------------------------------

module Primitive where

data NumOp = Add | Subtract | Multiply | Divide
             deriving (Eq, Ord, Show)

applyNumOp Add a b = a + b
applyNumOp Subtract a b = a - b
applyNumOp Multiply a b = a * b
applyNumOp Divide a b = a `div` b

data RelOp = GreaterThan | GreaterThanOrEqual
           | Equal | NotEqual | LessThan | LessThanOrEqual
             deriving (Eq, Ord, Show)

applyRelOp GreaterThan a b        = a > b
applyRelOp GreaterThanOrEqual a b = a >= b
applyRelOp Equal a b              = a == b
applyRelOp NotEqual a b           = a /= b
applyRelOp LessThan a b           = a < b
applyRelOp LessThanOrEqual a b    = a <= b

dualRelOp GreaterThan = LessThanOrEqual
dualRelOp GreaterThanOrEqual = LessThan
dualRelOp Equal = NotEqual
dualRelOp NotEqual = Equal
dualRelOp LessThan = GreaterThanOrEqual
dualRelOp LessThanOrEqual = GreaterThan

data BoolOp = And | Or
             deriving (Eq, Ord, Show)

applyBoolOp And a b = a && b
applyBoolOp Or a b = a || b

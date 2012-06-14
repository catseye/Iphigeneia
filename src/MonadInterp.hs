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
-- ======================= Monadic Interpreter ===================== --
-----------------------------------------------------------------------

--
-- This interpreter performs I/O.  It is not as straightforward as
-- PureInterp, as it must frame every function in terms of IO monads,
-- which tends to obscure the logic of the interpreter somewhat.
--

module MonadInterp where

import qualified Data.Char as Char

import Map
import AST
import Primitive

--
-- The eval* functions are passed a store and a continuation (cc).
--
-- The store maps VarName objects to their values (Integers).
--
-- The continuation is used with the loop and repeat constructs.
-- It is not a full-blown continuation in the sense of being a
-- function which represents the entire rest of the computation.
-- Rather, it represents only the matchings between occurrences
-- of loop and occurrences of repeat.
--
-- The continuation is implemented as list of NumExprs, where the
-- head NumExpr is the most recently encountered (innermost) loop
-- expression.  Each loop expression extends the continuation with
-- the expression being looped around, and a repeat expression
-- executes the continuation.
--

evalBool :: BoolExpr -> Map VarName Integer -> [NumExpr] -> IO Bool

evalBool (BoolOp op b1 b2) store cc = do
    val1 <- evalBool b1 store cc
    val2 <- evalBool b2 store cc
    return (applyBoolOp op val1 val2)

evalBool (RelOp op e1 e2) store cc  = do
    val1 <- evalNum e1 store cc
    val2 <- evalNum e2 store cc
    return (applyRelOp op val1 val2)

evalBool (Not b) store cc           = do
    val <- evalBool b store cc
    return (not val)

evalBool (BoolConst b) store cc     = do
    return b


evalNum :: NumExpr -> Map VarName Integer -> [NumExpr] -> IO Integer

evalNum (NumOp op e1 e2) store cc = do
    val1 <- evalNum e1 store cc
    val2 <- evalNum e2 store cc
    return (applyNumOp op val1 val2)

evalNum (NumConst i) store cc     = do
    return i

evalNum (IfExpr b e1 e2) store cc = do
    result <- evalBool b store cc
    evalNum (if result then e1 else e2) store cc

evalNum (VarRef v) store cc       = do
    return (get v store 0)

evalNum (Let v e1 e2) store cc    = do
    val <- evalNum e1 store cc
    evalNum e2 (set v val store) cc

evalNum (Loop e) store cc         = evalNum e store ((Loop e):cc)
evalNum (Repeat) store cc         = evalNum (head cc) store (tail cc)

evalNum (ValueOf v s) store cc    = do
    newStore <- interpret s store
    return (get v newStore 0)

evalNum (Input v e) store cc      = do
    symbol <- getChar
    evalNum e (set v (Prelude.fromIntegral (Char.ord symbol)) store) cc


interpret :: Statement -> Map VarName Integer -> IO (Map VarName Integer)

interpret (Block []) store = do
    return store
interpret (Block (s:rest)) store = do
    newStore <- interpret s store
    interpret (Block rest) newStore

interpret (Var v s) store = interpret s store

interpret (Assign v e) store = do
    val <- evalNum e store []
    return (set v val store)

interpret (IfStmt b s1 s2) store = do
    result <- evalBool b store []
    interpret (if result then s1 else s2) store

interpret (While b s) store = do
    result <- evalBool b store []
    loop result
  where
    loop True = do
          newStore <- interpret s store
          interpret (While b s) newStore
    loop False = do
          return store

interpret (Print e) store = do
    val <- evalNum e store []
    putChar (Char.chr (Prelude.fromIntegral val))
    return store

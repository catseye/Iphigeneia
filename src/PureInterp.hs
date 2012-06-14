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
-- ======================== Pure Interpreter ======================= --
-----------------------------------------------------------------------

--
-- This interpreter does not do any input or output.  Its purpose
-- is to present a very straightforward functional explication of
-- the language, uncluttered by monads.
--

module PureInterp where

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

evalBool (BoolOp op b1 b2) store cc = applyBoolOp op (evalBool b1 store cc) (evalBool b2 store cc)
evalBool (RelOp op e1 e2) store cc  = applyRelOp op (evalNum e1 store cc) (evalNum e2 store cc)
evalBool (Not b) store cc           = not (evalBool b store cc)
evalBool (BoolConst b) store cc     = b

evalNum (NumOp op e1 e2) store cc = applyNumOp op (evalNum e1 store cc) (evalNum e2 store cc)
evalNum (NumConst i) store cc     = i
evalNum (IfExpr b e1 e2) store cc
    | evalBool b store cc         = evalNum e1 store cc
    | otherwise                   = evalNum e2 store cc

evalNum (VarRef v) store cc       = get v store 0
evalNum (Let v e1 e2) store cc    = evalNum e2 (set v (evalNum e1 store cc) store) cc

evalNum (Loop e) store cc         = evalNum e store ((Loop e):cc)
evalNum (Repeat) store cc         = evalNum (head cc) store (tail cc)

evalNum (ValueOf v s) store cc    = get v (interpret s store) 0

evalNum (Input v e) store cc      = evalNum e (set v 0 store) cc

interpret (Block []) store = store
interpret (Block (s:rest)) store =
    interpret (Block rest) (interpret s store)

interpret (Var v s) store = interpret s store

interpret (Assign v e) store = set v (evalNum e store []) store

interpret (IfStmt b s1 s2) store
    | evalBool b store [] = interpret s1 store
    | otherwise           = interpret s2 store

interpret (While b s) store
    | evalBool b store [] = interpret (While b s) (interpret s store)
    | otherwise           = store

interpret (Print e) store = store

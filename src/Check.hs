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
-- ==================== Static Semantic Checker ==================== --
-----------------------------------------------------------------------

--
-- The static semantic checker returns a list of errors.
--

module Check where

import Map
import AST

data VarInfo = Undeclared
             | Updatable
             | SingleAssignment
             deriving (Eq, Show)

--
-- Helper functions
--

checkExists v env
    | (get v env Undeclared) == Undeclared =
        ["Variable " ++ (show v) ++ " not in scope"]
    | otherwise =
        []

checkAvailable v env
    | (get v env Undeclared) /= Undeclared =
        ["Variable " ++ (show v) ++ " already declared"]
    | otherwise =
        []

checkStore v env
    | (get v env Undeclared) == Undeclared =
        ["Variable " ++ (show v) ++ " not in scope"]
    | (get v env Undeclared) /= Updatable =
        ["Variable " ++ (show v) ++ " not updatable"]
    | otherwise =
        []

--
-- The checker proper
--

--
-- Currently we allow shadowing in let, valueof, and input, but not in var.
-- We could disallow it everywhere by adding:
--         declErrs = checkAvailable v env
-- in checkNumExpr (Let ...) and (ValueOf ...),
--

checkBoolExpr (BoolOp op b1 b2) env = (checkBoolExpr b1 env) ++ (checkBoolExpr b2 env)
checkBoolExpr (RelOp op e1 e2) env  = (checkNumExpr e1 env) ++ (checkNumExpr e2 env)
checkBoolExpr (Not b) env           = checkBoolExpr b env
checkBoolExpr (BoolConst b) env     = []

checkNumExpr (NumOp op e1 e2) env = (checkNumExpr e1 env) ++ (checkNumExpr e2 env)
checkNumExpr (NumConst i) env     = []
checkNumExpr (VarRef v) env       = checkExists v env
checkNumExpr (IfExpr b e1 e2) env = (checkBoolExpr b env) ++
                                    (checkNumExpr e1 env) ++ (checkNumExpr e2 env)
checkNumExpr (Let v e1 e2) env    =
    let
        exprErrs = checkNumExpr e1 env
        newEnv = set v SingleAssignment env
        bodyErrs = checkNumExpr e2 newEnv
    in
        exprErrs ++ bodyErrs

checkNumExpr (ValueOf v s) env    =
    let
        newEnv = set v Updatable env
        bodyErrs = checkStatement s newEnv
    in
        bodyErrs

checkNumExpr (Input v e) env    =
    let
        newEnv = set v SingleAssignment env
        bodyErrs = checkNumExpr e newEnv
    in
        bodyErrs

checkNumExpr (Loop e) env        = checkNumExpr e env
checkNumExpr (Repeat) env        = []

checkStatement (Block []) env =
    []
checkStatement (Block (s:rest)) env =
    (checkStatement s env) ++ (checkStatement (Block rest) env)

checkStatement (Var v s) env =
    let
        declErrs = checkAvailable v env
        newEnv = set v Updatable env
        stmtErrs = checkStatement s newEnv
    in
        declErrs ++ stmtErrs

checkStatement (Assign v e) env =
    (checkNumExpr e env) ++ (checkStore v env)

checkStatement (IfStmt b s1 s2) env =
    let
        exprErrs = checkBoolExpr b env
        s1Errs   = checkStatement s1 env
        s2Errs   = checkStatement s2 env
    in
        exprErrs ++ s1Errs ++ s2Errs

checkStatement (While b s) env =
    let
        exprErrs = checkBoolExpr b env
        bodyErrs = checkStatement s env
    in
        exprErrs ++ bodyErrs

checkStatement (Print e) env =
    checkNumExpr e env

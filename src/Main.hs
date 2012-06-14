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
-- ============================== Main ============================= --
-----------------------------------------------------------------------

import System

import Map
import AST
import qualified PureInterp
import qualified MonadInterp
import Parser
import Check

--
-- Utilities
--

--
-- Wrap the pure interpreter in a token monad (token in the sense of
-- inconsequential :) so that it has a type compatible with the monadic
-- interpreter.
--

pureInterpret prog map = do return (PureInterp.interpret prog map)

--
-- Parse and check the program.  If it's all OK, execute the given executor
-- function (continuation) on the resultant AST.  If it's not, execute the
-- given failure function (another continuation) on the resultant error list.
--

parseThen programText executor failureHandler =
    let
        (_, program) = parse programText
        errors = checkStatement program EmptyMap
    in
        case errors of
            [] ->
                executor program
            _ ->
                failureHandler errors

--
-- Useful functions to call from the Hugs interactive prompt.
--

run programText =
    runWith programText MonadInterp.interpret False

parseFile fileName = do
    programText <- readFile fileName
    (_, program) <- do return (parse programText)
    putStr (show program)

--
-- Program execution
--

runWith programText interpreter quiet =
    parseThen programText executor failureHandler
    where
        executor program = do
            result <- interpreter program EmptyMap
            putStr (if quiet then "" else (show result))
        failureHandler errors = do
            putStr ((show errors) ++ "\n")

runFileWith fileName interpreter quiet = do
    programText <- readFile fileName
    runWith programText interpreter quiet

--
-- Main entry point, so that we can build an executable using ghc.
-- When running the interpreter under hugs, it's not needed, as the
-- run function can be called directly from the interactive prompt.
--

main = do
    args <- getArgs
    (interpreter, quiet, fileName)
        <- processArgs args (MonadInterp.interpret) False ""
    case fileName of
        "" ->
            usage
        _ ->
            runFileWith fileName interpreter quiet

processArgs ("-p":rest) _ quiet fileName =
    processArgs rest (pureInterpret) quiet fileName

processArgs ("-q":rest) interpreter _ fileName =
    processArgs rest interpreter True fileName

processArgs (('-':unknownFlag):rest) interpreter quiet _ = do
    putStr ("Unknown command-line option: " ++ unknownFlag ++ "\n")
    return (interpreter, quiet, "")

processArgs (fileName:rest) interpreter quiet _ = do
    processArgs rest interpreter quiet fileName

processArgs [] interpreter quiet fileName = do
    return (interpreter, quiet, fileName)

usage = do
    putStr "iphi 2007.1125 - reference interpreter for Iphigeneia 1.0\n"
    putStr "(c)2007 Cat's Eye Technologies.  All rights reserved.\n\n"
    putStr "Usage:\n"
    putStr "    iphi [-p] [-q] filename\n"
    putStr "where\n"
    putStr "    -p: use pure interpreter (no IO)\n"
    putStr "    -q: don't dump final state of program to output\n"

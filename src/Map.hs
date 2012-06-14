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
-- ============================== Maps ============================= --
-----------------------------------------------------------------------

--
-- These can be used as environments, stores, etc.
--

module Map where

data Map k v = Binding k v (Map k v)
             | EmptyMap
             deriving (Eq, Ord)

get _ EmptyMap def = def
get key (Binding key' val map) def
    | key == key' = val
    | otherwise   = get key map def

set key val map = Binding key val (strip key map)

strip key EmptyMap = EmptyMap
strip key (Binding key' val map)
    | key == key' = strip key map
    | otherwise   = Binding key' val (strip key map)

--
-- Entries in second map override those in first map.
--
merge map EmptyMap = map
merge map (Binding key val rest) =
    merge (set key val map) rest

instance (Show k, Show v) => Show (Map k v) where
    show EmptyMap = ""
    show (Binding k v map) = (show k) ++ "=" ++ (show v) ++ "\n" ++ show map

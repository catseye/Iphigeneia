The Iphigeneia Programming Language
===================================

Language version 1.0, distribution version 2011.1010

Introduction
------------

The Iphigeneia programming language was designed as a workbench for an
exercise in transliterating between single-assignment (functional) and
mutable-variable (imperative) program forms. As such, the language
contains features paradigmatic to both forms.

As languages go, Iphigeneia is not particularly esoteric, nor is it
particularly practical; it's more academic, resembling those exciting
languages with inspired names like **Imp** and **Fun** that you're apt
to find in textbooks on formal semantics.

Note that this document only covers the Iphigeneia language itself, not
the transliteration process. This is because I still haven't fully
worked out the details of the transliteration, and that shortly after
designing the language, I changed my mind and decided that, for clarity,
it would probably be better to do the transliteration between two
*distinct* languages, rather than within a single language. So
Iphigeneia wanders a little bit from the original design goal, and
reflects a couple of design choices that are simply on whim rather than
strictly in support of the transliteration idea.

Note also that this document is an *informal* description of the
language that relies on the reader's intuition as a computer programmer.
I would like to write a formal semantics of Iphigeneia someday, since
it's a simple enough language that this isn't an unthinkably complex
task. In the meantime, you may wish to refer to the reference
implementation of the Iphigeneia interpreter for a more formal
definition (if you believe Haskell is sufficiently formally defined.)

The name Iphigeneia comes from the name of Agamemnon's daughter in Greek
mythology. The name was not chosen because of any particular
significance this figure holds — I just think it's a nice name. However,
I suppose if you wanted to force an interpretation, you could say that
Iphigeneia has two natures, princess and priestess, and so does her
namesake: imperative and functional.

Language
--------

The language constructs are generally straightforward to understand if
you've had any experience with the usual assortment of imperative and
functional languages, so forgive me if I'm a bit sketchy on the details
here and there, even to the point of just mentioning, rather than
describing, run-of-the-mill constructs like `while`.

The basic constructs of Iphigeneia are *expressions*, which evaluate to
a single value, and *commands*, which transform a store (a map between
variable names and values.) Expressions relate to the functional or
single-assignment side of things, and commands provide the imperative or
mutable-variable aspect of the language.

There are only two kinds of values in Iphigeneia: boolean values and
unbounded integer values. In addition, only integers can be "denoted"
(be stored in variables or have names bound to them); boolean
expressions can only appear in conditional tests. To keep things simple,
there are no subroutines, function values, pointers, references, arrays,
structures, or anything like that.

Constructs relating to the single-assignment side of things include
`let`, `loop`, `repeat`, and `valueof`. Imperative constructs include
`begin` blocks, `while` loops, and of course destructive variable update
with the `:=` operator. The lowly `if` makes sense in both "worlds", and
so leads a double life: one flavour appears in expressions and has
branches that are also expressions, and the other is a command and has
branches that are also commands.

Iphigeneia supports input and output. However, to further emphasize the
"split" in the language (and for no other good reason,) input is
considered "functional", leading to an `input` ... `in` form, while
output is considered "imperative", leading to a `print` command.

### Expressions

Expressions are formed from the usual assortment of infix operators with
their normative meaning and precedence. There are two kinds of
expressions, boolean expressions and integer expressions. Boolean
expressions only appear in tests (`if` and `while`). Integer expressions
appear everywhere else, and can also contain some more involved forms
which are explained in the remainder of this section.

Expressions are generally evaluated eagerly, left-to-right,
innermost-to-outermost. This only affects order of output with the
`print` command, however, since evaluation of an expression can never
side-effect a store. (Command sequences embedded in expressions always
work exclusively on their own, local store.)

#### `let` name `=` expr[0] `in` expr[1]

The `let` construct establishes a new binding. The expression expr[0] is
evaluated, and the result is associated with the given name during the
evaluation of expr[1]. That is, where-ever the name appears in expr[1]
or any sub-expression of expr[1], it is treated as if it had the value
of expr[0]. Note however that embedded commands (such as those appearing
in a `valueof`) are not considered to be sub-expressions, and the
influence of `let` bindings does not descend into them.

Let bindings shadow any enclosing let bindings with the same name.

#### `valueof` name `in` cmd

The `valueof` construct was a late addition, and is not strictly
necessary, although it adds a nice symmetry to the language. I decided
that, since there was already a (completely traditional) way to embed
expressions in commands (namely the `:=` assignment operator,) there
ought to be a complementary way to embed commands in expressions.

`valueof` blocks are evaluated in a completely new store; no other
stores or let bindings are visible within the block. There is no need to
declare the name with a `var` inside the block; the `valueof` counts as
a `var`, declaring the name in the new store.

#### `loop` ... `repeat`

The `loop` construct is modelled after Scheme's "named `let`" form. When
`repeat` executed, the innermost enclosing `loop` expression is
re-evaluated in the current environment. Since `loop` expressions do not
take arguments like a "named `let`", the values of bindings are instead
altered on subsequent iterations by enclosing the `repeat` in a `let`
expression, which gives new bindings to the names.

A `repeat` with an unmatched `loop` is a runtime error that aborts the
program. Also, the influence of a `loop` does not extend down through a
`valueof` expression. That is, the following `repeat` is not matched:
`loop valueof x in x := repeat`.

#### `input` name `in` expr

Works like `let`, except that the program waits for a character from the
standard input channel, and associates the ASCII value of this character
to the name when evaluating expr.

### Commands

#### `begin` ... `end`

Commands can be sequentially composed into a single compound command by
the `begin`...`end` construct.

#### `var` name `in` cmd

The `var` construct declares a new updatable variable. Variables must be
declared before they are used or assigned.

#### `print` expr

The `print` command evaluates expr and, if the result is between 0 and
255, produces a character with that ASCII value on the standard output
channel. The behaviour for other integers is not defined.

Grammar
-------

    Command ::= "if" BoolExpr "then" Command "else" Command
              | "while" BoolExpr "do" Command
              | "begin" Command {";" Command} "end"
              | "var" VarName "in" Command
              | "print" NumExpr
              | VarName ":=" NumExpr.

    BoolExpr ::= RelExpr {("&" | "|") RelExpr}
           | "!" BoolExpr
           | "(" BoolExpr ")".

    RelExpr ::= NumExpr (">" | "<" | ">=" | "<=" | "=" | "/=") NumExpr.
    NumExpr ::= MulExpr {("+" | "-") MulExpr}.
    MulExpr ::= Primitive {("*" | "/") Primitive}.

    Primitive ::= "(" NumExpr ")"
                | "if" BoolExpr "then" NumExpr "else" NumExpr
                | "let" VarName "=" NumExpr "in" NumExpr
            | "valueof" VarName "in" Command
            | "loop" NumExpr
            | "repeat"
                | "input" VarName "in" NumExpr
                | VarName
            | NumConst.

An Iphigeneia program, at the topmost level, is a command. (One idiom
for giving "functional" Iphigeneia programs is `var r in r := expr`, or
even just `print expr`.) Comments can be given anywhere in an Iphigeneia
program by enclosing them in `(*` and `*)`. Do not expect comments to
nest.

Implementation
--------------

There is a reference implementation of Iphigeneia written in Haskell 98.
It has been tested with ghc and Hugs, against a series of test cases
which are included with the distribution.

The reference implementation actually contains two interpreters. One is
a monadic interpreter, which supports the I/O facilities of Iphigeneia.
The other is a "pure" interpreter, which is written without the use of
monadic types; it does not support I/O, but its code may be easier to
follow. The pure interpreter always binds the name that occurs in a
`input` construct to zero, and it does not even evaluate the expressions
in `print` commands.

Compiling the reference implementation with ghc produces an executable
`iphi` which takes the following command-line options:

-   `-p` uses the pure interpreter instead of the default monadic
    interpreter.
-   `-q` suppresses the output of the final state of the program upon
    termination.

The reference interpreter is mostly written in a straightforward
(sometimes painfully straightforward) manner (except for, arguably,
`Main.hs`, which does some ugly things with continuations.) It provides
its own implementation of maps (environments) in `Map.hs`, instead of
using Haskell's `Data.Map`, to make the definition of the language more
explicit. The code is also released under a BSD-style license. So, even
though Iphigeneia is not a particularly exciting language, this
interpreter might serve as a good starting point for experimenting with
unusual features to add to an otherwise relatively vanilla imperative
and/or functional language.

-Chris Pressey  
November 25, 2007  
Chicago, Illinois

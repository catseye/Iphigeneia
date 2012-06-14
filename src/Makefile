# Makefile for iphi.
# $Id$

HC=ghc
# -O
HCFLAGS=
O=.o
PROG=iphi

OBJS=	AST${O} \
	Check${O} \
	PureInterp${O} \
	MonadInterp${O} \
	Map${O} \
	Main${O} \
	Parser${O} \
	Primitive${O} \
	Scanner${O}

all: ${PROG}

AST${O}: AST.hs Primitive${O}
	${HC} ${HCFLAGS} -c $*.hs

Check${O}: Check.hs Map${O} AST${O}
	${HC} ${HCFLAGS} -c $*.hs

Map${O}: Map.hs
	${HC} ${HCFLAGS} -c $*.hs

Main${O}: Main.hs Check${O} Parser${O} PureInterp${O}
	${HC} ${HCFLAGS} -c $*.hs

PureInterp${O}: PureInterp.hs Map${O} Primitive${O} AST${O}
	${HC} ${HCFLAGS} -c $*.hs

MonadInterp${O}: MonadInterp.hs Map${O} Primitive${O} AST${O}
	${HC} ${HCFLAGS} -c $*.hs

Scanner${O}: Scanner.hs
	${HC} ${HCFLAGS} -c $*.hs

Parser${O}: Parser.hs Scanner${O}
	${HC} ${HCFLAGS} -c $*.hs

Primitive${O}: Primitive.hs
	${HC} ${HCFLAGS} -c $*.hs


${PROG}: ${OBJS}
	${HC} -o ${PROG} -O ${OBJS}
	strip ${PROG}

clean:
	rm -rf *.o *.hi iphi

# Makefile for Iphigeneia regression test suite.
# $Id$

# This Makefile currently assumes GNU make.

# The suffixes are:
# .iphi  Iphigeneia source code
# .out	 Program run output  - used with 'diff' to check 'run'

IPHI?=../src/iphi
DIFF?=diff -u

TESTS=01.run 02.run 03.run 04.run 05.run 06.run 07.run 08.run 09.run 10.run \
      11.run 12.run 13.run 14.run

all: ${TESTS}

.PHONY: %.run

%.run: %.iphi %.out
	${IPHI} $< >OUTPUT
	${DIFF} OUTPUT $*.out

clean:
	rm -rf OUTPUT

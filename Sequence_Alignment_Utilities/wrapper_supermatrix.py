#!/usr/bin/env python


# Supermatrix wrapper
# Have list of alignments you want concatenated
# Feed them to supermatrix script
# Concatenate first two alignments take product and add to next alignment in list ...

import sys
from subprocess import call

InFile1 = sys.argv[1]
File1 = open(InFile1, 'rU')

AlignmentFiles = []
OutFile = "OutFile"

for Line in File1:
	Line = Line.strip("\n")
	AlignmentFiles.append(Line)
File1.close()

X = 1
call("supermatrix.py " + AlignmentFiles[0] + " " + AlignmentFiles[1] + " " + OutFile + str(X), shell=True)

AlignmentFiles = AlignmentFiles[2:]

for Alignment in AlignmentFiles:
	Y = X + 1
	call("supermatrix.py " + OutFile + str(X) + " " + Alignment + " " + OutFile + str(Y), shell=True)
	X = X + 1


	

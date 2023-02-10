#!/usr/bin/env python

import sys
import os
import textwrap

InFile1 = sys.argv[1]
InFile2 = sys.argv[2]
InFile3 = sys.argv[3]

OInFile1 = open(InFile1, 'rU')
OInFile2 = open(InFile2, 'rU')
OutFile1 = open(InFile3, 'w')

Concatdict1 = {}
TaxaList = []
for Line in OInFile1:
	Line = Line.strip("\n")
	if Line[0] == ">":
		TaxaList.append(Line)
		Concatdict1[Line] = []
	else:
		Concatdict1[TaxaList[len(TaxaList)-1]].append(Line)

Concatdict2 = {}
TaxaList2 = []
for Line in OInFile2:
	Line = Line.strip("\n")
	if Line[0] == ">":
		TaxaList2.append(Line)
		Concatdict2[Line] = []
	else:
		Concatdict2[TaxaList2[len(TaxaList2)-1]].append(Line)

MissingSeqs2 = frozenset(Concatdict1.keys()) - frozenset(Concatdict2.keys()) #Keys in 1 but not 2
MissingSeqs1 = frozenset(Concatdict2.keys()) - frozenset(Concatdict1.keys()) #Keys in 2 but not 1

print MissingSeqs1
print MissingSeqs2

SeqLengthFile1 = []

X = 1
for Element in Concatdict1:
	ValueStr = "".join(Concatdict1[Element])
	SeqLengthFile1.append(len(ValueStr))
	X = X + 1
	if X > 1:
		break
#print SeqLengthFile1

EmptySeq1 = "-" * SeqLengthFile1[0]


SeqLengthFile2 = []
X = 1
for Element in Concatdict2:
	ValueStr2 = "".join(Concatdict2[Element])
	SeqLengthFile2.append(len(ValueStr2))
	X = X + 1
	if X > 1:
		break
#print SeqLengthFile2

EmptySeq2 = "-" * SeqLengthFile2[0]


for Elements in MissingSeqs1:
	Concatdict1[Elements] = list(EmptySeq1)
for Elements in MissingSeqs2:
	Concatdict2[Elements] = list(EmptySeq2)
	
for Element in Concatdict2:
	SeqStr = "".join(Concatdict2[Element])
	Concatdict1[Element].append(SeqStr)

for Key in Concatdict1:
	print Key
	OutFile1.write(Key + "\n")
	SeqStr = "".join(Concatdict1[Key])
	OutFile1.write(SeqStr + "\n")
	#OutFile1.write(textwrap.fill(SeqStr, width=60) + "\n")
	print SeqStr
	#print len(SeqStr)

OInFile1.close()
OInFile2.close()	
OutFile1.close()

OutFile2 = "MissingSeqs.list_" + InFile3	
OpenOutFile2 = open(OutFile2, 'w')
OpenOutFile2.write("Missing sequences in " + InFile1 + " are: " + "\n")
for Element in MissingSeqs1:
	OpenOutFile2.write(Element + "\n")
OpenOutFile2.write("Missing sequences in " + InFile2 + " are: " + "\n")
for Element in MissingSeqs2:
	OpenOutFile2.write(Element + "\n")
OpenOutFile2.close()

OutFile3 = "Partition.list_" + InFile3
openOutFile3 = open(OutFile3, 'w')
openOutFile3.write(InFile1 + " = 1 - " + str(SeqLengthFile1[0]) + "\n")
first_value = SeqLengthFile1[0] + 1
second_value = SeqLengthFile1[0] + SeqLengthFile2[0]
openOutFile3.write(InFile2 + " = " + str(first_value) + " - " + str(second_value) + "\n")
openOutFile3.close()

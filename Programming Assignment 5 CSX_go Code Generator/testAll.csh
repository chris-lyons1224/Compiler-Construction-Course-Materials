#!/bin/csh

mkdir -p /tmp/testOut
/bin/rm -rf /tmp/testOut/*
set cs=/homes/raphael/courses/cs541/public
set testDir=/homes/raphael/courses/cs541/public/proj4/startup/tests
set testDir=../proj4/testInputs
set cup=$cs/cup
foreach index ( 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 \
		18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39\
		40 41 42 43 44 45 46) 
	set t=$testDir/test-$index.csx_go
	echo "working on $t"
	echo java -classpath ./classes:$cup/java-cup-10k.jar P5 $t
	java -classpath ./classes:$cup/java-cup-10k.jar P5 $t
	set o=/tmp/testOut/test-$index.csx_go.j
	mv p${index}csx.j $o
	echo "about to run jasmin $o"
	java -classpath $cup/cup.jar:$cup/jasmin-sable.jar jasmin.Main $o
	if (-e test.class) then
		if ($index == 11) then
			echo 10 | java -classpath .:./classes test
		else if ($index == 18) then
			echo '~5 ~13 20' | java -classpath .:./classes test
		else
			java -classpath .:./classes test
		endif
		/bin/rm -f test.class
	else
		echo "test $index failed"
	endif
end

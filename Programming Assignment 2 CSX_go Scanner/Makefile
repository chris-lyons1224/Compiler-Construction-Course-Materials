####
# Makefile for project 2
# Modified 1/2011 Raphael Finkel
#
#
# All classes are compiled into the ./classes directory
#
# The default is to make all .class files.
#
#    clean    -- removes all generated files
#
#    test     -- Tests proj2 by running P2.class with input tiny.csx_go
#
###

DESTPATH = ./classes
CUPPATH = /homes/raphael/courses/cs541/public/cup/java-cup-10k.jar
CLASSPATH = $(DESTPATH):$(CUPPATH)
VPATH = $(DESTPATH)
JCFLAGS = -Xlint:all,-auxiliaryclass,-rawtypes -deprecation -classpath $(CLASSPATH) -d $(DESTPATH)
JFLAGS = -classpath $(CLASSPATH)
JFLEX = ~/source/jflex-1.8.2/bin/jflex
JFLEX = jflex

%.class: %.java
	javac $(JCFLAGS) $<

all : tags P2.class sym.class Yylex.class 

P2.class : P2.java Yylex.class
sym.class: sym.java
Yylex.class: sym.class
Yylex.java: csx_go.jlex
	rm -f Yylex.java
	$(JFLEX) csx_go.jlex

###
# test
###

GOAL = testInput
test:   P2.class Yylex.class $(GOAL).csx_go
	java $(JFLAGS) P2 $(GOAL).csx_go

###
# style check
###

HOW = basic,braces,clone,design,finalizers,migrating,unusedcode,imports,optimizations,strictexception,strings,sunsecure,typeresolution

style: 
	t=`pwd`; cd ~raphael/source/pmd-4.2.5/bin; \
		./pmd.sh $$t text $(HOW) | sed -e '/Parameter.*final/d'

###
# lint check
###

lint: all
	jlint +all classes

###
# clean
###

clean: 
	rm -f $(DESTPATH)/*.class Yylex.java tags

tags: P2.java sym.java csx_go.jlex
	ctags --language-force=java P2.java sym.java csx_go.jlex


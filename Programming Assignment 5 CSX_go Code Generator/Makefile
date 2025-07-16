##########################################
####
# Makefile for project 3
# Modified 2/2011 Raphael Finkel
#
# All classes are compiled into the ./classes directory
#
# The default is to make all .class files.
#
#    clean    -- removes all generated files
#
#    test     -- Tests proj5 by running P5.class with inputs test[123].csx_go
#
###

DESTPATH = ./classes
COURSEDIR = /homes/raphael/courses/cs541/public
CUPPATH = $(COURSEDIR)/cup
CLASSPATH = $(DESTPATH):$(CUPPATH)/java-cup-10k.jar
JASMINPATH = $(CUPPATH)/cup.jar:$(CUPPATH)/jasmin-sable.jar
VPATH = $(DESTPATH)
JCFLAGS = -Xlint:all,-auxiliaryclass,-rawtypes -deprecation -classpath $(CLASSPATH) -d $(DESTPATH)
JFLAGS = -classpath $(CLASSPATH)
TESTDIR = $(COURSEDIR)/proj4/tests

%.class: %.java
	javac $(JCFLAGS) $<

.PHONY: all 
all: tags P5.class Yylex.class parser.class 

# don't use CUP 0.11a; I couldn't get it to work right.  -- Raphael 2/2011
sym.java parser.java: csx_go.cup 
	java $(JFLAGS) java_cup/Main < csx_go.cup
Yylex.java: csx_go.jlex
	jflex csx_go.jlex
parser.class: ASTNode.class Scanner.class 
Yylex.class: sym.class TypeEnum.class KindEnum.class
SymbolTable.class: Symb.class SymbolInfo.class EmptySTException.class \
	DuplicateException.class
Scanner.class: Yylex.class 
SymbolInfo.class: KindEnum.class TypeEnum.class
ASTNode.class: ast.java sym.class SymbolTable.class SymbolInfo.class
	javac $(JCFLAGS) ast.java
P5.class: Yylex.class Scanner.class parser.class ASTNode.class

GOAL=test-00
OUTFILE = $(subst test-,p,$(GOAL))csx.j
TESTDIR = ../proj4/testInputs
test: all CSXLib.class
	java $(JFLAGS) P5 $(TESTDIR)/$(GOAL).csx_go
	java -classpath $(JASMINPATH) jasmin.Main $(OUTFILE)
	java -classpath .:./classes test
	# /bin/rm -rf $(OUTFILE)
	@echo

testAll: all CSXLib.class
	./testAll.csh

tags: *java csx_go.cup csx_go.jlex
	ctags --language-force=java *.java csx_go.cup csx_go.jlex

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
	rm -f $(DESTPATH)/*.class Yylex.java* sym.java parser.java test.j *.class

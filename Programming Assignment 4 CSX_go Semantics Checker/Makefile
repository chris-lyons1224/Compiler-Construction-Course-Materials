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
#    test     -- Tests proj4 by running P4.class with inputs test[123].lite
#
###

DESTPATH = ./classes
COURSEDIR = /homes/raphael/courses/cs541/public
CUPPATH = $(COURSEDIR)/cup
CLASSPATH = $(DESTPATH):$(CUPPATH)/java-cup-10k.jar
VPATH = $(DESTPATH)
JCFLAGS = -Xlint:all,-auxiliaryclass,-rawtypes -deprecation -classpath $(CLASSPATH) -d $(DESTPATH)
JFLAGS = -classpath $(CLASSPATH)
TESTDIR = $(COURSEDIR)/proj4/tests

%.class: %.java
	javac $(JCFLAGS) $<

.PHONY: all 
all: tags P4.class Yylex.class parser.class 

# don't use CUP 0.11a; I couldn't get it to work right.  -- Raphael 2/2011
sym.java parser.java: csx_go.cup 
	java $(JFLAGS) java_cup/Main < csx_go.cup
Yylex.java: csx_go.jlex
	jflex csx_go.jlex
parser.class: ASTNode.class Scanner.class SyntaxErrorException.class
Yylex.class: sym.class TypeEnum.class KindEnum.class
SymbolTable.class: Symb.class SymbolInfo.class EmptySTException.class \
	DuplicateException.class
Scanner.class: Yylex.class 
SymbolInfo.class: KindEnum.class TypeEnum.class
ASTNode.class: ast.java sym.class SymbolTable.class SymbolInfo.class 
	javac $(JCFLAGS) ast.java
P4.class: Yylex.class Scanner.class parser.class ASTNode.class

GOAL = testInput.csx_go
test: all
	java $(JFLAGS) P4 $(GOAL)

TESTDIR = testInputs
testAll: all
	for file in $(TESTDIR)/* ; do java $(JFLAGS) P4 $$file ; done

SOURCE = ast.java DuplicateException.java EmptySTException.java KindEnum.java \
	P4.java Scanner.java Symb.java SymbolInfo.java SymbolTable.java \
	SyntaxErrorException.java TypeEnum.java
tags: $(SOURCE) csx_go.cup csx_go.jlex
	ctags --language-force=java $(SOURCE) csx_go.cup csx_go.jlex

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
	rm -f $(DESTPATH)/*.class Yylex.java* sym.java parser.java tags

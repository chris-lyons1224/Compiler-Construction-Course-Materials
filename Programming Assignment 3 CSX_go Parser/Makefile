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
#    test     -- Tests proj3 by running P3.class with input test.csx_go
#
###

DESTPATH = ./classes
CUPPATH = /homes/raphael/courses/cs541/public/cup
CLASSPATH = $(DESTPATH):$(CUPPATH)/java-cup-10k.jar
VPATH = $(DESTPATH)
JCFLAGS = -Xlint:all,-auxiliaryclass,-rawtypes -deprecation -classpath $(CLASSPATH) -d $(DESTPATH)
JFLAGS = -classpath $(CLASSPATH)

%.class: %.java
	javac $(JCFLAGS) $<

.PHONY: all 
all: tags P3.class Yylex.class parser.class 

# don't use CUP 0.11a; I couldn't get it to work right.  -- Raphael 2/2011
sym.java parser.java: csx_go.cup 
	java $(JFLAGS) java_cup/Main < csx_go.cup
Yylex.java: csx_go.jlex
	jflex csx_go.jlex
parser.class: ASTNode.class Scanner.class
Yylex.class: sym.class 
Scanner.class: Yylex.class
ASTNode.class: ast.java sym.class
	javac $(JCFLAGS) ast.java
P3.class: Yylex.class Scanner.class parser.class ASTNode.class

GOAL = testInput.csx_go
test: all $(GOAL)
	java $(JFLAGS) P3 $(GOAL)

tags: P3.java ast.java csx_go.cup csx_go.jlex
	ctags --language-force=java P3.java ast.java csx_go.cup csx_go.jlex

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
	rm -f $(DESTPATH)/*.class Yylex.java* sym.java parser.java

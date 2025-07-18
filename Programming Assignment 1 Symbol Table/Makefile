####
# Makefile for project 1
# Charles Fischer
# Modified 1/2011 Raphael Finkel
#
# All classes are compiled into the ./classes directory
#
# The default is to make all .class files.
#
#    clean    -- removes all generated files
#
#    test     -- Tests proj1 by running P1.class
#
###

CLASSPATH = ./classes
VPATH = $(CLASSPATH)
JCFLAGS = -Xlint:all -deprecation -classpath $(CLASSPATH) -d $(CLASSPATH)
JFLAGS = -classpath $(CLASSPATH)

%.class: %.java
	javac $(JCFLAGS) $<

all : tags P1.class EmptySTException.class SymbolTable.class \
		TestSym.class Symb.class DuplicateException.class

P1.class : EmptySTException.class SymbolTable.class \
		TestSym.class Symb.class DuplicateException.class
TestSym.class : Symb.class

SymbolTable.class : EmptySTException.class \
		Symb.class DuplicateException.class

tags: *java 
	ctags *.java 

###
# test
###

test: P1.class
	java $(JFLAGS) P1 < testInput

###
# style check
###

HOW = -R rulesets/java/quickstart.xml

style: 
	t=`pwd`; cd ~raphael/source/pmd-bin-6.20.0/bin; \
		./run.sh pmd -d $$t/classes -f text -cache $$t/cache $(HOW) 

###
# lint check
###

lint: all
	jlint +all classes

###
# clean
###

clean: 
	rm -f $(CLASSPATH)/*.class tags cache

CHECKSTYLEDIR = /tmp/checkstyle-5.7
newcheck:
	-java -classpath $(CHECKSTYLEDIR)/checkstyle-5.7-all.jar \
		com.puppycrawl.tools.checkstyle.Main \
		-c $(CHECKSTYLEDIR)/sun_checks.xml \
		*.java > /tmp/checked
	vi /tmp/checked

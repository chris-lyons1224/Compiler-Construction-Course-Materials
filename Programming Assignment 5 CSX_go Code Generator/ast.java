import java.util.List;
import java.util.ArrayList;
import java.io.PrintStream;

/* BOOL is represented by 0/1.
*/

class ASTNode {
// abstract superclass; only subclasses are actually created

	int lineNum; // sometimes set after the fact during checkSemantics().
	final int colNum;
	static int semanticsErrors = 0; // Total number of semantics errors found 
	final TypeEnum intOrChar[] = {TypeEnum.INTEGER, TypeEnum.CHAR};
	static PrintStream asmFile = null;
	static boolean doingVars = true; // false once we start function
	static int maxDepth; // starts at 0 for each function
	static int curDepth; // starts at 0 for each function

	void genIndent(int indent) {
		final String separator = "    "; // could use tab 
		if (lineNum == -1) {
			show("?");
		} else {
			show(lineNum);
		}
		show(":\t");
		for (int index = 0; index < indent; index += 1) {
			show(separator);
		}
	} // genIndent

	ASTNode() {
		lineNum = -1;
		colNum = -1;
	} // constructor ASTNode()

	ASTNode(CSXToken token) {
		lineNum = token.linenum;
		colNum = token.colnum;
	} // ASTNode

	void setPosition(ASTNode otherNode) {
		this.lineNum = otherNode.lineNum;
		// no attempt to set colnum
	}
	static void show(String message) { // convenience
		System.out.print(message);
	} // show

	void show(String message, int indent) { // convenience
		genIndent(indent);
		System.out.print(message);
	} // show

	static void show(char message) { // convenience
		System.out.print(message);
	} // show

	static void show(boolean message) { // convenience
		System.out.print(message);
	} // show

	static void show(int message) { // convenience
		System.out.print(message);
	} // show

	void unparse(int indent) {
		// This routine is normally redefined in a subclass
	} // unparse()

	void error(String... messages) {
		semanticsErrors += 1;
		final StringBuilder buffer = new StringBuilder().
			append("Error (line ").
			append(lineNum).
			append("): ");
		for (String message : messages) {
			buffer.append(message);
		}
		buffer.append(".\n");
		show(buffer.toString());
	} // error(String[]);

	void checkSemantics() {
		// define in subclasses
	} // checkSemantics

	boolean isSemanticsCorrect() {
		checkSemantics();
		return (semanticsErrors == 0);
	} // isSemanticsCorrect

	public static SymbolTable symbolTable = new SymbolTable();

	void closeScopeNoError() {
		try {
			symbolTable.closeScope();
		} catch (EmptySTException e) {
			error("Internal error: closing non-existent scope");
		}
	} // closeScopeNoError

	void insertNoEmptyError(SymbolInfo symbolInfo) throws DuplicateException {
		try {
			symbolTable.insert(symbolInfo);
		} catch (EmptySTException e) {
			error("Internal error: inserting in non-existent scope");
		}
	} // insertNoEmptyError

	boolean arrayLengthCompatible(int targetLength, int sourceLength) {
		return (targetLength == 0 || sourceLength == targetLength);
	} // arrayLengthCompatible(int, int)

	boolean assignCompatible(TypeEnum targetType, int targetArrayLength,
			ExprNode source) {
		boolean OK = false;
		if (source.type == TypeEnum.ERROR) {
			OK = true; // avoid cascading errors
		} else {
			switch (targetType) {
				case INTEGER: OK = source.type == TypeEnum.INTEGER; break;
				case CHAR: OK = source.type == TypeEnum.CHAR; break;
				case BOOL: OK = source.type == TypeEnum.BOOL; break;
				case INTEGERARRAY:
					OK = source.type == TypeEnum.INTEGERARRAY && 
						arrayLengthCompatible(targetArrayLength,
							source.arrayLength); 
					break;
				case CHARARRAY:
					OK = (source.type == TypeEnum.CHARARRAY && 
						arrayLengthCompatible(targetArrayLength,
							source.arrayLength)) ||
						(source.type == TypeEnum.STRING &&
						arrayLengthCompatible(targetArrayLength,
							source.arrayLength));
					break;
				case BOOLARRAY:
					OK = source.type == TypeEnum.BOOLARRAY && 
						arrayLengthCompatible(targetArrayLength,
							source.arrayLength); 
					break;
				case FUNCTION:
				case STRING:
				case VOID:
					OK = false; break;
				case ERROR: OK = true; break; // avoid cascading errors
				default: error("Internal error: target type not covered");
			} // switch(targetType)
		} // source type is not ERROR
		return(OK);
	} // assignCompatible

	static String lastCode = null; // last code emitted, to avoid ending with label
	// generate an instruction w/ 0 operands
	static void genCode(String... opcodes){
		final StringBuilder toPrint = new StringBuilder();
		if (opcodes.length > 0 && opcodes[0].charAt(0) != '.') {
			toPrint.append('\t'); // indent ordinary instructions
		}
		for (String opcode : opcodes) {
			toPrint.append(opcode).append(' ');
		}
		lastCode = toPrint.toString();
		asmFile.println(lastCode);
	} // genCode(String...)

	// generate a line number
	void genLine() {
		genCode(".line", Integer.toString(lineNum));
		// can output curDepth for debugging here, too.
	}

	// puts a constant on the stack
	void genConstant(int value) {
		if (0 <= value && value <= 5) {
			genCode("iconst_" + value);
		} else if (value == -1) { // unlikely
			genCode("iconst_m1");
		} else if (Math.abs(value) < 128) {
			genCode("bipush", Integer.toString(value));
		} else if (Math.abs(value) < 32768) {
			genCode("sipush", Integer.toString(value));
		} else {
			genCode("ldc", Integer.toString(value));
		}
		adjustStackDepth(1);
	} // genConstant

	// generates either (i|a)(load|store)(_n| n), as appropriate
	void genLoadStore(int register, char type, String loadStore,
			String... comments) {
		final StringBuffer result =
			new StringBuffer().append(type).append(loadStore);
		if (register <= 3) { // short form
			result.append('_');
		} else { // long form
			result.append(' ');
		}
		result.append(register);
		for (String comment : comments) {
			result.append(" ; ").append(comment);
		}
		genCode(result.toString());
		adjustStackDepth("load".equals(loadStore) ? 1 : -1);
	} // genLoadStore

	// keep track of curDepth and maxDepth
	void adjustStackDepth(int amount) {
		curDepth += amount;
		if (curDepth > maxDepth) {
			maxDepth = curDepth;
		}
	} // adjustStackDepth

	void assignTo(NameNode destination) {
		final int register = destination.symbolInfo.register;
		if (register == -1) { // static
			switch (destination.symbolInfo.type) {
				case INTEGER:
				case BOOL:
				case CHAR:
					genCode("putstatic",
						"test/" + destination.identifier.normalText,
						destination.symbolInfo.type.toJasminType());
					adjustStackDepth(-1);
					break;
				case INTEGERARRAY:
					if (destination.subscript != null) { // array cell
						genCode("iastore", ";", "assign to global array cell");
						adjustStackDepth(-3);
					} else {
						genCode("invokestatic", "CSXLib/cloneIntArray([I)[I");
						genCode("putstatic",
							"test/" + destination.identifier.normalText,
							destination.symbolInfo.type.toJasminType());
						adjustStackDepth(-1);
					}
					break;
				case BOOLARRAY:
					if (destination.subscript != null) { // array cell
						genCode("bastore", ";", "assign to global array cell");
						adjustStackDepth(-3);
					} else {
						genCode("invokestatic", "CSXLib/cloneBoolArray([Z)[Z");
						genCode("putstatic",
							"test/" + destination.identifier.normalText,
							destination.symbolInfo.type.toJasminType());
						adjustStackDepth(-1);
					}
					break;
				case CHARARRAY:
					if (destination.subscript != null) { // array cell
						genCode("castore", ";", "assign to global array cell");
						adjustStackDepth(-3);
					} else {
						genCode("invokestatic", "CSXLib/cloneCharArray([C)[C");
						genCode("putstatic",
							"test/" + destination.identifier.normalText,
							destination.symbolInfo.type.toJasminType());
						adjustStackDepth(-1);
					}
					break;
				default:
					error("Cannot handle assignment to ", 
						destination.symbolInfo.type.toString());
					genCode("pop", ";", "assign to ",
						destination.identifier.normalText);
					break;
			} // switch
		} else { // dynamic
			switch (destination.symbolInfo.type) {
				case INTEGER:
				case BOOL:
				case CHAR:
					genLoadStore(register, 'i', "store",
						destination.identifier.normalText);
					break;
				case INTEGERARRAY:
					if (destination.subscript != null) { // array cell
						genCode("iastore", ";", "assign to local array cell");
						adjustStackDepth(-3);
					} else {
						genCode("invokestatic", "CSXLib/cloneIntArray([I)[I");
						genLoadStore(register, 'a', "store",
							destination.identifier.normalText);
					}
					break;
				case BOOLARRAY:
					if (destination.subscript != null) { // array cell
						genCode("bastore", ";", "assign to local array cell");
						adjustStackDepth(-3);
					} else {
						genCode("invokestatic", "CSXLib/cloneBoolArray([Z)[Z");
						genLoadStore(register, 'a', "store",
							destination.identifier.normalText);
					}
					break;
				case CHARARRAY:
					if (destination.subscript != null) { // array cell
						genCode("castore", ";", "assign to local array cell");
						adjustStackDepth(-3);
					} else {
						genCode("invokestatic", "CSXLib/cloneCharArray([C)[C");
						genLoadStore(register, 'a', "store",
							destination.identifier.normalText);
					}
					break;
				default:
					error("Cannot handle assignment to ", 
						destination.symbolInfo.type.toString());
					genCode("pop", ";", "assign to ",
						destination.identifier.normalText);
					break;
			} //  switch (type)
		} // dynamic
	} // assignTo(NameNode)

	static int labelNumber = 0;
	String genLabel() {
		labelNumber += 1;
		return("label" + labelNumber);
	} // genLabel
		
	/*
	Translate the AST rooted by this node into JVM code, which is written to
	asmFile.  Return whether errors occur.
	*/
	boolean codeGen(PrintStream asmfile) {
		error("Internal error: parent codeGen invoked");
		return(false);
	} // codeGen

    // Translate AST node into JVM code.
	// The code is written to asmFile (set by codeGen).
	void codeGen(){
		// Normally overridden in subclasses
	}; // codeGen

} // class ASTNode

class ProgramNode extends ASTNode {
	
	final CSXIdentifierToken name;
	final VarDeclsNode varList;
	final FuncDeclsNode funcList;

	ProgramNode(CSXIdentifierToken name, VarDeclsNode varList,
			FuncDeclsNode funcList) {
		super(name);
		this.name = name;
		this.varList = varList;
		this.funcList = funcList;
	} // constructor ProgramNode()

	void unparse(int indent) {
		show(new StringBuilder().
				append("package ").
				append(name.identifierText).
				append("\n").toString(),
			indent);
		varList.unparse(indent);
		funcList.unparse(indent);
		show("// package ", indent);
		show(name.identifierText);
		show("\n");
	} // unparse

	void checkSemantics() {
		symbolTable.openScope();
		varList.checkSemantics();
		funcList.checkSemantics();

		final FuncDeclNode lastFUNCTION =
			funcList.funcList.get(funcList.funcList.size()-1);
		if (!"main".equals(lastFUNCTION.identifier.normalText) ||
				lastFUNCTION.returnType != null ||
				!lastFUNCTION.formals.formals.isEmpty()) {
			error("Last function must be main()");
		}
		closeScopeNoError();
	} // checkSemantics

	boolean codeGen(PrintStream asmFile) {
		ASTNode.asmFile = asmFile;
		codeGen();
		return (semanticsErrors == 0);
	} // boolean codeGen(PrintStream)

	void codeGen() {
		genCode(".class", "public", "test");
		genCode(".super", "java/lang/Object");
		doingVars = true;
		varList.codeGen();
		genCode(".method", "public static", "main([Ljava/lang/String;)V");
		varList.emitArrayInit();
		genCode("invokestatic", "test/main()V");
		genCode("return");
		genCode(".limit", "stack", "2");
		genCode(".end", "method", "; main");
		genCode(); // blank line separator
		doingVars = false;
		funcList.codeGen();
	} // codeGen()

} // ProgramNode

class FuncDeclsNode extends ASTNode {
	
	List<FuncDeclNode> funcList;

	FuncDeclsNode() {
		funcList = new ArrayList<FuncDeclNode>();
	} // constructor FuncDeclsNode()

	FuncDeclsNode addFunc(FuncDeclNode funDecl) {
		funcList.add(0, funDecl); // put at front
		return this;
	} // addVar(VarDeclNode)

	void unparse(int indent) {
		for (FuncDeclNode funcDecl : funcList) {
			funcDecl.unparse(indent);
		}
	} // unparse

	void checkSemantics() {
		for (FuncDeclNode funcDecl : funcList) {
			funcDecl.checkSemantics();
		}
	} // checkSemantics

	void codeGen() {
		for (FuncDeclNode funcDecl : funcList) {
			funcDecl.codeGen();
		}
	} // codeGen

} // FuncDeclsNode

class VarDeclsNode extends ASTNode {

	final List<VarDeclNode> varList;

	VarDeclsNode() {
		varList = new ArrayList<VarDeclNode>();
	} // constructor VarDeclsNode()

	VarDeclsNode addVar(VarDeclNode vardecl) {
		varList.add(0, vardecl); // put at front
		return this;
	} // addVar(VarDeclNode)

	void unparse(int indent) {
		for (VarDeclNode vardecl : varList) {
			vardecl.unparse(indent);
		}
	} // unparse

	void checkSemantics() {
		for (VarDeclNode var : varList) {
			var.checkSemantics();
		}
	} // checkSemantics

	void codeGen() {
		codeGen(0); // we don't care about numVars at this point
	}

	int codeGen(int numVars) {
		for (VarDeclNode var : varList) {
			numVars = var.codeGen(numVars);
		}
		return numVars;
	} // codeGen

	void emitArrayInit() {
		for (VarDeclNode var : varList) {
			var.emitArrayInit();
		}
	} // emitArrayInit	

} // VarDeclsNode

class FormalParamsNode extends ASTNode {
	
	final List<FormalParamNode> formals;
	final List<TypeEnum> formalTypes = new ArrayList<TypeEnum>();

	FormalParamsNode() {
		formals = new ArrayList<FormalParamNode>();
	} // constructor FormalParamsNode()

	FormalParamsNode(FormalParamNode formal) {
		formals = new ArrayList<FormalParamNode>();
		formals.add(0, formal);
	} // constructor FormalParamsNode()

	FormalParamsNode addFormal(FormalParamNode formal) {
		formals.add(0, formal);
		return this;
	} // addParam

	void unparse(int indent) {
		show('(');
		boolean first = true;
		for (FormalParamNode formal : formals) {
			if (!first) {
				show(", ");
			}
			formal.unparse(indent);
			first = false;
		}
		show(')');
	} // unparse

	void checkSemantics() {
		for (FormalParamNode formal : formals) {
			formal.setPosition(this);
			formal.checkSemantics();
			formalTypes.add(formal.type.type);
		}
	} // checkSemantics

	String typeString = null;

	int codeGen(int numVars) {
		final StringBuilder typePieces = new StringBuilder().append('(');
		for (FormalParamNode formal : formals) {
			typePieces.append(formal.type.type.toJasminType());
			formal.paramInfo.register = numVars;
			numVars += 1;
		}
		typeString = typePieces.append(')').toString();
		return numVars;
	} // codeGen

} // FormalParamsNode

class FormalParamNode extends ASTNode {
	
	final TypeNode type;
	final CSXIdentifierToken identifier;
	final boolean isArray;

	FormalParamNode(TypeNode type, CSXIdentifierToken identifier) {
		super(identifier);
		this.type = type;
		this.identifier = identifier;
		this.isArray = false;
	} // constructor FormalParamNode

	FormalParamNode(TypeNode type, CSXIdentifierToken identifier, 
			boolean isArray) {
		this.type = type;
		this.identifier = identifier;
		this.isArray = isArray;
	} // constructor FormalParamNode

	void unparse(int indent) {
		show(identifier.identifierText);
		show(' ');
		if (isArray) {
			show("[]");
		}
		type.unparse(indent);
	} // unparse

	SymbolInfo paramInfo = null;

	void checkSemantics() {
		type.checkSemantics();
		if (isArray) {
			switch (type.type) {
				case INTEGER: type.type = TypeEnum.INTEGERARRAY; break;
				case CHAR: type.type = TypeEnum.CHARARRAY; break;
				case BOOL: type.type = TypeEnum.BOOLARRAY; break;
				default: error("Internal error: strange array");
			} // switch (type.type)
		} // array
		paramInfo = new SymbolInfo(
			identifier.normalText, KindEnum.VARIABLE, type.type);
		try {
			insertNoEmptyError(paramInfo);
		} catch (DuplicateException e) {
			error("Duplicate formal parameter ", identifier.identifierText);
		} 
	} // checkSemantics

	void codeGen() {
		error("Internal error: FormalParamNode.codeGen() called");
	}

} // FormalParamNode

class VarDeclNode extends ASTNode {

	final TypeNode typeNode;
	final CSXIdentifierToken identifier;
	final CSXIntLitToken arrayLength;
	final ExprNode initialValue;
	final boolean isConst;

	VarDeclNode(TypeNode typeNode, CSXIdentifierToken identifier) {
		this(typeNode, identifier, null, null, false);
	} // constructor VarDeclNode(TypeNode, CSXIdentifierToken)

	VarDeclNode(TypeNode typeNode, CSXIdentifierToken identifier,
			CSXIntLitToken arrayLength) {
		this(typeNode, identifier, arrayLength, null, false);
	} // constructor VarDeclNode(TypeNode, CSXIdentifierToken, CSXIntLitToken)

	VarDeclNode(TypeNode typeNode, CSXIdentifierToken identifier,
			ExprNode initialValue) {
		this(typeNode, identifier, null, initialValue, false);
	} // constructor VarDeclNode(TypeNode, CSXIdentifierToken, ExprNode)

	VarDeclNode(CSXIdentifierToken identifier, ExprNode initialValue) {
		this(null, identifier, null, initialValue, true);
	} // constructor VarDeclNode(CSXIdentifierToken, ExprNode)

	VarDeclNode(TypeNode typeNode, CSXIdentifierToken identifier,
			CSXIntLitToken arrayLength, ExprNode initialValue,
			boolean isConst) {
		super(identifier);
		this.typeNode = typeNode;
		this.identifier = identifier;
		this.arrayLength = arrayLength;
		this.initialValue = initialValue;
		this.isConst = isConst;
	} // constructor VarDeclNode(TypeNode, CSXIdentifierToken, CSXIntLitToken)

	void unparse(int indent) {
		genIndent(indent);
		if (isConst) {
			show("const ");
		} else {
			show("var ");
		}
		show(identifier.identifierText);
		if (typeNode != null) {
			show(' ');
			typeNode.unparse(0);
		}
		if (arrayLength != null) { // declared as an array
			show(new StringBuilder().
				append('[').
				append(arrayLength.intValue).
				append(']').
				toString()
			);
		} else if (initialValue != null) { // not array, but has initialization
			show(" = ");
			initialValue.unparse(indent, true);
		}
		show(";\n");
	} // unparse

	SymbolInfo symbolInfo = null;

	void checkSemantics() {
		TypeEnum type; // default
		KindEnum kind; // default
		if (typeNode != null) {
			typeNode.checkSemantics();
			type = typeNode.type;
			if (initialValue != null) {
				initialValue.checkSemantics();
				kind = KindEnum.VARIABLE;
				if (!assignCompatible(type,
						(arrayLength == null ? 0 : arrayLength.intValue),
						initialValue)
				) {
					error(initialValue.type + " cannot be assigned to ", type.toString());
				}
			} else {
				kind = symbolTable.topLevel() ? KindEnum.VARIABLE : KindEnum.UNINITVAR;
				// top-level variables are assumed assigned for static analysis
			}
		} else if (initialValue != null) {
			initialValue.setPosition(this);
			initialValue.checkSemantics();
			type = initialValue.type;
			if (isConst) {
				kind = KindEnum.CONSTANT;
			} else {
				kind = KindEnum.VARIABLE;
			}
		} else {
			error("Internal error: neither type nor initial value");
			type = TypeEnum.ERROR;
			kind = KindEnum.VARIABLE;
		}
		if (arrayLength != null) { // declared as an array
			if (arrayLength.intValue <= 0) { // syntax disallows negative
				error("Array must have positive length, not ",
					Integer.toString(arrayLength.intValue));
			}
			switch (type) {
				case INTEGER: type = TypeEnum.INTEGERARRAY; break;
				case BOOL: type = TypeEnum.BOOLARRAY; break;
				case CHAR: type = TypeEnum.CHARARRAY; break;
				default: error("Internal error: array of ", type.toString());
			} // switch (type)
		}
		symbolInfo = arrayLength == null
			? new SymbolInfo(identifier.normalText, kind, type)
			: new SymbolInfo(identifier.normalText, type,
				arrayLength.intValue);
		try {
			insertNoEmptyError(symbolInfo);
		} catch (DuplicateException e) {
			error("Duplicate identifier ", identifier.identifierText);
		} 
	} // checkSemantics

	void emitArrayInit() { // this gets delayed until main.
		if (symbolInfo.arrayLength > 0) { // allocate the array
			genConstant(symbolInfo.arrayLength);
			switch (symbolInfo.type) {
				case INTEGERARRAY:
					genCode("newarray", "int");
					genCode("putstatic", "test/" + identifier.normalText, "[I");
					adjustStackDepth(-1);
					break;
				case CHARARRAY:
					genCode("newarray", "char");
					genCode("putstatic", "test/" + identifier.normalText, "[C");
					adjustStackDepth(-1);
					break;
				case BOOLARRAY:
					genCode("newarray", "boolean");
					genCode("putstatic", "test/" + identifier.normalText, "[Z");
					adjustStackDepth(-1);
					break;
				default:
					error("Internal error: array initialization; not an array");
			} // switch
		} // allocate array
	} // emitArrayInit

	int codeGen(int numVars) {
		if (doingVars) { // must make static
			symbolInfo.register = -1;
		} else { // not doing vars; must make dynamic
			symbolInfo.register = numVars;
			numVars += 1;
			if (symbolInfo.arrayLength > 0) { // allocate the array
				genConstant(symbolInfo.arrayLength);
				switch (symbolInfo.type) {
					case INTEGERARRAY:
						genCode("newarray", "int"); break;
					case BOOLARRAY:
						genCode("newarray", "boolean"); break;
					case CHARARRAY:
						genCode("newarray", "char"); break;
					default:
						error("not an array in array initialization");
				} // switch type
				genLoadStore(symbolInfo.register, 'a', "store",
					identifier.normalText);
			}
		} // dynamic declaration
		if (initialValue != null) {
			if (symbolInfo.register >= 0) {
				initialValue.codeGen();
				genLoadStore(symbolInfo.register, 'i', "store",
					identifier.normalText);
			} else {
				int initConstant = 0;
				switch (symbolInfo.type) {
					case INTEGER:
						initConstant = ((IntLitNode) initialValue).value.intValue;
						break;
					case BOOL:
						initConstant = ((BoolLitNode) initialValue).value ? 1 : 0;
						break;
					case CHAR:
						initConstant = ((CharLitNode) initialValue).value.effectiveChar;
						break;
					default:
						error("Internal error: initial value of type ",
							initialValue.type.toString());
				} // switch (initialValue.type);
				genCode(".field",  "static", identifier.normalText,
					symbolInfo.type.toJasminType(),
					"=", Integer.toString(initConstant));
			}
		} else { // no initialization
			if (symbolInfo.register == -1) {
				genCode(".field",  "static", identifier.normalText,
					symbolInfo.type.toJasminType());
			}
		}
		return numVars;
	} // codeGen

} // VarDeclNode

class StatementsNode extends ASTNode {
	
	final List<StatementNode> statements;

	StatementsNode() {
		statements = new ArrayList<StatementNode>();
	} // constructor StatementsNode()

	void addStatement(StatementNode statement) {
		statements.add(0, statement);
	} // addStatement

	void unparse(int indent) {
		for (StatementNode statement : statements) {
			statement.unparse(indent);
		} // each statement
	} // unparse

	void checkSemantics() {
		for (StatementNode statement : statements) {
			statement.checkSemantics();
		} // each statement
	} // checkSemantics

	int codeGen(int numVars) {
		for (StatementNode statement : statements) {
			numVars = statement.codeGen(numVars);
		}
		return numVars;
	} // codeGen
} // StatementsNode

abstract class StatementNode extends ASTNode {
	
	StatementNode(CSXToken token) {
		super(token);
	} // constructor StatementNode(CSXToken)

	abstract int codeGen(int numVars);
	// numVars is how many variables since top of func; returns updated count

} // StatementNode

class IfNode extends StatementNode {
	
	final ExprNode condition;
	final BlockNode thenPart;
	final BlockNode elsePart;

	IfNode(CSXToken token, ExprNode condition, BlockNode thenPart,
			BlockNode elsePart) {
		super(token);
		this.condition = condition;
		this.thenPart = thenPart;
		this.elsePart = elsePart;
		} // constructor IfNode()

	void unparse(int indent) {
		show("if ", indent);
		condition.unparse(indent, true);
		show(" {\n");
		thenPart.unparse(indent+1, true);
		show("}", indent);
		if (elsePart != null) {
			// "else" is reported with line/col of "if".
			show(" else {\n");
			elsePart.unparse(indent+1, true);
			show("}", indent);
		}
		show("\n");
	} // unparse

	void checkSemantics() {
		condition.setPosition(this);
		condition.checkSemantics();
		if (condition.type != TypeEnum.BOOL) {
			error("Condition for \"if\" must be bool, not ",
				condition.type.toString());
		}
		thenPart.checkSemantics(true);
		if (elsePart != null) {
			elsePart.checkSemantics(true);
		}
	} // checkSemantics

	int codeGen(int numVars) {
		genLine();
		String elseLabel, endLabel;
		endLabel = genLabel();
		condition.codeGen();
		if (elsePart != null) {
			elseLabel = genLabel();
			genCode("ifeq", elseLabel);
			adjustStackDepth(-1);
			numVars = thenPart.codeGen(numVars);
				// just add on new variables instead of overlapping variables
				// due to else and then parts
			genCode("goto", endLabel);
			genCode(elseLabel, ":");
			numVars = elsePart.codeGen(numVars);
			genCode(endLabel, ":");
		} else { // no else part
			genCode("ifeq", endLabel);
			adjustStackDepth(-1);
			numVars = thenPart.codeGen(numVars);
			genCode(endLabel, ":");
		}
		return numVars;
	} // codeGen

} // IfNode

class ForNode extends StatementNode {
	
	final CSXIdentifierToken label;
	final ExprNode condition;
	final BlockNode body;

	ForNode(CSXToken token, CSXIdentifierToken label, ExprNode condition,
			BlockNode body) {
		super(token);
		this.label = label;
		this.condition = condition;
		this.body = body;
	} // constructor ForNode()

	void unparse(int indent) {
		genIndent(indent);
		if (label != null) {
			show(label.identifierText);
			show(": ");
		}
		show("for ");
		condition.unparse(indent, true);
		show(" {\n");
		body.unparse(indent+1, false);
		show("} // " + (label == null ? "for" : label.identifierText) + "\n",
			indent);
	} // unparse

	SymbolInfo labelInfo = null;

	void checkSemantics() {
		if (label != null) {
			try {
				insertNoEmptyError(new SymbolInfo(
					label.normalText, KindEnum.LABEL, TypeEnum.VOID));
			} catch (DuplicateException e) {
				error("Loop label ", label.identifierText, " already in use");
			}
			symbolTable.openScope();
			labelInfo = new SymbolInfo(
				label.normalText, KindEnum.CURRENTLABEL, TypeEnum.VOID);
			try {
				insertNoEmptyError(labelInfo);
			} catch (DuplicateException e) {
				error("Internal error: duplicate active label");
			}
		} // label != null
		condition.setPosition(this);
		condition.checkSemantics();
		if (condition.type != TypeEnum.BOOL) {
			error("Condition for \"while\" must be bool, not ",
				condition.type.toString());
		}
		body.checkSemantics(true);
		if (label != null) {
			closeScopeNoError();
		}
	} // checkSemantics

	int codeGen(int numVars) {
		genLine();
		final String startLabel = genLabel();
		final String endLabel = genLabel();
		if (label != null) {
			labelInfo.breakLabel = genLabel();
			labelInfo.continueLabel = endLabel;
		}
		genCode("goto", endLabel);
		genCode(startLabel, ":");
		numVars = body.codeGen(numVars);
		genCode(endLabel, ":");
		condition.codeGen();
		genCode("ifne", startLabel);
		if (label != null) {
			genCode(labelInfo.breakLabel, ":");
		}
		adjustStackDepth(-1);
		return numVars;
	} // codeGen

} // ForNode

class AsgNode extends StatementNode {
	
	final NameNode lhs;
	final ExprNode rhs;

	AsgNode(CSXToken token, NameNode lhs, ExprNode rhs) {
		super(token);
		this.lhs = lhs;
		this.rhs = rhs;
	} // constructor AsgNode()

	void unparse(int indent) {
		genIndent(indent);
		lhs.unparse(indent);
		show(" = ");
		rhs.unparse(indent, true);
		show(";\n");
	} // unparse

	void checkSemantics() {
		lhs.setPosition(this);
		lhs.checkSemantics();
		if (lhs.kind != KindEnum.VARIABLE && lhs.kind != KindEnum.UNINITVAR) {
			error("Must assign to a variable, not to a ", lhs.symbolInfo.kind.toString());
		} else { // lhs kind is ok, check rhs and compatibility
			rhs.setPosition(this);
			rhs.checkSemantics();
			if (!assignCompatible(lhs.type, lhs.arrayLength, rhs)) {
				error(rhs.type + " cannot be assigned to ", lhs.type.toString());
			}
		} // lhs ok
		lhs.symbolInfo.kind = KindEnum.VARIABLE; // it has now been initialized
	} // checkSemantics

	int codeGen(int numVars) {
		genLine();
		final TypeEnum arrayType[] = {TypeEnum.INTEGERARRAY,
				TypeEnum.BOOLARRAY, TypeEnum.CHARARRAY};
		if (lhs.subscript != null) { // a subscripted variable on LHS
			if (lhs.symbolInfo.register == -1) { // static
				genCode("getstatic", "test/" + lhs.identifier.normalText,
					"[" + lhs.type.toJasminType());
				adjustStackDepth(1);
			} else { // dynamic
				genLoadStore(lhs.symbolInfo.register, 'a', "load", 
					"to array " + lhs.identifier.identifierText);
			}
			lhs.subscript.codeGen(); 
		}
		rhs.codeGen();
		if (rhs.type == TypeEnum.STRING) {
			genCode("invokestatic",
				"CSXLib/convertString(Ljava/lang/String;)[C");
		}
		assignTo(lhs);
		return numVars; // no new variables
	} // codeGen

} // AsgNode

class ReadListNode extends ASTNode {
	final List<NameNode> readList;

	ReadListNode(NameNode first) {
		readList = new ArrayList<NameNode>();
		readList.add(first);
	} // constructor ReadListNode

	void addName(NameNode next) {
		readList.add(0, next);
	} // addName

	void unparse(int indent) {
		show(' ');
		boolean first = true;
		for (NameNode name : readList) {
			if (!first) {
				show(", ");
			}
			name.unparse(indent);
			first = false;
		}
	} // unparse

	void checkSemantics() {
		for (NameNode name : readList) {
			name.setPosition(this);
			name.checkSemantics();
			if (name.symbolInfo.kind != KindEnum.VARIABLE && name.symbolInfo.kind != KindEnum.UNINITVAR) {
				error("Can only read into a variable, not a ",
					name.symbolInfo.kind.toString());
			} else if (name.type.notIn(intOrChar)) {
				error("Can only read int or char variables, not ",
					name.type.toString());
			}
			name.symbolInfo.kind = KindEnum.VARIABLE; // it has now been initialized
		}
	} // checkSemantics

	int codeGen(int numVars) {
		for (NameNode name : readList) {
			switch (name.type) {
				case INTEGER:
					genCode("invokestatic", "CSXLib/readInt()I");
					adjustStackDepth(1);
					assignTo(name);
					break;
				case CHAR:
					genCode("invokestatic", "CSXLib/readChar()C");
					adjustStackDepth(1);
					assignTo(name);
					break;
				default:
					error("Internal error: read to a ", name.type.toString());
			} // switch name.type
		}
		return numVars; // no new variables
	} // codeGen

} // class ReadListNode

class PrintListNode extends ASTNode {
	final List<ExprNode> printList;

	PrintListNode(ExprNode first) {
		printList = new ArrayList<ExprNode>();
		printList.add(first);
	} // constructor printListNode

	void addExpr(ExprNode next) {
		printList.add(0, next);
	} // addName

	void unparse(int indent) {
		show(' ');
		boolean first = true;
		for (ExprNode expr : printList) {
			if (!first) {
				show(", ");
			}
			expr.unparse(indent, true);
			first = false;
		}
	} // unparse

	void checkSemantics() {
		final TypeEnum goodType[] = {TypeEnum.INTEGER, TypeEnum.CHAR,
			TypeEnum.BOOL, TypeEnum.CHARARRAY, TypeEnum.STRING};
		for (ExprNode expr : printList) {
			expr.setPosition(this);
			expr.checkSemantics();
			if (expr.type.notIn(goodType)) {
				error("Cannot print items of type ", expr.type.toString());
			}
			expr.notUnassigned();
		}
	} // checkSemantics

	void codeGen() {
		for (ExprNode expr : printList) {
			expr.codeGen();
			switch (expr.type) {
				case INTEGER:
					genCode("invokestatic", "CSXLib/printInt(I)V");
					adjustStackDepth(-1);
					break;
				case CHAR:
					genCode("invokestatic", "CSXLib/printChar(C)V");
					adjustStackDepth(-1);
					break;
				case BOOL:
					genCode("invokestatic", "CSXLib/printBool(Z)V");
					adjustStackDepth(-1);
					break;
				case CHARARRAY:
					genCode("invokestatic", "CSXLib/printCharArray([C)V");
					adjustStackDepth(-1);
					break;
				case STRING:
					genCode("invokestatic",
						"CSXLib/printString(Ljava/lang/String;)V");
					adjustStackDepth(-1);
					break;
				default:
					error("Internal error: print a ", expr.type.toString());
			} // switch name.type
		} // each name in printList
	} // codeGen
} // class PrintListNode

class ReadNode extends StatementNode {
	
	final ReadListNode params;

	ReadNode(CSXToken token, ReadListNode params) {
		super(token);
		this.params = params;
	} // constructor ReadNode()

	void unparse(int indent) {
		genIndent(indent);
		show("read");
		params.unparse(indent);
		show(";\n");
	} // unparse

	void checkSemantics() {
		params.setPosition(this);
		params.checkSemantics();
	} // checkSemantics

	int codeGen(int numVars) {
		genLine();
		params.codeGen(numVars);
		return numVars; // no new variables
	} // codeGen
} // ReadNode

class PrintNode extends StatementNode {
	
	final PrintListNode params;

	PrintNode(CSXToken token, PrintListNode params) {
		super(token);
		this.params = params;
	} // constructor PrintNode()

	void unparse(int indent) {
		genIndent(indent);
		show("print");
		params.unparse(indent);
		show(";\n");
	} // unparse

	void checkSemantics() {
		params.setPosition(this);
		params.checkSemantics();
	} // checkSemantics

	int codeGen(int numVars) {
		genLine();
		params.codeGen();
		return numVars; // no new variables
	} // codeGen
} // PrintNode

class CallStatementNode extends StatementNode {
	
	final CSXIdentifierToken functionName;
	final ActualParamsNode actuals;

	CallStatementNode(CSXIdentifierToken functionName, ActualParamsNode actuals) {
		super(functionName);
		this.functionName = functionName;
		if (actuals == null) {
			this.actuals = new ActualParamsNode();
		} else {
			this.actuals = actuals;
		}
	} // constructor CallNode()

	void unparse(int indent) {
		genIndent(indent);
		show(functionName.identifierText);
		show('(');
		if (actuals != null) {
			actuals.unparse(indent);
		}
		show(");\n");
	} // unparse

	SymbolInfo symbolInfo = null;

	void checkSemantics() {
		if (actuals != null) {
			actuals.setPosition(this);
			actuals.checkSemantics();
		}
		symbolInfo = symbolTable.globalLookup(functionName.normalText);
		if (symbolInfo == null) {
			error(functionName.identifierText, " is not declared");
			try {
				symbolInfo = new SymbolInfo(functionName.normalText,
					TypeEnum.VOID, new ArrayList<TypeEnum>());
				insertNoEmptyError(symbolInfo); 
			} catch (DuplicateException dummy) {
				error("Internal error: duplicate of non-existent entry");
			}
		} else if (symbolInfo.kind != KindEnum.FUNCTION) {
			error("Attempt to call a non-function: ", functionName.identifierText,
				"; it is a ", symbolInfo.kind.toString());
		} else if (symbolInfo.returnType != TypeEnum.VOID) {
			error("Cannot call ", symbolInfo.returnType.toString(),
				" function as a statement");
		} else if (symbolInfo.paramTypes.size() != actuals.parameters.size()) {
			error("Wrong number of actual parameters to ",
				functionName.identifierText, "; ",
				Integer.toString(actuals.parameters.size()),
				" instead of ",
				Integer.toString(symbolInfo.paramTypes.size()));
		} else {
			for (int index = 0; index < actuals.parameters.size(); index += 1) {
				final TypeEnum destType = symbolInfo.paramTypes.get(index);
				final ExprNode sourceExpr = actuals.parameters.get(index);
				if (!assignCompatible(destType, 0, sourceExpr)) {
					error("Actual parameter ", Integer.toString(index+1),
						" to ", functionName.identifierText, ", type ",
						sourceExpr.type.toString(),
						", does not match formal, type ",
						destType.toString());
				}
			} // each actual
		}
	} // checkSemantics

	int codeGen(int numVars) {
		genLine();
		actuals.codeGen();
		genCode("invokestatic", "test/" + functionName.normalText + 
			symbolInfo.signature);
		adjustStackDepth(-actuals.parameters.size());
		return numVars; // no new variables
	} // codeGen

} // CallStatementNode

class ReturnNode extends StatementNode {
	
	final ExprNode returnVal;

	ReturnNode(CSXToken token, ExprNode returnVal) {
		super(token);
		this.returnVal = returnVal;
	} // constructor ReturnNode()

	void unparse(int indent) {
		genIndent(indent);
		show("return");
		if (returnVal != null) {
			show(' ');
			returnVal.unparse(indent, true);
		}
		show(";\n");
	} // unparse

	void checkSemantics() {
		if (returnVal != null) {
			returnVal.setPosition(this);
			returnVal.checkSemantics();
		}
		SymbolInfo returnInfo;
		returnInfo = symbolTable.globalLookup("RETURNVALUE");
		if (returnInfo.type == TypeEnum.VOID) { // should return void
			if (returnVal != null) {
				error("Must not return a value from a void function");
			}
		} else if (returnVal == null){
			error("Must return a value from a non-void function");
		} else if (returnVal.type != returnInfo.type) {
			error("Must return ", returnInfo.type.toString(), ", not ",
				returnVal.type.toString());
		}
		try {
			insertNoEmptyError(
				new SymbolInfo("RETURNED", KindEnum.VARIABLE, TypeEnum.VOID));
		} catch (DuplicateException dummy) {
			// no worries if there are multiple returns.
		}
	} // checkSemantics

	int codeGen(int numVars) {
		genLine();
		if (returnVal != null) {
			returnVal.codeGen();
			genCode("ireturn");
			adjustStackDepth(-1);
		} else {
			genCode("return");
		}
		return numVars; // no new variables
	} // codeGen
} // ReturnNode

class BreakNode extends StatementNode {
	
	final CSXIdentifierToken label;

	BreakNode(CSXIdentifierToken label) {
		super(label);
		this.label = label;
	} // constructor BreakNode()

	void unparse(int indent) {
		genIndent(indent);
		show("break");
		if (label != null) {
			show(' ');
			show(label.identifierText);
		}
		show(";\n");
	} // unparse

	SymbolInfo labelInfo;
	void checkSemantics() {
		labelInfo = symbolTable.globalLookup(label.normalText);
		if (labelInfo == null || labelInfo.kind != KindEnum.CURRENTLABEL) {
			error("Not in a loop named ", label.identifierText);
		}
	} // checkSemantics

	int codeGen(int numVars) {
		genLine();
		genCode("goto", labelInfo.breakLabel);
		return numVars; // no new variables
	} // codeGen

} // BreakNode

class ContinueNode extends StatementNode {
	
	final CSXIdentifierToken label;

	ContinueNode(CSXIdentifierToken label) {
		super(label);
		this.label = label;
	} // constructor ContinueNode()

	void unparse(int indent) {
		genIndent(indent);
		show("continue");
		if (label != null) {
			show(' ');
			show(label.identifierText);
		}
		show(";\n");
	} // unparse

	SymbolInfo labelInfo;
	void checkSemantics() {
		labelInfo = symbolTable.globalLookup(label.normalText);
		if (labelInfo == null || labelInfo.kind != KindEnum.CURRENTLABEL) {
			error("Not in a loop named ", label.identifierText);
		}
	} // checkSemantics

	int codeGen(int numVars) {
		genLine();
		genCode("goto", labelInfo.continueLabel);
		return numVars; // no new variables
	} // codeGen

} // ContinueNode

class BlockNode extends StatementNode {
	
	final VarDeclsNode vars;
	final StatementsNode statements;

	BlockNode(CSXToken token, VarDeclsNode vars,
			StatementsNode statements) {
		super(token);
		this.vars = vars;
		this.statements = statements;
	} // constructor BlockNode()

	void unparse(int indent) {
		genIndent(indent);
		show("{\n");
		vars.unparse(indent+1);
		statements.unparse(indent+1);
		genIndent(indent);
		show("}\n");
	} // unparse

	void unparse(int indent, boolean noBracket) {
		vars.unparse(indent);
		statements.unparse(indent);
	} // unparse

	void checkSemantics() { // if no parameter, then open scope
		checkSemantics(true);
	} // checkSemantics

	void checkSemantics(boolean newScope) {
		if (newScope) symbolTable.openScope();
		vars.checkSemantics();
		statements.checkSemantics();
		final SymbolInfo returnInfo = symbolTable.localLookup("RETURNED");
		if (newScope) closeScopeNoError();
		if (returnInfo != null) { // propagate a returned indicator upward
			try {
				symbolTable.insert(returnInfo);
			} catch (EmptySTException e) {
				error("Internal error: no scope outside a block node");
			} catch (DuplicateException e) {
				// no problem; other block has also returned.
			}
		}
	} // checkSemantics

	int codeGen(int numVars) {
		numVars = vars.codeGen(numVars);
		numVars = statements.codeGen(numVars);
		return numVars;
	} // codeGen

} // BlockNode

class FuncDeclNode extends ASTNode {
	
	final CSXIdentifierToken identifier;
	final FormalParamsNode formals;
	final BlockNode block;
	final TypeNode returnType;

	FuncDeclNode(TypeNode returnType, CSXIdentifierToken identifier,
			FormalParamsNode formals, BlockNode block) {
		super(identifier);
		this.returnType = returnType;
		this.identifier = identifier;
		if (formals == null) {
			this.formals = new FormalParamsNode();
		} else {
			this.formals = formals;
		}
		this.block = block;
	} // constructor FuncDeclNode()

	void unparse(int indent) {
		show("func ", indent);
		show(identifier.identifierText);
		formals.unparse(indent);
		if (returnType != null) {
			show(' ');
			returnType.unparse(indent);
		}
		show(" {\n");
		block.unparse(indent+1, true);
		genIndent(indent);
		show("} // func ");
		show(identifier.identifierText);
		show("\n");
	} // unparse

	SymbolInfo symbolInfo = null;
	boolean normalReturn = false;

	void checkSemantics() {
		if (returnType == null) {
			symbolInfo = new SymbolInfo(identifier.normalText, TypeEnum.VOID,
				formals.formalTypes);
		} else {
			returnType.checkSemantics();
			symbolInfo = new SymbolInfo(identifier.normalText, returnType.type,
				formals.formalTypes);
		}
		try {
			insertNoEmptyError(symbolInfo);
		} catch (DuplicateException dummy) {
			error("Already declared in this scope: " +
				identifier.identifierText);
		}
		symbolTable.openScope();
		formals.setPosition(this);
		formals.checkSemantics();
		try {
			insertNoEmptyError(new SymbolInfo("RETURNVALUE",
				KindEnum.VARIABLE,
				returnType == null ? TypeEnum.VOID : returnType.type
			));
		} catch (DuplicateException dummy) {
			error("Internal error: duplicate return value");
		}
		block.checkSemantics(false); // block shares my symbol table
		final SymbolInfo returnInfo = symbolTable.globalLookup("RETURNED");
		if (returnInfo == null && returnType != null) {
			error("Function ", identifier.identifierText,
				" has no return statement");
		}
		closeScopeNoError();
	} // checkSemantics

	void codeGen() {
		maxDepth = 0; // start over for each function
		curDepth = 0; // start over for each function
		int numVars = 0;
		numVars = formals.codeGen(numVars); // computes formals.typeString
		final StringBuilder signatureBuilder =
			new StringBuilder().append(formals.typeString);
		if (returnType == null) {
			signatureBuilder.append('V');
		} else {
			signatureBuilder.append(returnType.type.toJasminType());
		}
		symbolInfo.signature = signatureBuilder.toString();
		genCode(".method", "public static",
			identifier.normalText + symbolInfo.signature);
		numVars = block.codeGen(numVars);
		genCode(".limit", "locals",
			Integer.toString(formals.formals.size() + numVars));
		if (!normalReturn) {
			genCode("return", "; implicit");
		}
		if (lastCode.indexOf(':') >= 0) { // must not end with a label
			genCode("return", "; prophylactic");
		}
		genCode(".limit", "stack", Integer.toString(maxDepth));
		genCode(".end", "method", ";", identifier.normalText);
		genCode(); // blank line separator
		if (curDepth != 0) {
			show("cur depth ended up " + curDepth + '\n');
		}
	} // codeGen

} // class FunctionDeclNode

abstract class ExprNode extends ASTNode {
	public TypeEnum type;
	public int arrayLength; // only if type is one of the array types or string
	public KindEnum kind;

	abstract void unparse(int indent, boolean topLevel);
	abstract void checkSemantics();

	void notUnassigned() {
		if (this instanceof NameNode && kind == KindEnum.UNINITVAR) {
			error("Accessing an unassigned variable " +
				((NameNode) this).identifier.identifierText);
			kind = KindEnum.VARIABLE; // prevent cascading errors
		}
	} // notUnassigned

} // class ExprNode

class OpNode extends ExprNode {
	final int operator;
	final ExprNode left, right;
	OpNode(int op, ExprNode left, ExprNode right) {
		this.operator = op;
		this.left = left;
		this.right = right;
	} // constructor OpNode
	String opToString() {
		switch (operator) {
			case sym.SLASH : return("/");
			case sym.MINUS : return("-");
			case sym.NOT : return("!");
			case sym.LT : return("<");
			case sym.GEQ : return(">=");
			case sym.PLUS : return("+");
			case sym.COR : return("||");
			case sym.EQ : return("==");
			case sym.TIMES : return("*");
			case sym.CAND : return("&&");
			case sym.LEQ : return("<=");
			case sym.GT : return(">");
			case sym.NOTEQ : return("!=");
			default: return("unrecognized operator number " + operator);
		} // switch(operator)
	} // opToString

	void unparse(int indent, boolean topLevel) {
		if (right == null) { // unary
			show(opToString());
			left.unparse(indent, false);
		} else { // binary
			if (!topLevel) {
				show('(');
			}
			left.unparse(indent, false);
			show(new StringBuilder().
				append(' ').append(opToString()).append(' ').toString());
			right.unparse(indent, false);
			if (!topLevel) {
				show(')');
			}
		} // binary
	} // unparse

	void checkSemantics() {
		left.setPosition(this);
		left.checkSemantics();
		left.notUnassigned();
		if (right == null) { // unary NOT
			if (left.type != TypeEnum.BOOL) {
				error("Operand for \"not\" must be bool, not " + left.type);
			}
			type = TypeEnum.BOOL;
		} else { // binary operation
			right.setPosition(this);
			right.checkSemantics();
			right.notUnassigned();
			switch (operator) {
				case sym.SLASH :
				case sym.MINUS : 
				case sym.TIMES : 
				case sym.PLUS : 
					if (left.type.notIn(intOrChar)) {
						error("Left operand for " + opToString() +
							" must be int or char, not " + left.type);
					}
					if (right.type.notIn(intOrChar)) {
						error("Right operand for " + opToString() +
							" must be int or char, not " + right.type);
					}
					type = TypeEnum.INTEGER;
					break;
				case sym.COR :
				case sym.CAND :
					if (left.type != TypeEnum.BOOL) {
						error("Left operand for " + opToString() +
							" must be bool, not " + left.type);
					}
					if (right.type != TypeEnum.BOOL) {
						error("Right operand for " + opToString() +
							" must be bool, not " + right.type);
					}
					type = TypeEnum.BOOL;
					break;
				case sym.LT : 
				case sym.GEQ :
				case sym.EQ : 
				case sym.LEQ :
				case sym.GT : 
				case sym.NOTEQ :
					if (left.type == TypeEnum.ERROR) {
						// don't cascade errors
					} else if (!left.type.notIn(intOrChar)) {
						if (right.type.notIn(intOrChar)) {
							error("Right operand for " + opToString() +
								" must match left (" + left.type + "), not " +
								right.type);
						}
					} else if (left.type != TypeEnum.BOOL) {
						error("Left operand for " + opToString() +
							" must be int, char, or bool, not " + left.type);
					} else if (right.type != TypeEnum.BOOL) {
						error("Right operand for " + opToString() +
							" must match left (bool), not " + right.type);
					}
					type = TypeEnum.BOOL;
					break;
				default: error("Internal error: unrecognized operator number "
					+ operator);
			} // switch(operator)
		} // binary operation
		kind = KindEnum.VALUE;
	} // checkSemantics

	void emitConditional(String condition) {
		final String trueLabel = genLabel();
		final String endLabel = genLabel();
		genCode("if_icmp" + condition, trueLabel);
		genCode("bipush", "0");
		genCode("goto", endLabel);
		genCode(trueLabel, ":");
		genCode("bipush", "1");
		genCode(endLabel, ":");
	} // emitConditional

	void codeGen() {
		if (right == null) { // unary NOT: 1-val
			genConstant(1);
			left.codeGen();
			genCode("isub");
			adjustStackDepth(-1);
		} else { // binary operation
			left.codeGen();
			right.codeGen();
			switch (operator) {
				case sym.SLASH : genCode("idiv"); break;
				case sym.MINUS : genCode("isub"); break;
				case sym.PLUS : genCode("iadd"); break;
				case sym.TIMES : genCode("imul"); break;
				case sym.CAND : genCode("imul"); break;
				case sym.COR : genCode("iadd"); break;
				case sym.LT : 
					emitConditional("lt"); break;
				case sym.GEQ :
					emitConditional("ge"); break;
				case sym.EQ :
					emitConditional("eq"); break;
				case sym.LEQ :
					emitConditional("le"); break;
				case sym.GT :
					emitConditional("gt"); break;
				case sym.NOTEQ :
					emitConditional("ne"); break;
				default: error("unrecognized operator number " + operator);
					break;
			} // switch(operator)
			adjustStackDepth(-1);
		} // binary expression
	} // codeGen
} // class OpNode

class CastNode extends ExprNode {
	final TypeNode castType;
	final ExprNode expr;
	CastNode(TypeNode castType, ExprNode expr) {
		this.castType = castType;
		this.expr = expr;
	}
	void unparse(int indent, boolean topLevel) {
		castType.unparse(indent);
		show('(');
		expr.unparse(indent, true);
		show(')');
	} // unparse

	void checkSemantics() {
		castType.setPosition(this);
		castType.checkSemantics();
		final TypeEnum toType = castType.type;
		final TypeEnum validType[] =
			{TypeEnum.INTEGER, TypeEnum.CHAR, TypeEnum.BOOL};
		if (toType.notIn(validType)) {
			error("Cannot cast to type " + toType);
		}
		expr.setPosition(this);
		expr.checkSemantics();
		final KindEnum fromKind = expr.kind;
		final TypeEnum fromType = expr.type;
		if (fromKind != KindEnum.VARIABLE && fromKind != KindEnum.CONSTANT &&
				fromKind != KindEnum.VALUE) {
			error("Cannot cast from a " + fromKind);
		} else if (fromType.notIn(validType)) {
			error("Cannot cast from type " + fromType);
		}
		type = castType.type;
	} // checkSemantics

	void codeGen() {
		expr.codeGen();
		if (castType.type != expr.type &&
			castType.type == TypeEnum.CHAR) {
				genConstant(127);
				genCode("iand");
				adjustStackDepth(-1);
		}
	} // codeGen
} // class CastNode

class ActualParamsNode extends ASTNode {
	final List<ExprNode> parameters;

	ActualParamsNode() {
		parameters = new ArrayList<ExprNode>();
	} // constructor ActualParamsNode

	ActualParamsNode(ExprNode firstParam) {
		parameters = new ArrayList<ExprNode>();
		parameters.add(firstParam);
	} // constructor ActualParamsNode
	ActualParamsNode addActual(ExprNode nextParam) {
		parameters.add(0, nextParam);
		return this;
	}

	void unparse(int indent) {
		boolean first = true;
		for (ExprNode parameter : parameters) {
			if (!first) {
				show(", ");
			}
			parameter.unparse(indent, true);
			first = false;
		} // each parameter
	} // unparse

	void checkSemantics() {
		for (ExprNode parameter : parameters) {
			parameter.setPosition(this);
			parameter.checkSemantics();
		}
	} // checkSemantics

	void codeGen() {
		for (ExprNode parameter : parameters) {
			parameter.codeGen();
		}
	} // codeGen
} // class ActualParamsNode

class CallExpressionNode extends ExprNode {
	final CSXIdentifierToken functionName;
	final ActualParamsNode actuals;

	CallExpressionNode(CSXIdentifierToken functionName) {
		this.functionName = functionName;
		this.actuals = new ActualParamsNode();
	} // constructor CallExpressionNode

	CallExpressionNode(CSXIdentifierToken functionName, ActualParamsNode actuals) {
		this.functionName = functionName;
		this.actuals = actuals;
	} // constructor CallExpressionNode

	void unparse(int indent, boolean topLevel) {
		show(functionName.identifierText);
		show('(');
		if (actuals != null) {
			actuals.unparse(indent);
		}
		show(")");
	} // unparse

	SymbolInfo symbolInfo = null;
	void checkSemantics() {
		if (actuals != null) {
			actuals.setPosition(this);
			actuals.checkSemantics();
		}
		symbolInfo = symbolTable.globalLookup(functionName.normalText);
		if (symbolInfo == null) {
			error(functionName.identifierText + " is not declared");
			try {
				symbolInfo = new SymbolInfo(functionName.normalText,
					KindEnum.FUNCTION, TypeEnum.ERROR);
				insertNoEmptyError(symbolInfo); 
			} catch (DuplicateException dummy) {
				error("Internal error: duplicate of non-existent entry");
			}
			kind = KindEnum.FUNCTION;
			type = TypeEnum.ERROR;
		} else if (symbolInfo.kind != KindEnum.FUNCTION) {
			error("Attempt to call a non-function: " +
				functionName.identifierText + "; it is a " +
				symbolInfo.kind);
		} else if (symbolInfo.paramTypes.size() != actuals.parameters.size()) {
			error("Wrong number of actual parameters to " + 
				functionName.identifierText + "; " + actuals.parameters.size() +
				" instead of " + symbolInfo.paramTypes.size());
		} else {
			for (int index = 0; index < actuals.parameters.size(); index += 1) {
				final TypeEnum destType = symbolInfo.paramTypes.get(index);
				final ExprNode sourceExpr = actuals.parameters.get(index);
				if (!assignCompatible(destType, 0, sourceExpr)) {
					error("Actual parameter " + (index+1) + " to " +
						functionName.identifierText + ", type " +
						sourceExpr.type + ", does not match formal, type " +
						destType);
				}
			} // each actual
		}
		type = symbolInfo.returnType;
	} // checkSemantics

	void codeGen() {
		actuals.codeGen();
		genCode("invokestatic", "test/" + functionName.normalText +
			symbolInfo.signature);
		adjustStackDepth(1-actuals.parameters.size());
	} // codeGen
} // class CallExpressionNode

class IntLitNode extends ExprNode {
	final CSXIntLitToken value;
	IntLitNode(CSXIntLitToken value) {
		this.value = value;
	}
	void unparse(int indent, boolean topLevel) {
		if (value instanceof CSXBadIntLitToken) {
			show("int value out of range; reduced\n");
		}
		final int result = value.intValue;
		if (result >= 0) {
			show(result);
		} else {
			show('~');
			show(-result);
		}
	} // unparse

	void checkSemantics() {
		type = TypeEnum.INTEGER;
		kind = KindEnum.CONSTANT;
	} // checkSemantics

	void codeGen() {
		genConstant(value.intValue);
	} // codeGen

} // class IntLitNode

class CharLitNode extends ExprNode {
	final CSXCharLitToken value;
	CharLitNode(CSXCharLitToken value) {
		this.value = value;
	}
	void unparse(int indent, boolean topLevel) {
		show(value.charText);
	} // unparse

	void checkSemantics() {
		type = TypeEnum.CHAR;
		kind = KindEnum.CONSTANT;
	} // checkSemantics

	void codeGen() {
		genConstant(value.effectiveChar);
	} // codeGen
	
} // class CharLitNode

class StrLitNode extends ExprNode {
	final CSXStringLitToken value;
	StrLitNode(CSXStringLitToken value) {
		this.value = value;
	}
	void unparse(int indent, boolean topLevel) {
		show(value.stringText);
	} // unparse

	void checkSemantics() {
		type = TypeEnum.STRING;
		kind = KindEnum.CONSTANT;
		arrayLength = value.effectiveText.length();
	} // checkSemantics

	void codeGen() {
		genCode("ldc", value.stringText);
		adjustStackDepth(1);
	} // codeGen
} // class StrLitNode

class BoolLitNode extends ExprNode {
	final boolean value;
	BoolLitNode(CSXBoolLitToken value) {
		this.value = value.boolValue;
	}
	void unparse(int indent, boolean topLevel) {
		show(value);
	} // unparse

	void checkSemantics() {
		type = TypeEnum.BOOL;
		kind = KindEnum.CONSTANT;
	} // checkSemantics

	void codeGen() {
		genConstant(value ? 1 : 0);
	} // codeGen
} // class BoolLitNode

class NameNode extends ExprNode {

	final CSXIdentifierToken identifier;
	final ExprNode subscript;

	NameNode(CSXIdentifierToken identifier, ExprNode subscript) {
		this.identifier = identifier;
		this.subscript = subscript;
	}

	void unparse(int indent) {
		unparse(indent, false);
	} // unparse

	void unparse(int indent, boolean topLevel) {
		show(identifier.identifierText);
		if (subscript != null) {
			show('[');
			subscript.unparse(indent, true);
			show(']');
		}
	} // unparse()

	public SymbolInfo symbolInfo = null;

	void checkSemantics() {
		symbolInfo = symbolTable.globalLookup(identifier.normalText);
		if (symbolInfo == null) {
			error("No such symbol: " + identifier.identifierText);
			try {
				insertNoEmptyError(
					new SymbolInfo(identifier.normalText, KindEnum.VARIABLE,
						TypeEnum.ERROR)
				);
			} catch (DuplicateException dummy) {
				error("Internal error: duplicate of non-existent entry");
			}
			kind = KindEnum.VARIABLE;
			type = TypeEnum.ERROR;
		} else { // symbol exists
			if (subscript != null) { // subscripted
				subscript.setPosition(this);
				subscript.checkSemantics();
				final TypeEnum validType[] = {TypeEnum.INTEGER, TypeEnum.CHAR};
				if (subscript.type.notIn(validType)) {
					error("Subscript must be int or char, not " +
						subscript.type);
				}
				switch (symbolInfo.type) {
					case INTEGERARRAY: type = TypeEnum.INTEGER; break;
					case BOOLARRAY: type = TypeEnum.BOOL; break;
					case CHARARRAY: type = TypeEnum.CHAR; break;
					default:
						type = TypeEnum.ERROR;
						error("Cannot apply subscript to non-array " +
							identifier.identifierText);
				}
				kind = KindEnum.VARIABLE; // no check for initialization
			} else { // not subscripted; might be an array
				type = symbolInfo.type;
				switch (symbolInfo.type) {
					case INTEGERARRAY:
					case BOOLARRAY:
					case CHARARRAY:
						arrayLength = symbolInfo.arrayLength; break;
					default:
						arrayLength = -1; // not an array
				}
				kind = symbolInfo.kind;
			} // not subscripted, but possibly an array
			symbolInfo.kind = kind;
		} // symbol exists
	} // checkSemantics

	void codeGen() {
		switch (symbolInfo.type) {
			// TODO: factor these cases
			case INTEGERARRAY:
				if (symbolInfo.register == -1) { // static
					genCode("getstatic", "test/" + identifier.normalText,
						symbolInfo.type.toJasminType());
					adjustStackDepth(1);
				} else { // dynamic
					genLoadStore(symbolInfo.register, 'a', "load", 
						identifier.identifierText);
				}
				if (subscript != null) { // a subscripted variable on RHS
					// genCode("; array in NameNode");
					subscript.codeGen(); 
					genCode("iaload");
					adjustStackDepth(-1);
				}
				break;
			case BOOLARRAY:
				if (symbolInfo.register == -1) { // static
					genCode("getstatic", "test/" + identifier.normalText,
						symbolInfo.type.toJasminType());
					adjustStackDepth(1);
				} else { // dynamic
					genLoadStore(symbolInfo.register, 'a', "load", 
						identifier.identifierText);
				}
				if (subscript != null) { // a subscripted variable on RHS
					// genCode("; array in NameNode");
					subscript.codeGen(); 
					genCode("baload");
					adjustStackDepth(-1);
				}
				break;
			case CHARARRAY:
				if (symbolInfo.register == -1) { // static
					genCode("getstatic", "test/" + identifier.normalText,
						symbolInfo.type.toJasminType());
					adjustStackDepth(1);
				} else { // dynamic
					genLoadStore(symbolInfo.register, 'a', "load", 
						identifier.identifierText);
				}
				if (subscript != null) { // a subscripted variable on RHS
					// genCode("; array in NameNode");
					subscript.codeGen(); 
					genCode("caload");
					adjustStackDepth(-1);
				}
				break;
			case INTEGER:
			case BOOL:
			case CHAR:
				if (symbolInfo.register == -1) { // static
					genCode("getstatic", "test/" + identifier.normalText,
					symbolInfo.type.toJasminType());
					adjustStackDepth(1);
				} else { // dynamic
					genLoadStore(symbolInfo.register, 'i', "load", identifier.normalText);
				}
				break;
			case STRING:
				if (symbolInfo.register == -1) { // static
					genCode("getstatic", "test/" + identifier.normalText,
						"Ljava/lang/String;");
					adjustStackDepth(1);
				} else { // dynamic
					genLoadStore(symbolInfo.register, 'a', "load", 
						identifier.identifierText);
				}
				break;
			default:
				error("Internal error: can't put ", identifier.normalText,
					" on stack");
		} // switch symbolInfo.type
	} // codeGen

} // class NameNode

class TypeNode extends ASTNode {
	
	final CSXIdentifierToken name;
	TypeEnum type;

	TypeNode(CSXIdentifierToken name) {
		super(name);
		this.name = name;
	} // constructor TypeNode()

	void unparse(int indent) {
		show(name.identifierText);
	} // unparse

	void checkSemantics() {
		type = TypeEnum.toType(name.normalText);
	} // checkSemantics
} // TypeNode

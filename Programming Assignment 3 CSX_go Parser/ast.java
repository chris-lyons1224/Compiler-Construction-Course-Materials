import java.util.List;
import java.util.ArrayList;

class ASTNode {
// abstract superclass; only subclasses are actually created

	int lineNum; // sometimes set after the fact during checkSemantics().
	final int colNum;

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

} // VarDeclsNode

class FormalParamsNode extends ASTNode {
	
	final List<FormalParamNode> formals;

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
} // StatementsNode

abstract class StatementNode extends ASTNode {
	
	StatementNode(CSXToken token) {
		super(token);
	} // constructor StatementNode(CSXToken)

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
} // PrintNode

class CallStatementNode extends StatementNode {
	
	final CSXIdentifierToken funcName;
	final ActualParamsNode actuals;

	CallStatementNode(CSXIdentifierToken funcName, ActualParamsNode actuals) {
		super(funcName);
		this.funcName = funcName;
		if (actuals == null) {
			this.actuals = new ActualParamsNode();
		} else {
			this.actuals = actuals;
		}
	} // constructor CallNode()

	void unparse(int indent) {
		genIndent(indent);
		show(funcName.identifierText);
		show('(');
		if (actuals != null) {
			actuals.unparse(indent);
		}
		show(");\n");
	} // unparse
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
} // ContinueNode

class BlockNode extends StatementNode {
	
	final VarDeclsNode fields;
	final StatementsNode statements;

	BlockNode(CSXToken token, VarDeclsNode fields,
			StatementsNode statements) {
		super(token);
		this.fields = fields;
		this.statements = statements;
	} // constructor BlockNode()

	void unparse(int indent) {
		genIndent(indent);
		show("{\n");
		fields.unparse(indent+1);
		statements.unparse(indent+1);
		genIndent(indent);
		show("}\n");
	} // unparse

	void unparse(int indent, boolean noBracket) {
		fields.unparse(indent);
		statements.unparse(indent);
	} // unparse
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
} // class FuncDeclNode

abstract class ExprNode extends ASTNode {
	abstract void unparse(int indent, boolean topLevel);
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
} // class ActualParamsNode

class CallExpressionNode extends ExprNode {
	final CSXIdentifierToken funcName;
	final ActualParamsNode actuals;

	CallExpressionNode(CSXIdentifierToken funcName) {
		this.funcName = funcName;
		this.actuals = new ActualParamsNode();
	} // constructor CallExpressionNode

	CallExpressionNode(CSXIdentifierToken funcName, ActualParamsNode actuals) {
		this.funcName = funcName;
		this.actuals = actuals;
	} // constructor CallExpressionNode

	void unparse(int indent, boolean topLevel) {
		show(funcName.identifierText);
		show('(');
		if (actuals != null) {
			actuals.unparse(indent);
		}
		show(")");
	} // unparse
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
} // class IntLitNode

class CharLitNode extends ExprNode {
	final CSXCharLitToken value;
	CharLitNode(CSXCharLitToken value) {
		this.value = value;
	}
	void unparse(int indent, boolean topLevel) {
		show(value.charText);
	} // unparse
} // class CharLitNode

class StrLitNode extends ExprNode {
	final CSXStringLitToken value;
	StrLitNode(CSXStringLitToken value) {
		this.value = value;
	}
	void unparse(int indent, boolean topLevel) {
		show(value.stringText);
	} // unparse
} // class StrLitNode

class BoolLitNode extends ExprNode {
	final boolean value;
	BoolLitNode(CSXBoolLitToken value) {
		this.value = value.boolValue;
	}
	void unparse(int indent, boolean topLevel) {
		show(value);
	} // unparse
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

} // class NameNode

class TypeNode extends ASTNode {
	
	final CSXIdentifierToken name;

	TypeNode(CSXIdentifierToken name) {
		super(name);
		this.name = name;
	} // constructor TypeNode()

	void unparse(int indent) {
		show(name.identifierText);
	} // unparse

} // TypeNode

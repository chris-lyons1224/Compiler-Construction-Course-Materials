import java.io.Reader;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileNotFoundException;
import java_cup.runtime.*;

final class P2 {

	public static void
	main(String args[]) throws java.io.IOException {

		if (args.length != 1) {
			System.out.println(
				"Error: Input file must be named on command line." );
			System.exit(-1);
		}

		java.io.Reader yyin = null;

		try {
			yyin = new BufferedReader(new FileReader(args[0]));
		} catch (FileNotFoundException notFound){
			System.out.println ("Error: unable to open input file.");
			System.exit(-1);
		}

		// lex is a JLex-generated scanner that reads from yyin
		final Yylex lex = new Yylex(yyin);	

		System.out.println ("Raphael Finkel");
		System.out.println ("Begin test of CSX scanner.");

		Symbol token = lex.yylex();

		while ( token.sym != sym.EOF) {

			System.out.print( ((CSXToken) token.value).linenum + ":"
					+ ((CSXToken) token.value).colnum + "\t");

			switch (token.sym) {

			// all reserved words
			case  sym.rw_BOOL:
			case  sym.rw_BREAK:
			case  sym.rw_CHAR:
			case  sym.rw_CONST:
			case  sym.rw_CONTINUE:
			case  sym.rw_ELSE:
			case  sym.rw_FOR:
			case  sym.rw_FUNC:
			case  sym.rw_IF:
			case  sym.rw_INT:
			case  sym.rw_PACKAGE:
			case  sym.rw_PRINT:
			case  sym.rw_READ:
			case  sym.rw_RETURN:
			case  sym.rw_VAR:
				System.out.println("Reserved (" +
					((CSXIdentifierToken) token.value).identifierText +
					")");
				break;

			case sym.IDENTIFIER:
				System.out.println("Identifier (" +
					((CSXIdentifierToken) token.value).identifierText +
					")");
				break;

			case sym.INTLIT:
				if (token.value instanceof CSXBadIntLitToken) {
					System.out.println("Bad integer literal; using (" +
						((CSXBadIntLitToken) token.value).intValue + ")");
				} else {
					System.out.println("Integer literal (" +
						((CSXIntLitToken) token.value).intValue + ")");
				}
				break;

			case sym.STRLIT:
				System.out.println("String literal (" +
					((CSXStringLitToken) token.value).stringText + ")");
				break;

			case sym.BOOLLIT:
				System.out.println("Boolean literal (" +
					((CSXBoolLitToken) token.value).boolValue + ")");
				break;

			case sym.CHARLIT:
				System.out.println("Character literal (" +
					((CSXCharLitToken) token.value).charText + ")");
				break;

			case sym.LPAREN:
				System.out.println("Symbol (" + "(" + ")");
				break;

			case sym.RPAREN:
				System.out.println("Symbol (" + ")" + ")");
				break;

			case sym.LBRACKET:
				System.out.println("Symbol (" + "[" + ")");
				break;

			case sym.RBRACKET:
				System.out.println("Symbol (" + "]" + ")");
				break;

			case sym.ASG:
				System.out.println("Symbol (" + "=" + ")");
				break;

			case sym.SEMI:
				System.out.println("Symbol (" + ";" + ")");
				break;

			case sym.PLUS:
				System.out.println("Symbol (" + "+" + ")");
				break;

			case sym.MINUS:
				System.out.println("Symbol (" + "-" + ")");
				break;

			case sym.TIMES:
				System.out.println("Symbol (" + "*" + ")");
				break;

			case sym.SLASH:
				System.out.println("Symbol (" + "/" + ")");
				break;

			case sym.EQ:
				System.out.println("Symbol (" + "==" + ")");
				break;

			case sym.NOTEQ:
				System.out.println("Symbol (" + "!=" + ")");
				break;

			case sym.CAND:
				System.out.println("Symbol (" + "&&" + ")");
				break;

			case sym.COR:
				System.out.println("Symbol (" + "||" + ")");
				break;

			case sym.LT:
				System.out.println("Symbol (" + "<" + ")");
				break;

			case sym.GT:
				System.out.println("Symbol (" + ">" + ")");
				break;

			case sym.LEQ:
				System.out.println("Symbol (" + "<=" + ")");
				break;

			case sym.GEQ:
				System.out.println("Symbol (" + ">=" + ")");
				break;

			case sym.COMMA:
				System.out.println("Symbol (" + "," + ")");
				break;

			case sym.NOT:
				System.out.println("Symbol (" + "!" + ")");
				break;

			case sym.LBRACE:
				System.out.println("Symbol (" + "{" + ")");
				break;

			case sym.RBRACE:
				System.out.println("Symbol (" + "}" + ")");
				break;

			case sym.COLON:
				System.out.println("Symbol (" + ":" + ")");
				break;

			case sym.error:
				System.out.println("Invalid token (" +
					((CSXErrorToken) token.value).errorChar + ")");
				break;

			default:
				System.out.println("unrecognized token type: " + token.value);
			} // switch(token.sym)
			token = lex.yylex(); // get next token
		} // not at EOF
		System.out.println("End test of CSX scanner.");
	} // main()

	private P2() {
		// private constructor to prevent multiple instances.
	}

} // class P2

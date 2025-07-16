/* This file probably doesn't need to be changed */

import java_cup.runtime.Symbol;
import java.io.IOException;
import java.io.Reader;

final class Scanner {
	private	static Yylex lex = null;

	private static final Object lock = new Object();

	public static void init(Reader yyin) {
		synchronized(lock) {
			if (lex == null) {
				lex = new Yylex(yyin);
			} else {
				System.err.println("Scanner is already initialized.");
			}
		}
	} // init

	public static Symbol next_token() throws IOException {
		if (lex == null) {
			System.err.println("Scanner is not yet initialized.");
			System.exit(-1);
		} else {
			return lex.yylex();
		}
		return null; // To appease javac
	} // next_token

	private Scanner() {
		// no instances
	}
} // class Scanner

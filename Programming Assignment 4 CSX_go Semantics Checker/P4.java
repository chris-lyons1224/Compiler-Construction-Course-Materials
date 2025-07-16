import java_cup.runtime.Symbol;
import java.io.Reader;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.FileNotFoundException;

final class P4 {

	public static void main(String args[]) throws java.io.IOException {

		if (args.length != 1) {
			System.out.println("Error: Input file must be named on command line." );
			System.exit(-1);
		}

		java.io.Reader yyin = null;

		try {
			yyin = new BufferedReader(new FileReader(args[0]));
		} catch (FileNotFoundException notFound){
			System.out.print("Error: unable to open input file: ");
			System.out.println(args[0]);
			System.exit(1); // 1 means failure
		}

		Scanner.init(yyin); // Initialize Scanner class for parser
		final parser csxParser = new parser();
		System.out.println ("\n\n" + "Begin CSX_go compilation of " +
			args[0] + "\n");
		Symbol root=null;
		try {
			root = csxParser.parse(); // do the parse
			System.out.println ("CSX_go program parsed correctly.");
		} catch (Exception e) { // parser can throw generic Exception
			System.out.println ("Compilation terminated due to syntax errors.");
			return;
		}
		// ((ProgramNode)root.value).unparse(0); // DEBUG 
		if (((ProgramNode)root.value).isSemanticsCorrect()) {
			System.out.println("No CSX_go semantics errors detected.");
		} else {
			System.out.println("\nCSX_go compilation halted due to semantics errors.");
		}
	} // main

	private P4() {
		// no instances allowed.
	}

} // class P4

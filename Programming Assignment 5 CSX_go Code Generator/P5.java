import java_cup.runtime.Symbol;
import java.io.Reader;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.FileNotFoundException;

final class P5 {

	public static void
	main(String args[]) throws java.io.IOException {

		if (args.length != 1) {
			System.out.println("Error: Input file must be named on command line." );
			System.exit(-1);
		}

		Reader yyin = null;

		try {
			yyin = new BufferedReader(new FileReader(args[0]));
		} catch (FileNotFoundException notFound){
			System.out.print("Error: unable to open input file: ");
			System.out.println(args[0]);
			System.exit(1); // 1 means failure
		}

		Scanner.init(yyin); // Initialize Scanner class for parser
		final parser csxParser = new parser();
		System.out.println ("\n" + "Begin CSX_go compilation of " + args[0] +
			'\n');
		Symbol root=null;
		try {
			root = csxParser.parse(); // do the parse
			System.out.println ("CSX_go program parsed correctly.");
		} catch (Exception e) { // parser can throw generic Exception
			System.out.println ("Compilation terminated due to syntax errors.");
			System.exit(1);
		}
		// ((ProgramNode)root.value).unparse(0); // DEBUG 
		if (((ProgramNode)root.value).isSemanticsCorrect()) {
			System.out.println("No CSX_go semantic errors detected.");
		} else {
			System.out.println("\nCSX_go compilation halted due to semantics errors.");
			System.exit(1);
		}
		java.io.PrintStream outFile = null;
		final String outFileName =
			((ProgramNode)root.value).name.identifierText + ".j";
		try {
			outFile = new java.io.PrintStream(
				new java.io.FileOutputStream(outFileName));
		} catch (FileNotFoundException notFound){
			System.out.println ("Error: unable to open output file " +
				outFileName);
			System.exit(1);
		}
		if (((ProgramNode)root.value).codeGen(outFile)) {
			System.out.println ("Program translated; result is in " + outFileName);
		} else {
			System.out.println ("Error in translating CSX_go program");
			System.exit(1);
		}
	} // main

	private P5() {
		// no instances allowed
	} 

} // class P5

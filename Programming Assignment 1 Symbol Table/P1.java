import java.util.NoSuchElementException;
import java.util.InputMismatchException;
import java.util.Locale;

final class P1 {

	public static void main(String args[]){
		printInteractive(
			"Project 1 test driver. Enter any of the following commands:\n"+
			"  (Command prefixes are allowed)\n"+
			"\tOpen (a new scope)\n"+
			"\tClose (innermost current scope)\n"+
			"\tInsert (key, value pair (strings) into symbol table)\n"+
			"\tLookup (look symbol up in top scope)\n"+
			"\tGlobal (look symbol up in entire symbol table)\n" +
			"\tDump (contents of symbol table)\n"+
			"\tQuit (test driver)"
		);
		final SymbolTable ST = new SymbolTable();
		final java.util.Scanner inputScanner = new java.util.Scanner(System.in);
		while (true) {
			try {
				String command;
				String name; // used for some commands
				String value; // used for some commands
				TestSym sym; // used for some commands
				command = inputScanner.next();
				if (command == null) {
					break;
				}
				printBatch(command);
				command = command.toLowerCase(Locale.US);
				if ("open".startsWith(command)) {
					ST.openScope();
					System.out.println("New scope opened.");
				} else if ("close".startsWith(command)) {
					ST.closeScope();
					System.out.println("Top scope closed.");
				} else if ("dump".startsWith(command)) {
					ST.dump(System.out);
				} else if ("insert".startsWith(command)) {
					System.out.print("Enter symbol: ");
					name = inputScanner.next();
					printBatch(name);
					System.out.print("Enter associated value: ");
					value = inputScanner.next();
					printBatch(value);
					sym = new TestSym(name, value);
					ST.insert(sym);
					System.out.println(sym + " entered into symbol table.");
				} else if ("lookup".startsWith(command)) {
					System.out.print("Enter symbol: ");
					name = inputScanner.next();
					printBatch(name);
					sym = (TestSym) ST.localLookup(name);
					if (sym == null) {
						System.out.println(name + " not found in top scope.");
					} else {
						System.out.println(sym + " found in top scope");
					}
				} else if ("global".startsWith(command)) {
					System.out.print("Enter symbol: ");
					name = inputScanner.next();
					printBatch(name);
					sym = (TestSym) ST.globalLookup(name);
					if (sym == null) {
						System.out.println(name + " not found in symbol table");
					} else {
						System.out.println(sym + " found in symbol table.");
					}
				} else if ("quit".startsWith(command)) {
					System.out.println("Testing done");
					break;
				} else {
					System.out.println("invalid command: " + command);
				}
			} catch (InputMismatchException e) {
				System.out.println("Not an integer");
			} catch (NoSuchElementException e) {
				System.out.println("End of input");
				break; 
			} catch (Exception e) {
				System.out.println(e.getMessage());
			}	
		} // while(not eof)
	} // main()

	private static final boolean isBatch = (System.console() == null);

	// only print if non-interactive
	private static void printBatch(String message) {
		if (isBatch) {
			System.out.print(message + "\n");
		}
	} // printBatch(String)

	// only print if interactive
	private static void printInteractive(String message) {
		if (!isBatch) {
			System.out.print(message + "\n");
		}
	} // printInteractive(String)

	private P1() {
		// empty constructor
	}

} // class P1

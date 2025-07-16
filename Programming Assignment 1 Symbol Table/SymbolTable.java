import java.io.PrintStream;
import java.util.LinkedList;
import java.util.HashMap;

class SymbolTable {

	// We assume that symbols to be entered/retrieved are already in
	// the proper case.

	final LinkedList<HashMap<String, Symb>> tables =
		new LinkedList<HashMap<String, Symb>>();
	// the most recent is at the *front* to make it easy to search through all.

	SymbolTable() {
		// openScope(); // no open scope at start
	} // constructor SymbolTable

	public final void openScope() {
		tables.addFirst(new HashMap<String,Symb>());
	} // openScope()

	public void closeScope() throws EmptySTException {
		if (tables.isEmpty()) {
			throw new EmptySTException("Cannot close; no scopes are open.");
		}
		tables.remove();
	}

	public void insert(Symb symb) throws EmptySTException, DuplicateException {
		if (tables.isEmpty()) {
			throw new EmptySTException("Cannot insert; no scopes are open.");
		}
		if (tables.element().containsKey(symb.name())) {
			throw new DuplicateException("Attempt to insert duplicate symbol.");
		}
		tables.element().put(symb.name(), symb);
	} // insert(Symb)

	public Symb localLookup(String name) {
		if (tables.isEmpty()) {
			return null; // but no exception
		}
		return tables.element().get(name);
	} // localLookup(String)

	public Symb globalLookup(String name) {
		for (HashMap<String, Symb> map : tables) {
			final Symb result = map.get(name);
			if (result != null) {
				return(result);
			}
		}
		return null; // not found
	} // globalLookup(String)

	public String toString() {
		final StringBuffer result = new StringBuffer(50);
		result.append("Contents of symbol table:\n");
		for (HashMap<String, Symb> map : tables) {
			result.append('{');
			boolean first = true;
			for (Symb symb: map.values()) {
				if (first) {
					first = false;
				} else {
					result.append(", ");
				}
				result.append(symb.toString());
			} // each symb in the map
			result.append("}\n");
		} // each map in the symbol table
		return result.toString();
	} // globalLookup(String)

	void dump(PrintStream ps) {
		ps.print(toString());
	} // dump(PrintStream)

} // class SymbolTable

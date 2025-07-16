import java.io.PrintStream;
import java.util.LinkedList;
import java.util.HashMap;

class SymbolTable {

	// We assume that symbols to be entered/retrieved are already in
	// the proper case.

	final LinkedList<HashMap<String, SymbolInfo>> tables =
		new LinkedList<HashMap<String, SymbolInfo>>();
	// the most recent is at the *front* to make it easy to search through all.
	// we really need LinkedList, not the more generic List, to get addFirst.

	SymbolTable() {
		openScope(); // first empty scope
	} // constructor SymbolTable

	public final void openScope() {
		tables.addFirst(new HashMap<String,SymbolInfo>());
	} // openScope()

	public void closeScope() throws EmptySTException {
		if (tables.isEmpty()) {
			throw new EmptySTException("Cannot close; no scopes remain.");
		}
		tables.remove();
	}

	public void insert(SymbolInfo symb)
			throws EmptySTException, DuplicateException {
		if (tables.isEmpty()) {
			throw new EmptySTException("Cannot insert; no scopes remain.");
		}
		if (tables.element().containsKey(symb.name())) {
			throw new DuplicateException("Attempt to insert duplicate symbol.");
		}
		tables.element().put(symb.name(), symb);
	} // insert(SymbolInfo)

	public SymbolInfo localLookup(String name) {
		if (tables.isEmpty()) {
			return null; // but no exception
		}
		return tables.element().get(name);
	} // localLookup(String)

	public SymbolInfo globalLookup(String name) {
		for (HashMap<String, SymbolInfo> map : tables) {
			final SymbolInfo result = map.get(name);
			if (result != null) {
				return(result);
			}
		}
		return null; // not found
	} // globalLookup(String)

	public String toString() {
		final StringBuffer result = new StringBuffer(50);
		result.append("Contents of symbol table:\n");
		for (HashMap<String, SymbolInfo> map : tables) {
			result.append('{');
			boolean first = true;
			for (SymbolInfo symb: map.values()) {
				if (first) {
					first = false;
				} else {
					result.append(", ");
				}
				result.append(symb.name()).append('=').append(symb.toString());
			} // each symb in the map
			result.append("}\n");
		} // each map in the symbol table
		return result.toString();
	} // globalLookup(String)

	public boolean topLevel() {
		return tables.size() <= 2;
	} // topLevel

	void dump(PrintStream ps) {
		ps.print(toString());
	} // dump(PrintStream)

} // class SymbolTable

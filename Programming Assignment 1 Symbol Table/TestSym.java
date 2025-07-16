class TestSym extends Symb {
	private final String value;

	TestSym(String name, String val) {
		super(name);
		value = val;
	} // constructor TestSym(String, int)

	public String value() {
		return value;
	} // value()

	public String toString() {
		return new StringBuilder().
			append('(').
			append(name()).
			append(':').
			append(value).
			append(')').
			toString();
	} // toString

} // class TestSym

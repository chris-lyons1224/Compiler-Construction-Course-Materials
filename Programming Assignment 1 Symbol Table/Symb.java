class Symb {
	private final String name;

	Symb(String aName) {
		name = aName;
	} // constructor Symb(String)

	public String name() {
		return name;
	} // name()

	public String toString() {
		return new StringBuilder().append('(').append(name).
			append(')').toString();
	} // toString()
	
} // class Symb

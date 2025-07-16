import java.util.Locale;

public enum KindEnum {

	VARIABLE, UNINITVAR, CONSTANT, FUNCTION, LABEL, CURRENTLABEL, VALUE;
	// LABEL names a loop, even after the loop is complete, because the LABEL
	// belongs to the outer scope.  There is a second symbol-table entry while
	// the loop is open for the CURRENTLABEL.

	public String toString() {
		return (super.toString().replace("current", "current "));
	}
};

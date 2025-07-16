import java.util.Locale;
public enum TypeEnum {
	INTEGER, INTEGERARRAY, CHAR, CHARARRAY, BOOL, BOOLARRAY, STRING, ERROR,
	VOID, FUNCTION; 

	static TypeEnum toType(String name) { // assume name is lower case
		switch (name) {
			case "int": return(INTEGER);
			case "char": return(CHAR);
			case "bool": return(BOOL);
			case "string": return(STRING);
			case "void": return(VOID);
			default: return(ERROR);
		}
	} // toType(String)

	public boolean notIn(TypeEnum[] valid) {
		for (TypeEnum type : valid) {
			if (type == this) {
				return(false);
			}
		}
		return(this != ERROR); // error is always "in"
	} // notIn

	public String toString() {
		return (super.toString().replace("array", " array").
			replace("integer", "int")
		);
	}

} // enum TypeEnum

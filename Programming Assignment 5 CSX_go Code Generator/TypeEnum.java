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

	String toJasminType() { // assume name is lower case
		String answer = null;
		switch (this) {
			case INTEGER:
				answer = "I"; break;
			case CHAR:
				answer = "C"; break;
			case BOOL:
				answer = "Z"; break;
			case STRING:
				answer = "[Ljava/lang/String"; break;
			case INTEGERARRAY:
				answer = "[I"; break;
			case CHARARRAY:
				answer = "[C"; break;
			case BOOLARRAY:
				answer = "[Z"; break;
			case VOID:
			answer = "V"; break;
			default: answer = "?"; break; // should not happen
		} // switch
		return answer;
	} // toJasminType()

	String toJasminCode() { // assume name is lower case
		switch (this) {
			case INTEGER:
			case CHAR:
			case BOOL:
				return("i");
			case STRING:
			case INTEGERARRAY:
			case CHARARRAY:
			case BOOLARRAY:
				return("a");
			default:
				return("?"); // should not happen
		} // switch
	} // toJasminCode()

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

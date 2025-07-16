import java.util.List;

/**************************************************
*  class used to hold information associated with
*  Symbs (which are stored in SymbolTables)
*
****************************************************/

class SymbolInfo extends Symb {
// TODO: refactor with subclasses for arrays, functions
	final public TypeEnum type;
	      public KindEnum kind; // might change from UNINITVAR to VARIABLE
	
	final public int arrayLength; // only for array types; 0 means formal param
	final public TypeEnum returnType; // only for functions
	final public List<TypeEnum> paramTypes; // only for functions

	public SymbolInfo(String id, KindEnum kind, TypeEnum type){
		super(id);
		this.kind = kind;
		this.type = type;
		this.arrayLength = 0; // unused
		returnType = TypeEnum.ERROR; // unused
		paramTypes = null; // unused
	}; // constructor SymbolInfo(id, KindEnum, TypeEnum)

	public SymbolInfo(String id, TypeEnum type, int arrayLength) {
		super(id);
		this.kind = KindEnum.VARIABLE;
		this.type = type;
		this.arrayLength = arrayLength;
		returnType = TypeEnum.ERROR; // unused
		paramTypes = null; // unused
	} // constructor SymbolInfo(String, type, arrayLength)

	public SymbolInfo(String id, TypeEnum returnType,
			List<TypeEnum> paramTypes) {
		super(id);
		this.kind = KindEnum.FUNCTION;
		this.type = TypeEnum.FUNCTION;
		this.arrayLength = 0; // unused
		this.returnType = returnType;
		this.paramTypes = paramTypes;
	} // constructor SymbolInfo(String, type, arrayLength)

	public String toString(){
		return new StringBuilder().
			append('(').
			append(name()).
			append(": kind=").
			append(kind).
			append(", type=").
			append(type).
			append(')').
			toString();
	};

} // SymbolInfo


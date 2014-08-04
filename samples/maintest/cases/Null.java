package maintest.cases;

public class Null {
//	@MayAliases(                              {"x.l -> null"})
	public static void setXLToNull(Tree x) { x.l =  null; }

//	@MayAliases({}) @Returns("null")
	public static Null returnNull() { return null; }


	////// Transfer ////////////////////////////////////////////////////////////
//	@MayAliases({"x.l -> null"})
	public static void setXLToNullTransfer(Tree x) { setXLToNull(x); }

//	@MayAliases({}) @Returns("null")
	public static Null returnNullTransfer() { return returnNull(); }
	

	///// Entry function ///////////////////////////////////////////////////////
	public static void doAnalysis() {
		Tree x = null;

        setXLToNull(x);
        returnNull();

        setXLToNullTransfer(x);
        returnNullTransfer();
	}
}

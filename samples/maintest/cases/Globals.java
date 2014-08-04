package maintest.cases;

public class Globals {
	static Tree global1, global2;

	public static Tree returnGlobal()        { return global1; }
	public static void setXLToGlobal(Tree x) { x.l = global1; }
	public static void setGlobalToX(Tree x)  { global1 = x; }
	public static void setGlobalLToX(Tree x) { global1.l = x; }
	public static void setGlobalOrGlobalLToX(Tree x) {
		if (x == null) global1.l = x;
		else           global1 = x;
    }

	////// Transfer ////////////////////////////////////////////////////////////
	
	public static Tree returnGlobalTransfer()        { return returnGlobal(); }
	public static void setXLToGlobalTransfer(Tree x) { setXLToGlobal(x); }
	public static void setGlobalToXTransfer(Tree x)  { setGlobalToX(x); }
	public static void setGlobalLToXTransfer(Tree x) { setGlobalLToX(x); }

	///// Entry function ///////////////////////////////////////////////////////
	public static void doAnalysis() {
		Tree x = null;
		
        setGlobalToX(x);
        setGlobalLToX(x);
        setXLToGlobal(x);
        returnGlobal();
        
        setGlobalToXTransfer(x);
        setGlobalLToXTransfer(x);
        setXLToGlobalTransfer(x);
        returnGlobalTransfer();
	}
}

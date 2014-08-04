package maintest.cases;

public class InterProc {
	////// Direct recursion ////////////////////////////////////////////////////
	public static Tree returnLMostRecursion(Tree x) {
		if (x.l == null) return x;
		return returnLMostRecursion(x.l);
	}

	////// Indirect recursion //////////////////////////////////////////////////
	public static Tree returnLRInTurnsLRecursion(Tree x) {
		if (x.l == null) return x;
		return returnLRInTurnsRRecursion(x.l);
	}

	public static Tree returnLRInTurnsRRecursion(Tree x) {
		if (x.r == null) return x;
		return returnLRInTurnsLRecursion(x.r);
	}
	
	////// Other ///////////////////////////////////////////////////////////////
	public static void setXRLToYTransfer(Tree x, Tree y)
	{ SimpleCases.setXLToY(x.r, y); }
	
	public static void setXLToYLTransfer(Tree x, Tree y)
	{ SimpleCases.setXLToY(x, y.l); }

	public static void resolveToAllocOrFormalTransfer(Tree x, Tree y) {
		if (x.l == null) x = new Tree(null, null);
		SimpleCases.setXLToY(x, y);
    }

	public static void resolveIndirectTransfer(Tree x1, Tree x2, Tree x3, Tree y) {
		if (x1.l == null) x1.l = x2;
		x1.l.l = x3;
		SimpleCases.setXLToY(x1.l.l, y);
    }

	public static void resolveIndirectTransfer2(Tree x1, Tree x2, Tree x3, Tree y) {
		if (x1.l == null) x1.l = x2;
		x1.l.l = x3;
		SimpleCases.setXLToY(x1.l, y);
    }

	public static Tree walkXLRLRMost(Tree x) {
		Tree xl = Loops.returnLMostLoop(x);
		Tree xlr = Loops.returnRMostLoop(xl);
		Tree xlrl = Loops.returnLMostLoop(xlr);
		Tree xlrlr = Loops.returnRMostLoop(xlrl);
		return xlrlr;
	}

	///// Entry function ///////////////////////////////////////////////////////
	public static void doAnalysis() {
		Tree x = new Tree(null, null);
		Tree y = new Tree(null, null);

        returnLMostRecursion(x);
        returnLRInTurnsLRecursion(x);
        returnLRInTurnsRRecursion(x);
        setXRLToYTransfer(x, y);
        setXLToYLTransfer(x, y);
        resolveToAllocOrFormalTransfer(x, y);
        resolveIndirectTransfer(x, x, x, y);
        resolveIndirectTransfer2(x, x, x, y);
        walkXLRLRMost(x);
	}

}

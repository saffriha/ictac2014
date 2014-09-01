package maintest.cases;

class ArrayWrapper { public Tree[] trees; }

public class Arrays {
	public static void intArrayCopyFst(int[] src, int[] tgt)
	{ tgt[0] = src[0]; }
	
	public static void treeArrayCopyFst(Tree[] src, Tree[] tgt)
	{ tgt[0] = src[0]; }

	public static void treeArrayCopyAllLoop(Tree[] src, Tree[] tgt) {
		for (int i = 0; i < tgt.length; ++i)
			tgt[i] = src[i];
    }
	
	public static void treeArrayPtrAssign(ArrayWrapper aw, Tree[] a)
	{ aw.trees = a; }
	
	public static Tree[] treeArrayPtrReturn(ArrayWrapper aw)
	{ return aw.trees; }

	////// Transfer ////////////////////////////////////////////////////////////
	public static void intArrayCopyFstTransfer(int[] src, int[] tgt)
	{ intArrayCopyFst(src, tgt); }
	
	public static void treeArrayCopyFstTransfer(Tree[] src, Tree[] tgt)
	{ treeArrayCopyFst(src, tgt); }

	public static void treeArrayCopyAllLoopTransfer(Tree[] src, Tree[] tgt)
	{ treeArrayCopyAllLoop(src, tgt); }
	

	///// Entry function ///////////////////////////////////////////////////////
	public static void doAnalysis() {
		ArrayWrapper aw = null;
		Tree[] ts = null;
		int[] is = null;
		
		intArrayCopyFst(is, is);
		treeArrayCopyFst(ts, ts);
		treeArrayCopyAllLoop(ts, ts);
		treeArrayPtrAssign(aw, ts);
		treeArrayPtrReturn(aw);

		intArrayCopyFstTransfer(is, is);
		treeArrayCopyFstTransfer(ts, ts);
		treeArrayCopyAllLoopTransfer(ts, ts);
	}
}

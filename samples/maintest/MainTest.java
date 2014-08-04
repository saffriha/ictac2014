package maintest;

import maintest.cases.Globals;
import maintest.cases.Inheritance;
import maintest.cases.InterProc;
import maintest.cases.Loops;
import maintest.cases.Null;
import maintest.cases.OOP;
import maintest.cases.SideEffects;
import maintest.cases.SimpleCases;

public class MainTest {
	public static void main(String[] args) {
		SimpleCases.doAnalysis();
		Loops.doAnalysis();
		Null.doAnalysis();
		Globals.doAnalysis();
		OOP.doAnalysis();
		Inheritance.doAnalysis();
		InterProc.doAnalysis();
		SideEffects.doAnalysis();
	}
	
}

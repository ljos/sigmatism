package test.java;

import sigmatism.java.Sigmatism;

public class Test  {

    public static void main(String [] args) throws Exception {
    	
    	System.out.println(Sigmatism.eval("(quote a)", "()"));
    	
    	System.out.println(Sigmatism.eval("(atom (quote a))", "()"));
    	System.out.println(Sigmatism.eval("(atom (quote (a b c)))", "()"));
    	
    	System.out.println(Sigmatism.eval("(eq (quote a) (quote a))", "()"));
    	System.out.println(Sigmatism.eval("(eq (quote a) (quote b))", "()"));
    	
    	System.out.println(Sigmatism.eval("(car (quote (a b c)))", "()"));
    	
    	System.out.println(Sigmatism.eval("(cdr (quote (a b c)))", "()"));
    	
    	System.out.println(Sigmatism.eval("(cons (quote a) (quote (b c)))", "()")); 
    	
    	System.out.println(Sigmatism.eval("(cond ((atom (quote a)) (quote perfect)))", "()"));
    	System.out.println(Sigmatism.eval("(cond ((atom (quote (a b))) (quote failed)) " +
    			                           "((eq (quote a) (quote a)) (quote perfect)))", "()")); 
    	
    	System.out.println(Sigmatism.eval("((lambda (test) (cons (quote a) test)) (quote (b c)))", "()")); 
    	System.out.println(Sigmatism.eval("((lambda (x y) (cons x (cdr y))) (quote z) (quote (a b c)))", "()"));
    }
}







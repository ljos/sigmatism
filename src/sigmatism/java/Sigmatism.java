package sigmatism.java;

import java.io.FileInputStream;
import java.util.Scanner;


public class Sigmatism {

	/**
	 * @param args
	 *            lisp source file.
	 *            lisp namespace file.
	 * 
	 * Sigmatism takes source file and and a namespace file with any 
	 * additional defined functions.
	 */
	public static void main(String[] args) throws Exception {
		StringBuilder file = new StringBuilder();
	    String NL = System.getProperty("line.separator");
	    try(Scanner scanner = new Scanner(new FileInputStream(args[0]))) {
	    	while (scanner.hasNextLine()){
	    		file.append(scanner.nextLine() + NL);
	    	}
	    }
	    
	    EXPR expr = read(file.toString());
	    
	    file = new StringBuilder();
	    try(Scanner scanner = new Scanner(new FileInputStream(args[0]))) {
	    	while (scanner.hasNextLine()){
	    		file.append(scanner.nextLine() + NL);
	    	}
	    }

		EXPR ns = read(file.toString());
		System.out.println(eval(expr, ns).toString());
	}
	
	private static EXPR assoc(EXPR symbol, EXPR ns) throws Exception {
		EXPR caar = ns.car().car();
		if(ns.equals(CONS.NIL)) {
			throw new Exception("Symbol not in namespace.");
		} else if (caar.equals(symbol)) {
			EXPR cadar = ns.car().cdr().car();
			return cadar;
		} else {
			return assoc(symbol, ns.cdr());
		}
	}
	
	private static EXPR evcon(EXPR cons, EXPR ns) throws Exception {
		EXPR caar = cons.car().car();
		EXPR cadar = cons.car().cdr().car();
		if(eval(caar, ns).equals(SYMBOL.T)) {
			return eval(cadar, ns);
		} else {
			return evcon(cons.cdr(), ns);
		}
	}
	
	private static EXPR evlis(EXPR expr, EXPR ns) throws Exception {
		if (expr.equals(CONS.NIL)) {
			return CONS.NIL;
		} else {
			return new CONS(eval(expr.car(), ns), evlis(expr.cdr(), ns));
		}
	}
	
	private static EXPR append(EXPR x, EXPR y) throws Exception {
		if (x.equals(CONS.NIL)) {
			return y;
		} else {
			return new CONS(x.car(), append(x.cdr(), y));
		}
	}
	
	private static EXPR pair(EXPR x, EXPR y) throws Exception {
		if (x.equals(CONS.NIL) && y.equals(CONS.NIL)) {
			return CONS.NIL;
		} else {
			EXPR list = new CONS(x.car(), new CONS(y.car(), CONS.NIL));
			return new CONS(list, pair(x.cdr(), y.cdr()));
		}
	}
	
	public static EXPR eval(String expr, String ns) throws Exception {
		EXPR exp = read(expr);
		EXPR n = read(ns);
		return eval(exp, n);
	}
	

	private static EXPR eval(EXPR expr, EXPR ns) throws Exception {
		if (expr instanceof SYMBOL) {
			return assoc(expr, ns);
		} else if (expr instanceof CONS) {
			if (expr.car() instanceof SYMBOL) {
				String symbol = expr.car().toString();
				EXPR cadr = expr.cdr().car();
				EXPR caddr = expr.cdr().cdr().car();
				switch (symbol) {
				case "quote":
					return cadr;
				case "atom":
					return eval(cadr, ns) instanceof SYMBOL ? 
							SYMBOL.T : CONS.NIL; 
				case "eq":
					return eval(cadr, ns).equals(eval(caddr, ns)) ? 
							SYMBOL.T : CONS.NIL;
				case "car":
					return eval(cadr, ns).car();
				case "cdr":
					return eval(cadr, ns).cdr();
				case "cons":
					return new CONS(eval(cadr, ns), eval(caddr,ns));
				case "cond":
					return evcon(expr.cdr(), ns);
				default:
					EXPR f = assoc(expr.car(), ns);
					return eval(new CONS(f, expr.cdr()), ns);
				}
			} else {
				EXPR caar = expr.car().car();
				EXPR caddar = expr.car().cdr().cdr().car();
				EXPR cadar = expr.car().cdr().car();
				
				if (caar.equals(SYMBOL.label)) {
					CONS list = new CONS(cadar, new CONS(expr.car(), CONS.NIL)); 
					return eval(new CONS(caddar, expr.cdr()), new CONS(list, ns));
				} else if (caar.equals(SYMBOL.lambda)) {
					EXPR pair = pair(cadar, evlis(expr.cdr(), ns));
					EXPR append = append(pair, ns);
					return eval(caddar, append);
				}
			}
		}

		throw new Exception("Unknown input.");
	}
	
	private static String symbol = "[a-z]+[a-z0-9_-]*";
//	private static String cons = "^\\(.*\\)$";
	private static String nil = "\\(\\)";

	private static EXPR read(String source) {
		source = source.replaceAll("\\s+", " ").trim();
		if(source.matches(nil) || source.isEmpty()) {
			return CONS.NIL;
		}
		if (source.matches(symbol)) {
			return new SYMBOL(source);
		}
		if (source.startsWith("(")) {
			int i = 1;
			for (int j = 1; j > 0; ++i) {
				if (source.charAt(i) == '(') {
					++j;
				} else if (source.charAt(i) == ')') {
					--j;
				}
			}
			
			if (i == source.length()) {
				return read(source.replaceFirst("^\\((.*)\\)", "$1 ()"));
			}
			EXPR car = read(source.substring(0, i));
			EXPR cdr = read(source.substring(i));
			return new CONS(car, cdr);
		}
		EXPR car = read(source.replaceFirst("("+symbol+").+", "$1"));
		EXPR cdr = read(source.replaceFirst(symbol+"\\s(.+)", "$1"));
		return new CONS(car, cdr);
	}

	private static abstract class EXPR {
		public EXPR car() throws Exception {
			throw new Exception("Cannot call car on EXPR that is not a CONS: " + this.toString());
		}
		
		public EXPR cdr() throws Exception {
			throw new Exception("Cannot call cdr on EXPR that is not a CONS: " + this.toString());
		}
	}

	private static class SYMBOL extends EXPR {
		public final String symbol;
		
		public final static SYMBOL T = new SYMBOL("t"); 
		public final static SYMBOL label = new SYMBOL("label");
		public final static SYMBOL lambda = new SYMBOL("lambda");

		public SYMBOL(String s) {
			symbol = s.trim();
		}

		public String toString() {
			return symbol;
		}

		@Override
		public boolean equals(Object o) {
			if (o instanceof SYMBOL) {
				SYMBOL s = (SYMBOL) o;
				return this.symbol.equals(s.symbol);
			} else {
				return false;
			}
		}
	}

	private static class CONS extends EXPR {

		public final static CONS NIL = new CONS(null, null);

		private final EXPR car;
		private final EXPR cdr;

		public CONS(EXPR head, EXPR rest) {
			this.car = head;
			this.cdr = rest;
		}
		
		
		public EXPR car() {
			return car;
		}
		
		public EXPR cdr() {
			return cdr;
		}

		public String toString() {
			if (this.equals(NIL)) {
				return "()";
			}
			String s = "(";

			s += car.toString() + " ";
			s += cdr.toString().replaceFirst("^\\((.*)\\)", "$1");
			s = s.trim() + ")";
			return s;
		}
		
		@Override
		public boolean equals(Object o) {
			if (o instanceof CONS) {
				CONS s = (CONS) o;
				return this.car == null 
						&& this.cdr == null 
						&& s.car == null 
						&& s.cdr == null;
			} else {
				return false;
			}
		}
	}
}

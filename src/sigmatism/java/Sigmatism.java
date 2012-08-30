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

		CONS ns = (CONS)read(file.toString());
		System.out.println(eval(expr, ns).toString());
	}
	
	private static EXPR assoc(SYMBOL symbol, CONS ns) throws Exception {
		EXPR caar = ((CONS)ns.head).head;
		if(ns.equals(CONS.NIL)) {
			throw new Exception("Symbol not in namespace.");
		} else if (caar.equals(symbol)) {
			EXPR cadar = ((CONS)((CONS)ns.head).rest).head;
			return cadar;
		} else {
			return assoc(symbol, (CONS)ns.rest);
		}
	}
	
	private static EXPR evcon(CONS cons, CONS ns) throws Exception {
		EXPR caar = ((CONS)cons.head).head;
		EXPR cadar = ((CONS)((CONS)cons.head).rest).head;
		if(eval(caar, ns).equals(SYMBOL.T)) {
			return eval(cadar, ns);
		} else {
			return evcon((CONS)cons.rest, ns);
		}
	}
	
	private static CONS evlis(CONS expr, CONS ns) throws Exception {
		if (expr.equals(CONS.NIL)) {
			return CONS.NIL;
		} else {
			return new CONS(eval(expr.head, ns), evlis((CONS)expr.rest, ns));
		}
	}
	
	private static CONS append(CONS x, CONS y) {
		if (x.equals(CONS.NIL)) {
			return y;
		} else {
			return new CONS(x.head, append((CONS)x.rest, y));
		}
	}
	
	private static CONS pair(CONS x, CONS y) {
		if (x.equals(CONS.NIL) && y.equals(CONS.NIL)) {
			return CONS.NIL;
		} else {
			EXPR list = new CONS(x.head, new CONS(y.head, CONS.NIL));
			return new CONS(list, pair((CONS)x.rest, (CONS)y.rest));
		}
	}
	
	public static EXPR eval(String expr, String ns) throws Exception {
		EXPR exp = read(expr);
		CONS n = (CONS)read(ns);
		return eval(exp, n);
	}
	

	private static EXPR eval(EXPR expr, CONS ns) throws Exception {
		if (expr instanceof SYMBOL) {
			SYMBOL e = (SYMBOL)expr;
			return assoc(e, ns);
		} else if (expr instanceof CONS) {
			CONS cons = (CONS) expr;
			if (cons.head instanceof SYMBOL) {
				String symbol = cons.head.toString();
				EXPR cadr = ((CONS)cons.rest).head;
				EXPR caddr = ((CONS)((CONS)cons.rest).rest).head;
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
					return ((CONS)eval(cadr, ns)).head;
				case "cdr":
					return ((CONS)eval(cadr, ns)).rest;
				case "cons":
					return new CONS(eval(cadr, ns), eval(caddr,ns));
				case "cond":
					return evcon((CONS)cons.rest, ns);
				default:
					EXPR f = assoc((SYMBOL)cons.head, ns);
					return eval(new CONS(f, cons.rest), ns);
				}
			} else {
				EXPR caar = ((SYMBOL)((CONS)cons.head).head);
				EXPR caddar = ((CONS)((CONS)((CONS)cons.head).rest).rest).head;
				EXPR cadar = ((CONS)((CONS)cons.head).rest).head;
				
				if (caar.equals(SYMBOL.label)) {
					CONS list = new CONS(cadar, new CONS(cons.head, CONS.NIL)); 
					return eval(new CONS(caddar, cons.rest), new CONS(list, ns));
				} else if (caar.equals(SYMBOL.lambda)) {
					CONS pair = pair((CONS)cadar, evlis((CONS)cons.rest, ns));
					CONS append = append(pair, ns);
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

		EXPR head;
		EXPR rest;

		public CONS(EXPR head, EXPR rest) {
			this.head = head;
			this.rest = rest;
		}

		public String toString() {
			if (head == null && rest == null) {
				return "()";
			}
			String s = "(";

			s += head.toString() + " ";
			s += rest.toString().replaceFirst("^\\((.*)\\)", "$1");
			s = s.trim() + ")";
			return s;
		}
		
		@Override
		public boolean equals(Object o) {
			if (o instanceof CONS) {
				CONS s = (CONS) o;
				return this.head == null 
						&& this.rest == null 
						&& s.head == null 
						&& s.rest == null;
			} else {
				return false;
			}
		}
	}
}

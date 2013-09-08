package sigmatism.java;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Stack;

public class Sigmatism {

    public static Object read(Reader reader) throws IOException {
	int c = reader.read();
	Stack<List<Object>> depth = new Stack<>();
	Object expr = null;

	StringBuilder stringBuilder = new StringBuilder();
	while (c != -1) {
	    switch (c) {
	    case '(':
		List<Object> l = new LinkedList<>();
		List<Object> current;
		if (!depth.isEmpty()) {
		    current = depth.peek();
		    current.add(l);
		} else {
		    current = l;
		}

		if (stringBuilder.length() > 0) {
		    current.add(stringBuilder.toString());
		    stringBuilder = new StringBuilder();
		}
		depth.push(l);
		break;
	    case ')':
		List<Object> list = depth.pop();
		if (stringBuilder.length() > 0) {
		    list.add(stringBuilder.toString());
		    stringBuilder = new StringBuilder();
		}
		expr = list;
		break;
	    case ' ':
		if (depth.empty()) {
		    break;
		}
		if (stringBuilder.length() > 0) {
		    depth.peek().add(stringBuilder.toString());
		    stringBuilder = new StringBuilder();
		}
		break;
	    default:
		stringBuilder.append((char) c);
	    }
	    c = reader.read();
	}

	if (stringBuilder.length() > 0) {
	    return stringBuilder.toString();
	} else {
	    return expr;
	}
    }

    @SuppressWarnings("unchecked")
    public static Object eval(Object e, Map<String, Object> ns) {
	if (e instanceof String) {
	    String s = (String) e;
	    return ns.get(s);
	} else if (e instanceof List) {
	    List<Object> list = (List<Object>) e;
	    Object first = list.get(0);

	    Object arg1;
	    Object arg2;

	    if (first instanceof String) {
		String symbol = (String) first;
		switch (symbol) {
		case "quote":
		    return list.get(1);
		case "atom":
		    Object ans = eval(list.get(1), ns);
		    return ans instanceof String ? "t" : "nil";
		case "eq":
		    arg1 = eval(list.get(1), ns);
		    arg2 = eval(list.get(2), ns);
		    if (arg1 instanceof String) {
			return arg1.equals(arg2) ? "t" : "nil";
		    }
		    return "nil";
		case "car":
		    arg1 = eval(list.get(1), ns);
		    return ((List<Object>) arg1).get(0);
		case "cdr":
		    arg1 = eval(list.get(1), ns);

		    List<Object> cons = (List<Object>) arg1;
		    cons.remove(0);
		    return cons;
		case "cons":
		    List<Object> c = new LinkedList<>();
		    c.add(eval(list.get(1), ns));
		    arg2 = eval(list.get(2), ns);
		    if (arg2 instanceof List) {
			c.addAll((List<Object>)arg2);
		    } else {
			c.add(arg2);
		    }
		    return c;
		case "cond":
		    List<Object> args = new LinkedList<>(list);
		    args.remove(0);
		    for (Object o : args) {
			List<Object> con = (List<Object>) o;
			if (!eval(con.get(0), ns).equals("nil")) {
			    return eval(con.get(1), ns);
			}
		    }
		    break;
		default:
		    return eval(list.set(0, ns.get(first)), ns);
		}
	    } else if (first instanceof List) {
		List<Object> fun = (List<Object>) first;

		String symbol = (String) fun.get(0);
		if (symbol.equals("label")) {
		    String name = (String) fun.get(1);
		    Object body = fun.get(2);
		    Map<String, Object> ns2 = new HashMap<>(ns);
		    ns2.put(name, fun);
		    List<Object> l = new LinkedList<Object>();
		    l.add(body);
		    List<Object> args = list.subList(1, list.size());
		    l.addAll(args);
		    return eval(l, ns2);
		} else if (symbol.equals("lambda")) {
		    List<Object> largs = (List<Object>) fun.get(1);
		    List<Object> args = list.subList(1, list.size());

		    Map<String, Object> ns2 = new HashMap<>(ns);
		    for(int i = 0; i < args.size(); i++) {
			Object arg = args.get(i);
			arg = eval(arg, ns);
			String larg = (String)largs.get(0);
			ns2.put(larg, arg);
		    }
		    
		    return eval(fun.get(2), ns2);
		}
	    }
	}

	return null;
    }
    
    public static void print(Object e) {
	if (e instanceof String) {
	    System.out.print(e);
	} else {
	    @SuppressWarnings("unchecked")
	    List<Object> list = (List<Object>) e;
	    System.out.print("(");
	    print(list.get(0));
	    list.remove(0);
	    for(Object o : list) {
		System.out.print(" ");
		print(o);
	    }
	    System.out.print(")");
	}
    }

    public static void repl() throws IOException {
	@SuppressWarnings("resource")
	Scanner scanner =  new Scanner(System.in);
	for(;;) {
	    System.out.print("lambda> ");
	    System.out.flush();
	    String input = scanner.nextLine();
	    Object read = read(new StringReader(input));
	    Object eval = eval(read, new HashMap<String, Object>());
	    print(eval);
	    System.out.println();
	    System.out.flush();
	}
    }

    public static void main(String[] args) throws IOException {
	repl();
    }

}

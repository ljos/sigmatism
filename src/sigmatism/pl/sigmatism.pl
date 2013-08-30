char(C, [C|A], A) :-
    char_type(C, alnum); char_type(C, period); char_type(C, quote).

chars([C]) --> char(C).
chars([C|Cs]) --> char(C), chars(Cs).

symbol(Symbol, A, R) :-
    append(S, R, A),
    phrase(chars(Cs), S),
    atom_chars(Symbol, Cs).

rest([C]) --> cons(C).
rest([S]) --> symbol(S).
rest([C | R]) --> cons(C), [' '], rest(R).
rest([S1 | S2]) --> symbol(S1), [' '] , rest(S2).

cons([]) --> ['(',')'].
cons(Elems) --> ['('], rest(Elems), [')'].

read_lisp(String, AST) :-
    phrase((cons(AST) ; symbol(AST)), String).

evcon([[F, S] | _], Ns, Value) :-
    eval_lisp(F, Ns, B),
    B \= nil,
    eval_lisp(S, Ns, Value).
evcon([_ | R], Ns, Value) :-
    evcon(R, Ns, Value).

evlis([], _, []).
evlis([Arg | Args], Ns, [Value | R]) :-
    eval_lisp(Arg, Ns, Value),
    evlis(Args, Ns, R).

pair([], _, Ns, Ns).
pair([Key | Keys], [Arg | Args], Ns, Namespace) :-
    put_assoc(Key, Ns, Arg, Assoc),
    pair(Keys, Args, Assoc, Namespace).

eval_lisp(AST, Ns, Value) :-
    atom(AST),
    get_assoc(AST, Ns, Value).
eval_lisp([quote, Q], _, Q).
eval_lisp([atom, A], Ns, t) :-
    eval_lisp(A, Ns, Value),
    atom(Value).
eval_lisp([atom | _], _, nil).
eval_lisp([eq, F, S], Ns, t) :-
    eval_lisp(F, Ns, VF),
    eval_lisp(S, Ns, VS),
    atom(VF),
    atom(VS),
    VF = VS.
eval_lisp([eq | _], _, nil).
eval_lisp([car, F], Ns, Value) :-
    eval_lisp(F, Ns, [Value | _]).
eval_lisp([cdr, F], Ns, Value) :-
    eval_lisp(F, Ns, [_ | Value]).
eval_lisp([cons, F, S], Ns, [V1 | V2]) :-
    eval_lisp(F, Ns, V1),
    eval_lisp(S, Ns, V2).
eval_lisp([cond | R], Ns, Value) :-
    evcon(R, Ns, Value).
eval_lisp([[label, Name, Lambda] | Args], Ns, Value) :-
    put_assoc(Name, Ns, [label, Name | Lambda], Assoc),
    eval_lisp([Lambda | Args], Assoc, Value).
eval_lisp([[lambda, LArgs, Body] | Rest], Ns, Value) :-
    evlis(Rest, Ns, Args),
    pair(LArgs, Args, Ns, Namespace),
    eval_lisp(Body, Namespace, Value).
eval_lisp([H | T], Ns, Value) :-
    get_assoc(H, Ns, E),
    eval_lisp([E | T], Ns, Value).

insert_space([H], [H]).
insert_space([H | T], [H, ' ' | R]) :-
    insert_space(T, R).

writeln_lisp(L) :-
    write_lisp(L), nl.
write_lisp(A) :-
    atom(A),
    write(A).
write_lisp(L) :-
    write('('),
    insert_space(L, P),
    maplist(write_lisp, P),
    write(')').

repl :-
    prompt1('lambda> '),
    current_input(Stream),
    read_line_to_codes(Stream, String),
    maplist(char_code, Chars, String),
    read_lisp(Chars, AST),
    list_to_assoc([], Namespace),
    eval_lisp(AST, Namespace, V),
    writeln_lisp(V),
    repl.

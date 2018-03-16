
% partial order

cnf(bot, axiom, leq(bot,X)).

cnf(trans, axiom, ~leq(X,Y) | ~leq(Y,Z) | leq(X,Z)).

cnf(refl, axiom, leq(X,X)).

cnf(antisymm, axiom, ~leq(X,Y) | ~leq(Y,X) | X=Y).

cnf(mono_g, axiom, ~leq(X,Y) | leq(g(X),g(Y))).

cnf(mono_h, axiom, ~leq(X,Y) | leq(h(X),h(Y))).

cnf(mono_f, axiom, ~leq(X,Y) | ~leq(V,W) | leq(f(X,V),f(Y,W))).

% def xi

cnf(x0, axiom, x(t0)   = bot).

cnf(xs, axiom, x(s(T)) = h(g(x(T)))).

% def ai, 1st version

cnf(a1, axiom, a1(T) = f(h(g(x(T))),g(x(T)))).

% def ai, 2nd version

cnf(a2, axiom, a2(T) = f(g(x(T)),g(x(T)))).

% time

%cnf(time1, axiom, p(s(T)) = T | T = tk).

cnf(time2, axiom, tk != t0).

% properties

cnf(prop0, axiom, x(p(tk))   = x(tk)).  % fixpoint reached in x

cnf(prop1, axiom, a1(p(tk))  = a1(tk)). % no fixpoint reached yet

cnf(prop1, axiom, a2(p(tk))  = a2(tk)). % no fixpoint reached yet


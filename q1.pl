% Order of precedence
:- op(800, xfy, [&]). % Conjunction
:- op(850, xfy, [v]). % Disjunction
:- op(870, xfy, [=>]). % Implication
:- op(880, xfy, [<=>]). % Equivalence

% ------------------------ satisfies(I,F) predicate ----------------------------
% satisfies(I,F) hols if and only if interpretation I satisfies the propositional logic formula F (i.e I makes F true at all times).

satisfies([X], X).  % base-case: if X is the only element in the list.
satisfies(I, X) :-
    atom(X), member(X, I).  % recurisve-case: if X is an atom and X is a member of the list, return true.

satisfies(I, -(X)) :-
    not(atom(X)), !, elimImpl(-X, R1), nnf(R1, R2), cnf(R2, R3), satisfies(I, R3).  % if X is not an atom, recurisvely call elimImpl(-X, R1), nnf(R1, R2), cnf(R2, R3) and then call satisfies(I, R3).

% OR RULE (v)
satisfies([X], X v _).  % base-case: X OR anything returns TRUE.

satisfies(I, X v Y) :-
    (atom(X), atom(Y)), !, (member(X, I); member(Y, I)).    % base-case: if X and Y are prolog atoms, then check if either X or Y is a member of I.
satisfies(I, X v Y) :-
    satisfies(I, X); satisfies(I, Y).   % recurisve-case: if X and Y are not prolog atoms, then recurisvely call satisfies(I, X) and satisfies(I, Y).

satisfies(I, -X v -Y) :-
    (atom(X), atom(Y)), !, (member(-X, I); member(-Y, I)).  % base-case: if X and Y are prolog atoms, then check if either -X or -Y is a member of I.
satisfies(I, -X v -Y) :-
    satisfies(I, -X); satisfies(I, -Y). % recurisve-case: if X and Y are not prolog atoms, then recurisvely call satisfies(I, -X) and satisfies(I, -Y).

% AND RULE (&)
satisfies([X], X & X).  % base-case: X AND X returns TRUE.

satisfies(I, X & Y) :-
    (atom(X)), !, member(X, I), satisfies(I, Y).    % recurisve-case: if X is a prolog atom, check if X is the member of I and then recurisvely call satisfies(I, Y).
satisfies(I, X & Y) :-
    satisfies(I, X), satisfies(I, Y).   % recurisve-case: if X is not a prolog atom, recurisvely call satisfies(I, X) and satisfies(I, Y).

satisfies(I, -X & -Y) :-
    (atom(X)), !, member(X, I), satisfies(I, -Y).   % recurisve-case: if X is a prolog atom, check if X is the member of I and then recurisvely call satisfies(I, -Y).
satisfies(I, -X & -Y) :-
    satisfies(I, -X), satisfies(I, -Y).  % recurisve-case: if X is not a prolog atom, recurisvely call satisfies(I, -X) and satisfies(I, -Y).

% IMPLICATION RULE (=>)
satisfies([X], _ => X). % base-case: anything IMPLIES X is TRUE.

satisfies(I, X => Y) :-
    satisfies(I, -X v Y).   % recurisve-case: by definition of X => Y, we know -X v Y. So call satisfies(I, -X v Y).

% DOUBLE IMPLICATION RULE (<=>)
satisfies([X], X <=> X).    % base-case: (X IMPLIES X) AND (X IMPLIES X) is TRUE.

satisfies(I, X <=> Y) :-
    satisfies(I, X => Y), satisfies(I, Y => X). % recurisve-case: by definition of X <=> Y, we know X <=> Y. So call satisfies(I, X => Y) and satisfies(I, Y => X).

% ------------------------ elimImpl(F,R) predicate -----------------------------
% elimImpl(F, R) hols if R is the result of replacing all implications and double implications in a propositional logic formula by their definition.
% Definition of (X => Y) = (-X v Y)
% Definition of (X <=> Y) = (X => Y & Y => X) 

elimImpl(F, R) :-
    atom(F), R = F. % base-case: if F is an atom, set R to be F.

elimImpl(-F, R) :-
    atom(F), !, R = -F. % base-case: if F is an atom then -F is also an atom.
elimImpl(-F, R) :-
    elimImpl(F, R1), R = -R1.   % base-case: if F is not an atom, but has a negation symbol in front, compute elimImpl(F, R1) and R = -R1.

elimImpl(X => Y, R) :-
    (atom(X), atom(Y)), !, R = (-(X) v Y).  % base-case: if X and Y are both atom, set R to be the definition of =>
elimImpl(X <=> Y, R) :-
    (atom(X), atom(Y)), !, R = ((-(X) v Y) & (-(Y) v X)).   % base-case: if X and Y are both atom, set R to be the definition of <=>

elimImpl(X => Y, R) :-
    elimImpl(X, R1), elimImpl(Y, R2), R = (-(R1) v R2).   % recurisve case when two formulas are separated by =>
elimImpl(X <=> Y, R) :-
    elimImpl(X => Y, R1), elimImpl(Y => X, R2), R = (R1 & R2).   % recurisve case when two formulas are separated by <=>

elimImpl(X & Y, R) :-
    elimImpl(X, R1), elimImpl(Y, R2), R = (R1 & R2).    % recurisve case when two formulas are separated by &
elimImpl(X v Y, R) :-
    elimImpl(X, R1), elimImpl(Y, R2), R = (R1 v R2).    % recurisve case when two formulas are separated by v

% -------------------------- nnf(F,R) predicate --------------------------------
% nnf(F, R) hols if R is the result of putting propositional logic formula F in negation normal form.
% A propositional logic formula is in negation normal form if negation only appears in front of propositional variables and there are no nested negations.

nnf(F, R) :-
    atom(F), R = F.	% base-case: if F is an atom, return R as F.
nnf(-F, R) :-
    atom(F), R = -F. % base-case: if F is an atom with signle negation, return R as -F.

nnf(- -F, R) :-
	atom(F), !, R = F.	% base-case: if F is an atom with double negation, return R as F using double negation law.
nnf(- -F, R) :-
	nnf(-F, R1), nnf(-R1, R2), R2 = R.	% recursive-case: if F is not an atom with double negation, compute nnf(-F, R1) and then use calculate nnf(-R1, R2) and return R as R2.

nnf(-(X & Y), R) :-
    (atom(X), atom(Y)), !, R = (-X v -Y).   % base-case: if X and Y are both prolog atoms, then compute R as -X v -Y using de-morgans law.
nnf(-(X & Y), R) :-
    nnf(-X, R1), nnf(-Y, R2), R = (R1 v R2).    % recursive-case: if X and Y are not prolog atoms, then compute nnf(-X, R1) and nnf(-Y, R2) and return R as R1 v R2.

nnf(-(X v Y), R) :-
    (atom(X), atom(Y)), !, R = (-X & -Y).   % base-case: if X and Y are both prolog atoms, then compute R as -X & -Y using de-morgans law.
nnf(-(X v Y), R) :-
    nnf(-X, R1), nnf(-Y, R2), R = (R1 & R2).    % recursive-case: if X and Y are not prolog atoms, then compute nnf(-X, R1) and nnf(-Y, R2) and return R as R1 & R2.

nnf(X & Y, R) :-
    (atom(X), atom(Y)), !, R = (X & Y). % base-case: if X and Y are both prolog atoms, then return R as X & Y.
nnf(X & Y, R) :-
    nnf(X, R1), nnf(Y, R2), R = (R1 & R2).  % recursive-case: if X and Y are not prolog atoms, then computer nnf(X, R1) and nnf(Y, R1) and return R as R1 & R2.

nnf(X v Y, R) :-
    (atom(X), atom(Y)), !, R = (X v Y). % base-case: if X and Y are both prolog atoms, then return R as X v Y.
nnf(X v Y, R) :-
    nnf(X, R1), nnf(Y, R2), R = (R1 v R2).  % recursive-case: if X and Y are not prolog atoms, then computer nnf(X, R1) and nnf(Y, R1) and return R as R1 v R2.

	
% -------------------------- cnf(F,R) predicate --------------------------------
% cnf(F, R) hols if R is the result of putting propositional logic formula F in conjunctive normal form.
% A propositional logic formula is in conjunctive normal form if it is a conjunction of disjunctions of literals, where a literal is a propositional variable or its negation.

cnf(F, R) :-
	atom(F), !, R = F.  % base-case: if F is a prolog atom, return R as F.

cnf(-F, R) :-
	atom(F), !, R = -F. % base-case: if F is a prolog atom, return R as -F.
cnf(-F, R) :-
    nnf(-F, R1), cnf(R1, R2), R = R2.   % recurisve-case: if F is not a prolog atom, compute nnf(-F, R1) and cnf(R1, R2) and return R as R2.

cnf(X v (Y & Z), R) :-
	(atom(X), atom(Y), atom(Z)), !, R = ((X v Y) & (X v Z)).    % base-case: if X, Y, Z are prolog atoms using the distribution law return R as (X v Y) & (X v Z).
cnf(X v (Y & Z), R) :-
	cnf(X v Y, R1), cnf(X v Z, R2), R = (R1 & R2).  % recurisve-case: if X, Y, Z are not prolog atoms calculate cnf(X v Y, R1) and cnf(X v Z, R2) and return R as (R1 & R2).

cnf((X & Y) v Z, R) :-
	(atom(X), atom(Y), atom(Z)), !, R = ((X v Z) & (Y v Z)).    % base-case: if X, Y, Z are prolog atoms using the distribution law return R as (X v Z) & (Y v Z).
cnf((X & Y) v Z, R) :-
	cnf(X v Z, R1), cnf(Y v Z, R2), R = (R1 & R2).  % recurisve-case: if X, Y, Z are not prolog atoms calculate cnf(X v Z, R1) and cnf(Y v Z, R2) and return R as (R1 & R2).

cnf((X v Y) & Z, R) :-
	(atom(X), atom(Y), atom(Z)), !, R = ((X v Y) & Z).  % base-case: if X, Y, Z are prolog atoms using the distribution law return R as ((X v Z) & Z).
cnf((X v Y) & Z, R) :-
	cnf(X v Y, R1), cnf(Z, R2), R = (R1 & R2).  % recurisve-case: if X, Y, Z are not prolog atoms calculate cnf(X v Y, R1) and cnf(Z, R2) and return R as (R1 & R2).

cnf(X & (Y v Z), R) :-
	(atom(X), atom(Y), atom(Z)), !, R = (X & (Y v Z)).  % base-case: if X, Y, Z are prolog atoms using the distribution law return R as (X & (Y v Z)).
cnf(X & (Y v Z), R) :-
	cnf(X, R1), cnf(Y v Z, R2), R = (R1 & R2).  % recurisve-case: if X, Y, Z are not prolog atoms calculate cnf(X, R1) and cnf(Y v Z, R2) and return R as (R1 & R2).

cnf(X v Y, R) :-
	(atom(X), atom(Y)), !, R = (X v Y). % base-case: if X and Y are both prolog atoms, then return R as X v Y.
cnf(X v Y, R) :-
	cnf(X, R1), cnf(Y, R2), R = (R1 v R2).  % recurisve-case: if X and Y are not prolog atoms, then compute cnf(X, R1) and cnf(Y, R2) and return R as R1 v R2.

cnf(X & Y, R) :-
	(atom(X), atom(Y)), !, R = (X & Y). % base-case: if X and Y are both prolog atoms, then return R as X & Y.
cnf(X & Y, R) :-
	cnf(X, R1), cnf(Y, R2), R = (R1 & R2).  % recurisve-case: if X and Y are not prolog atoms, then compute cnf(X, R1) and cnf(Y, R2) and return R as R1 & R2.

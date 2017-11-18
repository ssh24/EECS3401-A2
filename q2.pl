% Operator precedence
:- op(700,xfy,>).	% Sequence of primitive actions
:- op(800,xfy,?).	% Non-deterministic branching
% | is predefined as xfy with precedence 1100	% Interleaved concurrency
:- op(1120,xfy,$).	% Synchronized concurrency

% include facts
:- include('facts/facts.pl').

% include example process definitions
:- include('examples/deadlock.pl').
:- include('examples/producer-consumer.pl').
:- include('examples/other.pl').

% --------------------------- run(P, R) predicate ------------------------------
% run(P, R) holds if and only if R is a complete execution of process P.
% A complete execution is an execution where the last process is final or cannot make any further transitions.

run(P, R) :-
	defproc(P, B), !, run(B, R).	% if P is a process defined by defproc with body B, run B and output in R (i.e run(B, R)).

run(P, R) :-
	final(P), R = [P].	% if P is a final process, return R as just P.

run(P, R) :-
	P-P-0, !, R = [P, P, 0].	% if P is a primitive action which has completed and has nothing more to be done, return R as P-P-0.

run(P1 > P2, R) :-
	(P1 > P2)-P1-P2, !, 
	append([(P1 > P2)], [], R1), append(R1, [P1], R2), run(P2, R3), 
	append(R2, R3, R).	% Sequence of primitive action: if P1 is a primitive action, doing a step of (P1 > P2), involves doing P1 and then leaving P2 to be done. The final execution R should be the execution upto P1 appended with the execution of P2.

run(P1 > P2, R) :-
	append([(P1 > P2)], [], R1), run(P1, R2), run(P2, R4), append(R1, R2, R3),
	append(R3, R4, R).	% Sequence of primitive action: if P1 is not a primitive action, perform run(P1, R2) and run(P2, R4) and return the execution list R as R3 appended with R4, where R3 is the execution of the inital execution and R2.

run(P1 ? P2, R) :-
	(P1 ? P2)-A-PR, append([(P1 ? P2)], [], R1), append(R1, [A], R2),
	run(PR, R3), append(R2, R3, R).	% Non-deterministic branching: Either does P1 or P2 with an action A to achieve some process PR and call run(PR, R3). The final execution is R, the appended list R2 and R3.

run(P1 | P2, R) :-
	((P1 | P2)-A-(P1 | P2R), append([(P1 | P2)], [], R1), append(R1, [A], R2), 
	run(P1 | P2R, R3), append(R2, R3, R));	% Interleaved concurrency: Executes process P1 with an action A, and calls run(P1 | P2R, R3). The final execution is R, the appended list R2 and R3.

	((P1 | P2)-A-(P1R | P2), append([(P1 | P2)], [], R1), append(R1, [A], R2), 
	run(P1R | P2, R3), append(R2, R3, R)).	% Interleaved concurrency: Executes process P2 with an action A, and calls run(P1R | P2, R3). The final execution is R, the appended list R2 and R3.

run(P1 $ P2, R) :-
	(P1 $ P2)-A-(P1R $ P2R), append([P1 $ P2], [], R1), append(R1, [A], R2),
	run(P1R $ P2R, R3), append(R2, R3, R).	% Synchronized concurrency: Executes process P1 and P2 with an action A, and calls run(P1R | P2R, R3). The final execution is R, the appended list R2 and R3.

% ------------------------- print_run(R) predicate -----------------------------
% print_run(R) prints execution in a readable way.
% The execution prints processs P1, then the action A needed to transition from A to P2 and the process P2 each in one line.
% Finally it also prints the transition between P1-A-P2.

print_run([P1,A,P2]) :-
	write("P1: "), writeln(P1),
	write("A: "), writeln(A),
	write("P2: "), writeln(P2),
	write("Transition: "), writeln(P1-A-P2).	% base-case: when there are only three elements in the execution list.
print_run([P1,A,P2|Rest]) :-
	write("P1: "), writeln(P1),
	write("A: "), writeln(A),
	write("P2: "), writeln(P2),
	write("Transition: "), writeln(P1-A-P2),
	print_run([P2|Rest]).	% recursive-case: print P1, A and P2 and recursively call print_run([P2|Rest]). Note P2 is re-included on the list because it will be the starting process of the next transition.

% --------------------- has_infinite_run(P) predicate --------------------------
% has_infinite_run(P) holds if and only if process P has an infinite run (i.e if there is a cycle configuration graph).

has_infinite_run(P) :-
	P - _ - PR, has_infinite_run(PR, [P]).	% If there is a transition such that we can get to PR from P, then recursively check if P has been already checked to reach PR. 

% helper predicate has_infinite_run(P, Path) where Path is a list denoting a sequence of transitions.
has_infinite_run(P, Path) :-
	member(P, Path), !.	% if P has already been visited in the transition path, then return true.
has_infinite_run(P, Path) :-
	P - _ - PR, has_infinite_run(PR, [P|Path]).	% if P has not been visited already and PN can be achieved by some action from P recursively call, recursively call has_infinite_run(PR, [P|Path]).

% --------------------- deadlock_free(P) predicate ---------------------------
% deadlock_free(P) holds if and only if process P cannot reach a deadlocked configuration (i.e a configuration where the process is not final but cannot make any further transition).

deadlock_free(P) :-
	P - _ - PR, not(final(PR)), not(PR - _ - _), !, fail.	% base-case: if there is a transition from P to PR and PR is not final and there is no more transition from PR, then P is not deadlock free by definition.

deadlock_free(P) :-
	P - _ - PR, final(PR), !.	% base-case: if there is a transition from P to PR and PR is final, P is deadlock free.
deadlock_free(P) :-
	P - _ - PR, deadlock_free(PR).	% recursive-case: if there is a transition from P to PR and PR is not final, recursively check if PR is deadlock free.

% --------------------- cannot_occur(P, A) predicate ---------------------------
% cannot_occur(P, A) holds if and only if there is no execution of process P where action A occurs.
% Used to check safety property.

cannot_occur(P, _) :-
	has_infinite_run(P), !, fail.	% base-case: if P has an infinite execution then fail. This is because it is undecidable if in an infinite execution of P action A will ever occur or not.
cannot_occur(P, A) :-
	primAct(A), run(P, R), forall(member(X, R), not(X = A)). % if P does not have an infinite execution, check if A is a primitive action and find an execution R for process P and check for every member X in R; X is not equal to A.

% --------------- whenever_eventually(P, A1, A2) predicate ---------------------
% whenever_eventually(P, A1, A2) holds if and only if in all executions of process P, whenever action A1 occurs, action A2 occurs afterwards.
% Used to check liveness property.

whenever_eventually(P, _, _) :-
	has_infinite_run(P), !, fail.	% base-case: if P has an infinite execution, fail because it is not decidable if whenever A1 occurs in a transition from P, A2 occurs afterwards.
whenever_eventually(P, A1, A2) :-
	primAct(A1), primAct(A2), run(P, R), whenever_a_b(A1, A2, R).	% recursive-case: if P does not have an infinite execution, check if A1 and A2 are primitive actions and find an execution R for process P and check whenever A1 occurs, A2 occurs afterwards in R using the helper predicate whenever_a_b(A1, A2, R).

% helper predicate whenever_a_b(A, B, R).
% The predicate recursively checks that whenever A occurs in the list R, B is occurs afterwards in R as well.
whenever_a_b(A, B, [A, B]).	% base-case:  if there is only two elements in the list and B is followed by A.
whenever_a_b(A, B, [A|Rest]) :-
	member(B, Rest), not(member(A, Rest)), !.	% recursive-case: if A is the first element and B is in Rest and A is not in Rest anymore return true.
whenever_a_b(A, B, [A|Rest]) :-
	member(B, Rest), member(A, Rest), whenever_a_b(A, B, Rest).	% recursive-case: if A is the first element, check if B exists in the Rest of the list along with A then recursively call whenever_a_b(A, B, Rest).
whenever_a_b(A, B, [_|Rest]) :-
	whenever_a_b(A, B, Rest).	% recursive-case: if A is not the first element of the list, recursively call whenever_a_b(A, B, Rest).

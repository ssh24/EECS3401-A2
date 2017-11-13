% Operator precedence
:- op(700,xfy,>).
:- op(800,xfy,?).
% | is predefined as xfy with precedence 1100
:- op(1120,xfy,$).

% facts
A-A-0 :- primAct(A).	% An action A has completed leaves nothing more to be done.

(A > P)-A-P :- primAct(A).	% Doing a step of a sequence (A > P) involves doing the initial action A leaving P to be done afterwards.

(P1 ? P2)-A-PR :- 
	P1-A-PR; P2-A-PR.	% Non-deterministic branching: Holds if either P1-A-PR holds or P2-A-PR holds.
		
(P1 | P2)-A-(P1R | P2) :-
	P1-A-P1R.	% Interleaved concurrency: Holds if P1-A-P1R holds.
	
(P1 | P2)-A-(P1 | P2R) :-
	P2-A-P2R.	% Interleaved concurrency: Holds if P2-A-P2R holds.
		
(P1 $ P2)-A-(P1R $ P2R) :-
	P1-A-P1R, P2-A-P2R.	% Synchronized concurrency: Holds if both P1-A-P1R and P2-A-P2R holds.
		
PN-A-PR :-
	defproc(PN, PB), PB-A-PR.	% Holds if PN is the name of a procedure that has a body PB and PB-A-PR holds.

% predicate final(p) holds when process P may legally terminate.
final(0).
final(P1 ? P2) :-
	final(P1); final(P2).
final(P1 | P2) :-
	final(P1), final(P2).
final(P1 $ P2) :-
	final(P1), final(P2).
final(P) :-
	defproc(P, B), final(B).

% simple primitive actions for other examples
primAct(a1).
primAct(a2).
primAct(a3).

% primitive actions for deadlock example
primAct(acquireLock1).
primAct(acquireLock2).
primAct(releaseLock1).
primAct(releaseLock2).
primAct(doSomething).

% primitive actions for producer-consumer example
primAct(produce).
primAct(consume).
primAct(underflow).
primAct(overflow).
primAct(notFull).
primAct(notEmpty).

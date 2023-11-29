% Defines the base semantics for the combinations. I.e when no speculation is happening

:- module(_, [], [assertions, fsyntax, datafacts, hiord, datafacts]).

:- doc(title, "µAsm semantics Base").

:- use_module(library(lists)).
:- use_module(library(write)).
:- use_module(library(streams)).
:- use_module(muasm_program).
:- use_module(spectector_flags).
:- use_module(concolic(concolic)).
:- use_module(concolic(symbolic)).
:- use_module(muasm_semantics).
:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(assoc)).

:- use_module(semanticsv5, [runv5/2, xrunv5_1/2, xrunv5/3]).
:- use_module(semanticsv4_noBranch, [xrunv4_1/2]).
:- use_module(semanticsv2, [xrunv2_1/2]).
:- use_module(semantics_sls, [xrunvSLS_1/2]).

% We use a combined conf for all combinations. I.e. even in the combination of V1 + V4 there is an RSB in the state but it is never used.
% This greatly simplifies these predicates here because we have a uniform representation

% We modify the non spec config to contain an empty stack. This is done for backwards compability to V5
% Check if we are sls
spec(ret, xc_c(Ctr, Conf, R, S), EndBrLoc, sls) := XC2 :-
    ConfN = ~xrunvSLS_1(xc(Ctr, Conf, S)), % Do one step of SLS
    ConfN = xc(Ctr1, Conf1, S1),
    XC2 = xc_c(Ctr1, Conf1, R, S1).

% If not just do standard
spec(Instr, C, EndBrLoc, _) := XC2 :-
    write('In spec del'), write(Instr),
    XC2 = ~spec_(Instr, C, EndBrLoc)
    .

spec_(call, xc_c(Ctr, c(M,A), R, S), _) := XC2 :-
    ConfN = ~xrunv5_1(xc_v5(Ctr, c(M,A, []), R, S)),
    ConfN = xc_v5(Ctr1, Conf1, R1, S1),
    Conf1 = c(M1, A1, _),                   
    XC2 = xc_c(Ctr1, c(M1,A1), R1, S1)
    . 

    % Duplication but not sure how to merge clauses
spec_(ret,  xc_c(Ctr, c(M,A), R, S), _):= XC2 :-
    ConfN = ~xrunv5_1(xc_v5(Ctr, c(M,A, []), R, S)),
    ConfN = xc_v5(Ctr1, Conf1, R1, S1),
    Conf1 = c(M1, A1, _),                   
    XC2 = xc_c(Ctr1, c(M1,A1), R1, S1)
    .

% V4
spec_(store,  xc_c(Ctr, Conf, R, S), _) := XC2 :-
    ConfN = ~xrunv4_1(xc41(Ctr, Conf, S)), % Do one step of V4
    ConfN = xc41(Ctr1, Conf1, S1),
    XC2 = xc_c(Ctr1, Conf1, R, S1).

%V1
spec_(branch,  xc_c(Ctr, Conf, R, S), _) := XC2 :-
    ConfN = ~xrun1(xc(Ctr, Conf, S)), % Do one step of V1
    ConfN = xc(Ctr1, Conf1, S1),
    XC2 = xc_c(Ctr1, Conf1, R, S1).

% V2 uses spec4 rollbacks but they work exactly the same
spec_(indirect_jump,  xc_c(Ctr, c(M,A), R, S), EndBrLoc) := XC2 :-
    ConfN = ~xrunv2_1(xc_v2(Ctr, c(M,A, EndBrLoc), S)), % Do one step of V1
    ConfN = xc_v2(Ctr1, Conf1, S1),
    Conf1 = c(M1, A1, _),
    XC2 = xc_c(Ctr1, c(M1, A1), R, S1).


% We use runv51 because it handles calls and returns in a combined way
nospec(xc_c(Ctr, Conf, R, S)) := XC2 :-
    % Think of these unpackings as the (non)-spec projection functions
    enabled(S),
    Conf = c(M, A),
    ( trace_rawpc(Conf), ConfN = ~runv5(c(M,A,[])); stop(Conf), ConfN = c(M,A, [])), % Catching end of program here
    ConfN = c(M1, A1, _),
    ( (spbarr = ~p(~pc(A)); stop(Conf)) -> S2 = ~zeroes(S); S2 = ~decr(S) ),
    XC2 = xc_c(Ctr, c(M1, A1), R, S2).


% SLS rollback Need to eat ret instruction even though V4 state is used
rlb(spec4(_,_,_), xc_c(Ctr, Conf, R, S), sls) := XC2 :-
    ConfN = ~xrunvSLS_1(xc(Ctr, Conf, S)),
    ConfN = xc(Ctr1, Conf1, S1),
    XC2 = xc_c(Ctr1, Conf1, R, S1).

rlb(Spec, Conf, _) := XC2 :-
    XC2 = ~rlb_(Spec, Conf)
    .
% V1 rollback
rlb_(spec(_,_,_,_,_), xc_c(Ctr, Conf, R, S)) := XC2 :-
    ConfN = ~xrun1(xc(Ctr, Conf, S)),
    ConfN = xc(Ctr1, Conf1, S1),
    XC2 = xc_c(Ctr1, Conf1, R, S1).


% V4 rollback
rlb_(spec4(_,_,_), xc_c(Ctr, Conf, R, S)) := XC2 :-
    ConfN = ~xrunv4_1(xc41(Ctr, Conf, S)),
    ConfN = xc41(Ctr1, Conf1, S1),
    XC2 = xc_c(Ctr1, Conf1, R, S1).
   
% V5 rollback
rlb_(spec5(_,_,_,_,_), xc_c(Ctr, c(M,A), R, S)) := XC2 :-
    ConfN = ~xrunv5_1(xc_v5(Ctr, c(M,A,[]), R, S)),
    ConfN = xc_v5(Ctr1, Conf1, R1, S1),
    Conf1 = c(M1, A1, _),                   
    XC2 = xc_c(Ctr1, c(M1,A1), R1, S1).


% Returns atom representing a key for an instruction. ie load(X,E) -> load. This is done to do the membership check with Z which only contains those atoms. I dont want to put instructions into Z because I think that could yield unification problems
instr_key(store(_,_)) := store.
instr_key(beqz(_,_)) := branch.
instr_key(callstart) := call.
instr_key(retstart) := ret.
instr_key(indirect_jump(_)) := indirect_jump.
instr_key(_) := nospec . 



:- export(xrun_c/6).
xrun_c(xc_c(Ctr, Conf, R, S), Z, Timeout, EndBrLoc, Flag) := Conf :- Timeout =< 0, !,
    trace(timeout).

xrun_c(xc_c(Ctr, Conf, R, S), Z, Timeout, EndBrLoc, Flag) := XC2 :-
     (stop(Conf), S = [] -> XC2 = xc_c(Ctr,Conf, R, S)
        % Note: S=[] (so that spec continue decr otherwise)
	; XC1 = ~xrun_c_1(xc_c(Ctr,Conf, R, S), Z, EndBrLoc, Flag), %Conf = c(_M,A), write( ~p(~pc(A))),
	  ( stop(Conf) -> Timeout1 = Timeout
	  ; Timeout1 is Timeout-1,
	    track_ins(Conf)
	  ),
	  XC2 = ~xrun_c(XC1, Z, Timeout1, EndBrLoc, Flag)
	).
% Flag is either sls or v5
% Z is the list of instructions we want to speculate on
xrun_c_1(Conf, Z, EndBrLoc, Flag) := CT :-
    Conf = xc_c(_,c(M,A),_, S),
    (stop(c(M,A)), K = nospec; K = ~instr_key(~p(~pc(A)))), % Propagate stop through rollback
    % If it is not enabled we need to rollback
    (enabled(S) -> 
        (member(K, Z) -> CT = ~spec(K, Conf, EndBrLoc, Flag) ; CT = ~nospec(Conf)) % Check if instr is in spec list Z
        ;
           S = [ST | S2], CT = ~rlb(ST, Conf, Flag)   ) % In this case S cannot be empty
    .



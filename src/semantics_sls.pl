:- module(_, [], [assertions, fsyntax, datafacts, hiord]).

:- doc(title, "µAsm semantics for SLS").

:- use_module(library(write)).
:- use_module(library(streams)).
:- use_module(library(lists)).
:- use_module(muasm_program).
:- use_module(spectector_flags).
:- use_module(concolic(concolic)).
:- use_module(concolic(symbolic)).
:- use_module(spectector_stats).
:- use_module(engine(messages_basic), [message/2]).
:- use_module(muasm_semantics).
:- use_package(.(muasm_syntax)).


:- export(xrunvSLS/3).
xrunvSLS(Conf, Timeout) := Conf :- Timeout =< 0, !,
    trace(timeout).

xrunvSLS(xc(Ctr,Conf, S), Timeout) := XC2 :-
    ( stop(Conf), S = [] -> XC2 = xc(Ctr,Conf, S)
        % Note: S=[] (so that spec continue decr otherwise)
	;  XC1 = ~xrunvSLS_1(xc(Ctr,Conf, S)), %,Conf = c(_M,A), write( ~p(~pc(A))), write(S),
	  ( stop(Conf) -> Timeout1 = Timeout
	  ; Timeout1 is Timeout - 1,
	    track_ins(Conf)
	  ),
	  XC2 = ~xrunvSLS(XC1,Timeout1)
	).



:- export(xrunvSLS_1/2).


% AM- Nobranch
xrunvSLS_1(xc(Ctr,Conf, S)) := XC2 :-
    enabled(S), % is this enabled' aswell? try a new way here I think cmov is missing here
    Conf = c(_M, A),
    ( ~p(~pc(A)) \= retstart; stop(Conf) ),
    (stop(Conf) ->  Conf2 = Conf;
    Conf2 = ~run1(Conf), trace_rawpc(Conf)),
     
    
    write('Execute NoBranch'), nl,
    ( (spbarr = ~p(~pc(A)); stop(Conf)) -> S2 = ~zeroes(S); S2 = ~decr(S) ),

    XC2 = xc(Ctr,Conf2, S2).

% AM Ret-Skip
xrunvSLS_1(xc(Ctr,Conf, S)) := XC2 :-
    enabled(S),
    (Conf = c(_M, A),  retstart  = ~p(~pc(A)) ), % Constrain instruction
    
    S2 = ~decr(S),
    S3 = [spec4(Conf, Ctr,~window(S))|S2], % create a transaction
    write('Execute Ret Skip'), nl,
    % Sine a Ret instruction is more than one instruction we need to increase the pc multiple times
    A2 = ~update0(A, pc, ~incpc(A)),
    A3 = ~update0(A2, pc, ~incpc(A2)),
    A4 = ~update0(A3, pc, ~incpc(A3)),
    A5 = ~update0(A4, pc, ~incpc(A4)),
    ConfP = c(_M, A5),
    Ctr1 is Ctr + 1,
    trace_rawpc(Conf),
    trace(start(Ctr)), trace(skip(~pc(A))),

    XC2 = xc(Ctr1,ConfP, S3).

% AM Rollback
xrunvSLS_1(xc(Ctr,Conf, S)) := XC2 :-
    
    S = [spec4(ConfP, ID, 0) | S2], % Window is 0
    ConfP = c(_M, A), % Use config from speculative transaction
    
    % Do one step here in non spec semantics
    trace_rawpc(Conf),
    trace(rollback(ID)),
    ConfP2 = ~run1(ConfP),
    ConfP2 = c(M_2, A_2),
    %write('Executing Rollback'),nl,
    trace(pc(~pc(A_2))), % order is now not exactly as before
    XC2 = xc(Ctr,ConfP2, S2).

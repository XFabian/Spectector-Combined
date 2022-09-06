:- module(_, [], [assertions, fsyntax, datafacts, hiord]).

:- doc(title, "µAsm semantics for V4").

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


%%%%%%%%%%%------------------------------------------------ Spectre V4 -------------------------------%%%%%

:- export(xrunv41/3).
xrunv41(Conf, Timeout) := Conf :- Timeout =< 0, !,
    trace(timeout).

xrunv41(xc41(Ctr,Conf, S), Timeout) := XC2 :-
    ( stop(Conf), S = [] -> XC2 = xc41(Ctr,Conf, S)
        % Note: S=[] (so that spec continue decr otherwise)
	;  XC1 = ~xrunv4_1(xc41(Ctr,Conf, S)),%Conf = c(_M,A)% write( ~p(~pc(A))), write(S),
	  ( stop(Conf) -> Timeout1 = Timeout
	  ; Timeout1 is Timeout - 1,
	    track_ins(Conf)
	  ),
	  XC2 = ~xrunv41(XC1,Timeout1)
	).



:- export(xrunv4_1/2).


% AM- Nobranch
xrunv4_1(xc41(Ctr,Conf, S)) := XC2 :-
    enabled(S), % is this enabled' aswell? try a new way here I think cmov is missing here
    Conf = c(_M, A),
    ( (
      (~p(~pc(A)) = store(X,E) -> ( X = bp; X = pc + 2)) ;
          (~p(~pc(A)) \= store(X1,E1))
      );
     stop(Conf)),
    (stop(Conf) ->  Conf2 = Conf;
    Conf2 = ~run1(Conf), trace_rawpc(Conf)),
     
    
    %write('Execute NoBranch'), nl,
    ( (spbarr = ~p(~pc(A)); stop(Conf)) -> S2 = ~zeroes(S); S2 = ~decr(S) ),

    XC2 = xc41(Ctr,Conf2, S2).

% AM Store-Skip
xrunv4_1(xc41(Ctr,Conf, S)) := XC2 :-
    enabled(S),
    (Conf = c(_M, A),  store(X, E)  = ~p(~pc(A)), (\+ X = bp, \+ X = pc+2) ), % Constrain instruction
    
    S2 = ~decr(S),
    S3 = [spec4(Conf, Ctr,~window(S))|S2], % create a transaction
    %write('Execute Store Skip'), nl,
    A2 = ~update0(A, pc, ~incpc(A)),
    ConfP = c(_M, A2),
    Ctr1 is Ctr + 1,
    trace_rawpc(Conf),
    trace(start(Ctr)), trace(skip(~pc(A))),

    XC2 = xc41(Ctr1,ConfP, S3).

% AM Rollback
xrunv4_1(xc41(Ctr,Conf, S)) := XC2 :-
    
    S = [spec4(ConfP, ID, 0) | S2], % Window is 0
    ConfP = c(_M, A), % Use config from speculative transaction
    
    % Do one step here in non spec semantics
    trace_rawpc(Conf),
    trace(rollback(ID)),
    ConfP2 = ~run1(ConfP),
    ConfP2 = c(M_2, A_2),
    %write('Executing Rollback'),nl,
    trace(pc(~pc(A_2))), % order is now not exactly as before
    XC2 = xc41(Ctr,ConfP2, S2).

 



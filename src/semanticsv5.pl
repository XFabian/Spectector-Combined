% Defines the Semantic rules for version 5

:- module(_, [], [assertions, fsyntax, datafacts, hiord, datafacts]).
:- use_module(library(lists)).
:- use_module(library(write)).
:- use_module(library(streams)).
:- use_module(muasm_program).
:- use_module(spectector_flags).
:- use_module(concolic(concolic)).
:- use_module(concolic(symbolic)).
:- use_module(muasm_semantics).
:- use_module(engine(messages_basic), [message/2]).


:- data rsb_size/1. % This is data with a getter and setter if no value is used get returns 200
% TODO there should be a better way to define a constant
% I use a function right now
:- export(set_rsb_size/1).
set_rsb_size(N) :-
	set_fact(rsb_size(N)).
get_rsb_size(N) :-
    ( rsb_size(N0) -> N0 = N
	; N = 5 % default
	).
:- export(rsb_window/2).
rsb_window([]) := N :-
    get_rsb_size(N).

    



%% New semantics not using eip but just using sp
:- export(runv5/2).
runv5(Confv5) := ConfNew :-
    Confv5 = c(M,A,ST),
    (retstart = ~p(~pc(A)) -> ConfNew = ~run_ret(retstart, Confv5);
        callstart = ~p(~pc(A)) -> ConfNew = ~run_call(callstart, Confv5);
        Conf1 = ~run1(c(M,A)), Conf1 = c(M1,A1), ConfNew = c(M1,A1,ST) ).
       
% Call
run_call(callstart, c(M,A, ST)) := c(M4,A4, ST) :-
    % eat callstart
    A2 = ~update0(A,pc,~incpc(A)),
    % eat push instruction
    Conf2 = ~run1(c(M,A2)),
    Conf3 = ~run1(Conf2),
    Conf3 = c(M3, A3),
    % eat jmp 
    Conf4 = ~run1(c(M3, A3)), % This creates a pc observation which I think is fine
    Conf4 = c(M4, A4).


% Return
run_ret(retstart, c(M,A, ST)) := c(M4,A4, ST) :-
    % eat retstart
    A2 = ~update0(A,pc,~incpc(A)),
     % eat pop instruction
    Conf2 = ~run1(c(M,A2)),
    Conf3 = ~run1(Conf2),
    Conf3 = c(M3, A3),
    %Ra = ~concretize(~ev(tmp,A3)), % we can seafely get the value since pc is always concret
    
    
    %write('RA stored on stack: '), write(Ra),
    Conf4 = ~run1(c(M3, A3)),
    Conf4 = c(M4, A4).

%% Speculative Semantics
:- export(xrunv5/3). 
xrunv5(Conf, Timeout) := Conf :- Timeout =< 0, !,
    trace(timeout).

xrunv5(xc_v5(Ctr,Conf, R, S), Timeout) := XC2 :-
    (stop(Conf), S = [] -> XC2 = xc_v5(Ctr,Conf, R, S)
        % Note: S=[] (so that spec continue decr otherwise)
	; XC1 = ~xrunv5_1(xc_v5(Ctr,Conf, R, S)),
	  ( stop(Conf) -> Timeout1 = Timeout
	  ; Timeout1 is Timeout-1,
	    track_ins(Conf)
	  ),
	  XC2 = ~xrunv5(XC1,Timeout1)
	).


:- export(xrunv5_1/2).
% AM-NoBranch
xrunv5_1(xc_v5(Ctr,Conf, R, S)) := XC2 :-
    enabled(S),
    (Conf = c(_M, A, ST), retstart \= ~p(~pc(A)), callstart \= ~p(~pc(A)) ; stop(Conf) ),
    (stop(Conf) -> Conf1 = Conf ; trace_rawpc(Conf), Conf1 = ~runv5(Conf)),
    
    (Conf = c(_M,A, ST), (spbarr = ~p(~pc(A)); stop(Conf)) -> S2 = ~zeroes(S); S2 = ~decr(S) ),
     
    %write('In No Branch'), nl,
    
    XC2 = xc_v5(Ctr,Conf1, R, S2).

% AM Rollback
xrunv5_1(xc_v5(Ctr,Conf, R, S)) := XC2 :-
    S = [spec5(ConfP, ID, 0, RP, L) | S2],
    (RP = [L | R3] -> R4 = R3 ; R4 = RP),
    %write('In rollback'), nl,
    trace_rawpc(Conf),
    trace(rollback(ID)),
    
    ConfP2 = ~runv5(ConfP),
    ConfP2 = c(_, A2, _),
    trace(pc(~pc(A2))), % This prints the current location after the jump
    XC2 = xc_v5(Ctr,ConfP2, R4, S2).
     
    
% AM-Call
xrunv5_1(xc_v5(Ctr,Conf, R, S)) := XC2 :-
    enabled(S),
    (Conf = c(_M, A, ST), callstart = ~p(~pc(A)) ), 
    length(R, N), N < ~rsb_window([]),
    % Since our call is longer than one instruction incpc is not enough needs to be + 4
    R2 = [~concretize(~ev(pc+4,A)) | R], 
    trace_rawpc(Conf),
    Conf2 = ~runv5(Conf),
    %write('In AM CALL'), nl,
    S2 = ~decr(S),
    XC2 = xc_v5(Ctr,Conf2, R2, S2).

% AM-Ret
xrunv5_1(xc_v5(Ctr,Conf, R, S)) := XC2 :-
    enabled(S),
    (Conf = c(_M, A, ST), retstart = ~p(~pc(A)) ),
    R = [L |R2],
      
    SP  = ~ev(sp,A),
    L_RET = ~concretize(~element(_M, SP)), 
    L_RET \= L,
    
    % We ned to eat the return manually since we want to jump to l and not not L1!
    % eat retstart
    %write('In AM Ret'), nl,
    trace_rawpc(Conf),
    A2 = ~update0(A,pc,~incpc(A)),
    % eat pop instruction
    Conf2 = ~run1(c(_M,A2)),
    Conf3 = ~run1(Conf2),
    Conf3 = c(M3, A3),
    A4 = ~update0(A3, pc, L), % New return location Make sure that this is a concrete value
    %write('New Return Location: '), write(L), nl,
    %write('Safed Value: '), write(L_RET), nl,
    ConfNew = c(M3, A4, ST),
    
    S2 = ~decr(S),
    S3 = [spec5(Conf,Ctr,~window(S),R, L)|S2],
    Ctr1 is Ctr + 1,
   
    trace(start(Ctr)),
    trace(ret(L)),      
    
    XC2 = xc_v5(Ctr1,ConfNew, R2, S3).


% SE-Ret_SAME
xrunv5_1(xc_v5(Ctr,Conf, R, S)) := XC2 :-
    enabled(S),
    (Conf = c(_M, A, ST), retstart = ~p(~pc(A)) ),

    % we need to load from sp here
    L = ~ev(sp,A),
    Ra = ~concretize(~element(_M, L)), % get the return address from stack reasoning as in AM-Ret
    R = [L2 | R2],
    L2 == Ra,
    %write('In SE-Ret Same'), nl, 
    Conf1 = ~runv5(Conf),
    S2 = ~decr(S),

    XC2 = xc_v5(Ctr,Conf1, R2, S2).


% Corner case when rsb is full

% SE-Ret Empty
xrunv5_1(xc_v5(Ctr,Conf, R, S)) := XC2 :-
    enabled(S),
    (Conf = c(_M, A, ST), retstart = ~p(~pc(A)) ),
    length(R,N), N = 0,
    %write('In SE-Ret Empty'), nl,
    Conf1 = ~runv5(Conf),
    S2 = ~decr(S),
    XC2 = xc_v5(Ctr,Conf1, R, S2).

 % SE-Call-Full
xrunv5_1(xc_v5(Ctr,Conf, R, S)) := XC2 :-
    enabled(S),
    (Conf = c(_M, A, ST), callstart = ~p(~pc(A)) ),

    length(R, N),
    N >= ~rsb_window([]),
    %write('In SE-Call Full'), nl,
    Conf1 = ~runv5(Conf),
    S2 = ~decr(S),
    XC2 = xc_v5(Ctr,Conf1, R, S2).
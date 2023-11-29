:- module(_, [], [assertions, fsyntax, datafacts, hiord]).

:- doc(title, "µAsm semantics for V2").

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


:- export(xrunv2/3).
xrunv2(Conf, Timeout) := Conf :- Timeout =< 0, !,
    trace(timeout).

xrunv2(xc_v2(Ctr,Conf, S), Timeout) := XC2 :-
    ( stop(Conf), S = [] -> XC2 = xc_v2(Ctr,Conf, S)
        % Note: S=[] (so that spec continue decr otherwise)
	;  XC1 = ~xrunv2_1(xc_v2(Ctr,Conf, S)),% Conf = c(_M,A, EndBrLoc), write( ~p(~pc(A))),% write(S),
	  ( stop(Conf) -> Timeout1 = Timeout
	  ; Timeout1 is Timeout - 1,
	    track_ins(Conf)
	  ),
	  XC2 = ~xrunv2(XC1,Timeout1)
	).



:- export(xrunv2_1/2).


% AM- Nobranch
xrunv2_1(xc_v2(Ctr,Conf, S)) := XC2 :-
    enabled(S),
    Conf = c(_M, A, EndBrLoc),
    % We only mispredict indirect jumps. If there is nothing to mispredict we use this rule again
    ( ~p(~pc(A)) \= indirect_jump(E) ; stop(Conf); (~p(~pc(A)) = indirect_jump(E), length(EndBrLoc, 0)) ),
    (stop(Conf) ->  Conf2 = Conf;
        Conf1 = ~run1(c(_M, A)),
        Conf1 = c(M1, A1),
        Conf2 = c(M1, A1, EndBrLoc),
        trace_rawpc(Conf)),
    
    write('V2 Execute NoBranch'), nl,
    ( (spbarr = ~p(~pc(A)); stop(Conf)) -> S2 = ~zeroes(S); S2 = ~decr(S) ),

    XC2 = xc_v2(Ctr,Conf2, S2).






% AM Jmp Misprediction
xrunv2_1(xc_v2(Ctr,Conf, S)) := XC2 :-
    enabled(S),
    (Conf = c(_M, A, EndBrLoc),  indirect_jump(E)  = ~p(~pc(A)) ), % Constrain instruction
    S2 = ~decr(S),

    trace_rawpc(Conf),
    trace(start(Ctr)), trace(skip(~pc(A))),

    write('Spec V2'),
    % Need to create multiple speculative instances. One for each entry in the EndBrLoc. Possbily with backtrack
    % We create the first one manually so that we can put it to the front and create correct trace events. Also its the new
    EndBrLoc = [N| EndBrRest], % That assumes there is an entry if there is none we cannot speculate
    Ctr1 is Ctr + 1,
    S_new = ~create_transactions(EndBrRest, Ctr1, Conf, ~window(S), []), % Maybe needs to be reversed
    %write(S_new),
    % The config of the old state needs to be retrofitted to point to the next speculative transaction
    S3 = [spec4(c(_M, A), Ctr,~window(S))|S2], % create a transaction to rollback to
    append(S_new, S3, S4), % Add all other states to the front for multiple transactions
    write('Execute Jmp Speculation'), nl,
    write(S4),
    A2 = ~update0(A, pc, N),
    ConfP = c(_M, A2, EndBrLoc),
    length(EndBrLoc, L),
    CtrP is Ctr + L, % L new speculative transactions are created
    

    XC2 = xc_v2(CtrP,ConfP, S4).

% AM Rollback
xrunv2_1(xc_v2(Ctr,Conf, S)) := XC2 :-
    Conf = c(_,_, EndBrLoc),
    S = [spec4(ConfP, ID, 0) | S2], % Window is 0
    ConfP = c(_M, A), % Use config from speculative transaction
    
    % Do one step here in non spec semantics for the endbr
    trace_rawpc(Conf),
    trace(rollback(ID)),
    Conf2 = ~run1(c(_M, A)),
    Conf2 = c(M_2, A_2),
    ConfP3 = c(M_2, A_2, EndBrLoc),
    write('V2: Executing Rollback'),nl,
    XC2 = xc_v2(Ctr,ConfP3, S2).


create_transactions([], _, _, _, Spec) := SpecRes :- SpecRes = Spec.
create_transactions([N| XS], Ctr, Conf, Window, Spec) := SpecRes  :-
    Conf = c(M, A, EndBrLoc),
    A2 = ~update0(A, pc, N),
    S = spec4(c(M, A2), Ctr, Window),
    trace(start(Ctr)),
    Ctr1 is Ctr + 1,
    SpecRes = ~create_transactions(XS, Ctr1, Conf, Window, [S|Spec]).
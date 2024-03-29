% Copyright 2018 The Spectector authors
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
% ===========================================================================

:- module(_, [], [assertions, fsyntax, datafacts, dcg]).

%:- doc(title, "Speculative non-interference check").

:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(write)).
:- use_module(library(streams)).
:- use_module(library(format)).
:- use_module(muasm_semantics).
:- use_module(muasm_program).
:- use_module(muasm_print).
:- use_module(spectector_flags).
:- use_module(spectector_stats).
:- use_module(concolic(symbolic)).
:- use_module(concolic(concolic), [pathgoal/2, sym_filter/2, conc_stats/3, set_nextpath_timeout/1]).
:- use_module(engine(runtime_control), [statistics/2]).

:- data time_control/1.
:- data time_data/1.
:- data time_trace/1.
:- data termination/1.
:- data data_check/1.
:- data control_check/1.

:- export(noninter_check/2).
% `Low` is a list of register names or memory indices that are "low".
% `C0` is the initial configuration.
noninter_check(Low, C0) :-
	set_nextpath_timeout(~get_limit(nextpath_timeout)),
	get_maxtime(MaxTime),
	%
	log('[exploring paths]'),
	( % (failure-driven loop)
	  set_fact(time_control(0)), set_fact(time_data(0)), set_fact(time_trace(0)),
	  set_fact(data_check(false)), set_fact(control_check(false)),
	  set_last_time(_),
	  ( concrun(C0, (C, Trace))
	  ; all_conc_stats_unknown, % In the case there are paths and concrun fails, collect stats % TODO: the paths may not be unknown?
	    collect_stats(conc_error, []), fail
	  ),
	  set_fact(time_control(0)), set_fact(time_data(0)), set_fact(time_trace(0)),
	  set_fact(data_check(false)), set_fact(control_check(false)),

	  last_time(LTP), set_last_time(TP), TimeTrace is TP - LTP, % Trace time
	  set_fact(time_trace(TimeTrace)),
	  ( member(timeout, Trace) ->
	    log('[maximum number of steps reached]'),
	    message(warning, 'maximum number of steps reached -- ignoring remaining path')
	  ; true
	  ),
	  log('[path found]'),
	  pretty_print([triple(C0,Trace,C)]),
	  log('[checking speculative non-interference]'),
	  (C0 = xc(_,C0n,_); C0 = xc_v4(_, C0n, _, _, _, _);
              C0 = xc_v5(_, c(M_, A_, ST_), _, _), C0n = c(M_,A_);
                  C0 = f(sls, xc(_, C0n, _));
                      C0 = f(2, xc_v2(_, c(M_, A_, _), _)), C0n = c(M_, A_);
                          C0 = f(X,xc_c(_, C0n, _, _), _); % For the combinations
                                      C0 = f(4, xc41(_, c(M_,A_), _)), C0n = c(M_,A_)
          ),
	  noninter_cex(Low, C0n, Trace, MaxTime, Safe),
	  collect_stats(Safe, Trace),
	  (
	    ( Safe = unknown_noninter ->
	      log('[unknown noninter]')
	    ; Safe = global_timeout ->
	      log('[global timeout reached]')
	    ; Safe = no(Status) ->
	      log('[path is unsafe]'),
	      set_fact(termination(Status))
	    ; log('[path is safe]')  % TODO: change log?
	    ),
	    % For bounded analysis
	    ( check_maxtime_limit(MaxTime) ->
	      log('[full timeout reached, program is assumed as safe]'),
	      !, % stop here
	      collect_path_limit_stats
	    ; explored_paths_left(N) -> % Fails if not initialized
	      ( N > 1 ->
		new_explored_path,
		( ( Mode = data ; Mode = control ), stop_on_leak, termination(Mode) ->
		  % Check if should stop
		  log('[program is unsafe]')
		; fail % go for next path
		)
	      ; log('[maximum number of paths reached, program is assumed as safe]'),
		!, % stop here
		collect_path_limit_stats
	      )
	    ; ( Mode = data ; Mode = control ),
		stop_on_leak, termination(Mode),
		log('[program is unsafe]')
	    ; fail % go for next path
	    )
	  )
	; ( termination(T) -> true ; T = safe ),
	  log(~atom_concat(~atom_concat('[program is ', T), ']')),
	  new_analysis_stat(status=string(~atom_codes(T)))
	).

% Compute MaxTime for full timeout
get_maxtime(MaxTime) :-
	( FullTO = ~get_limit(full_timeout),
	  FullTO > 0 ->
	    statistics(walltime, [Time0, _]),
	    MaxTime is Time0 + FullTO
	; MaxTime = 0 % no timeout
	).

% Check if we have reached the MaxTime limit (full timeout)
check_maxtime_limit(MaxTime) :-
	MaxTime > 0, % (disabled otherwise)
	statistics(walltime, [CurrTime, _]),
	CurrTime > MaxTime.

% Compute MaxTime for noninter timeout
get_noninter_maxtime(MaxTime) :-
	( NoninterTO = ~get_limit(noninter_timeout),
	  NoninterTO > 0 ->
	    statistics(walltime, [Time0, _]),
	    MaxTime is Time0 + NoninterTO
	; MaxTime = 0 % no timeout
	).

% Remaining time for noninter timeout (0 for disabled and -1 if timeout reached)
rem_noninter_time(MaxTime, TO) :- MaxTime > 0, !, % (disabled otherwise)
	statistics(walltime, [CurrTime, _]),
	TO1 is ceiling(MaxTime - CurrTime), % (keep it as integer)
	( TO1 =< 0 -> TO = -1
	; TO = TO1
	).
rem_noninter_time(_, 0). % timeout disabled

collect_stats(Safe, Trace) :- stats, !,
	trace_length(Trace, TL),
	add_path_stat(trace_length=TL),
	findall(X, conc_stats_json(X), LConc),
	add_path_stat(concolic_stats=LConc),
	( Safe = no(Mode) -> StatusStr = ~atom_codes(Mode)
	; Safe = unknown_noninter -> StatusStr = "unknown_noninter" % TODO: change?
	; Safe = global_timeout -> StatusStr = "global_timeout" % TODO: change?
	; Safe = conc_error -> StatusStr = "conc_error" % TODO: change?
	; StatusStr = "safe"
	),
	time_data(TimeData), time_control(TimeControl), time_trace(TimeTrace),
	control_check(ControlCheck), data_check(DataCheck),
	new_path([status=string(StatusStr),time_trace=TimeTrace,time_data=TimeData,time_control=TimeControl,
	control_check=ControlCheck, data_check=DataCheck]),
	( Safe = no(_) -> new_analysis_stat(status=string(StatusStr)) ; true ).
collect_stats(_, _).

conc_stats_json(json([len=ConcLen,time=ConcT,status=string(ConcStStr)])) :-
	conc_stats(ConcLen, ConcT, ConcSt),
	ConcStStr = ~atom_codes(ConcSt).

collect_path_limit_stats :- stats, !,
	( termination(T) -> true ; T = safe ), atom_concat(T, '_bound', TBound),
	new_analysis_stat(status=string(~atom_codes(TBound))). % TODO: Check if there are no paths to inspect left
collect_path_limit_stats.

% Obtain a counter example for speculative non-interference.
% NOTE: see the paper for details, this check works on a single trace
%   at a time.

:- data noninter_status/2.


% :- compilation_fact(short_circuit_cex).
:- if(defined(short_circuit_cex)).
noninter_cex(Low, C0, Trace, MaxTime, no(Mode)) :-
        
	retractall_fact(noninter_status(_,_)),
	( perform_data, Mode = data ; perform_control, Mode = control ),
	% \+ \+ noninter_cex_(Mode, Low, C0, Trace, MaxTime), !.
	set_last_time(_),
	( \+ \+ noninter_cex_(Mode, Low, C0, Trace, MaxTime) ->
		log(~atom_concat('[Found ',~atom_concat(Mode, ' leak]'))),
		last_time(Time0), set_last_time(Time),
		TotalTime is Time - Time0,
		( Mode = data ->
			set_fact(time_data(TotalTime)),
			set_fact(time_control(0)),
			set_fact(data_check(true))
		; Mode = control ->
			set_fact(time_control(TotalTime)),
			set_fact(control_check(true))
		)
	; log(~atom_concat('[No ',~atom_concat(Mode, ' leak]'))),
		last_time(Time0), set_last_time(Time),
		TotalTime is Time - Time0,
		( Mode = data -> set_fact(time_data(TotalTime))
		; Mode = control -> set_fact(time_control(TotalTime))
		),
		fail
	),
	!.		
:- else.
noninter_cex(Low, C0, Trace, MaxTime, no(Mode)) :-
	retractall_fact(noninter_status(_,_)),
	set_last_time(_),
	( perform_data ->
	  ( \+ \+ noninter_cex_(data, Low, C0, Trace, MaxTime) ->
	    log('[Found data leak]'),
	    last_time(TimeD0), set_last_time(TimeD),
	    TotalTimeD is TimeD - TimeD0,
	    set_fact(time_data(TotalTimeD)),
	    set_fact(data_check(true)),
	    Leak = data
	  ; log('[No data leak]'),
	    last_time(TimeD0), set_last_time(TimeD),
	    TotalTimeD is TimeD - TimeD0,
	    set_fact(time_data(TotalTimeD)) )
	; true
	),
	( perform_control ->
	  ( \+ \+ noninter_cex_(control, Low, C0, Trace, MaxTime) ->
	    log('[Found control leak]'),
	    last_time(TimeC0), set_last_time(TimeC),
	    TotalTimeC is TimeC - TimeC0,
	    set_fact(time_control(TotalTimeC)),
	    set_fact(control_check(true)),
	    ( var(Leak) -> Leak = control ; true )
	  ; log('[No control leak]'),
	    last_time(TimeC0), set_last_time(TimeC),
	    TotalTimeC is TimeC - TimeC0,
	    set_fact(time_control(TotalTimeC))
	  )
	; true 
	),
	nonvar(Leak),
	!,
	Mode = Leak.
:- endif.



noninter_cex(_, _, _, _, Safe) :-
	( noninter_status(_, unknown) -> % unknown safety if some get_model/2 returned unknown
	    Safe = unknown_noninter
	; noninter_status(_, global_timeout) ->
	    Safe = global_timeout
	; Safe = yes
	).

noninter_cex_(Mode, Low, C0a, TraceA0, MaxTime) :-
	erase_and_dump_constrs(C0a, InGoalA),
	erase_model([InGoalA,TraceA0]), % remove all other concrete assignments
	get_noninter_maxtime(NoninterMaxTime),
	( Mode = data ->
	    % Data-based leak
	    TraceA = TraceA0,
	    rename_symspec(Low,
	                   C0a, InGoalA, TraceA, [],
			   C0b, InGoalB, TraceB, [],
			   LowGoal),
	    differdisj(TraceA, TraceB, OrCond),
	    \+ OrCond = [], % (just fail)
      	    DiffGoal = [~or_cond(OrCond) = 1]
	; Mode = control ->
	    % Control-based leak
	    % (nondet)
	    select_spec_cond(TraceA0, TraceA, CondA), % select cond/1 in speculative fragments
	    rename_symspec(Low,
	                   C0a, InGoalA, TraceA, CondA,
			   C0b, InGoalB, TraceB, CondB,
			   LowGoal),
	    NegCondB = ~negcond(CondB), DiffGoal = [CondA,NegCondB],
	    X = sym(cond(CondA)), Y = sym(cond(NegCondB))
	; throw(unknown_mode(Mode))
	),
	rem_noninter_time(NoninterMaxTime, SolverTO),
	( ( SolverTO = -1 % timeout w.r.t. NoninterMaxTime
          ; check_maxtime_limit(MaxTime) % timeout w.r.t. MaxTime
	  ) ->
	    !, % (stop search, remember status, and fail)
	    assertz_fact(noninter_status(Mode, global_timeout)),
	    fail
	; true
	),
	% Get input model for two different heaps
	Goal = ~append(InGoalA,
	          ~append(~pathgoal(~sym_filter(TraceA)),
		    ~append(InGoalB,
		      ~append(~pathgoal(~sym_filter(TraceB)),
                        ~append(LowGoal,DiffGoal))))),
        % Goal without the difference in the speculative trace
        %write('Goal before to sat '), write(Goal), nl,
        add_formula_length(~length(Goal)),
	set_solver_opt(timeout, SolverTO),
	get_model(Goal, Status),
	assertz_fact(noninter_status(Mode, Status)),
	Status = sat, % (fail otherwise)
	%
	show_cex(C0a, TraceA, C0b, TraceB, X, Y, OrCond).

:- use_module(library(terms_vars)).
:- use_module(concolic(symbolic), [unassign/2, sym_to_map/2]).

show_cex(C0a, _TraceA, C0b, _TraceB, X, Y, OrCond) :-
    log('[path is unsafe, showing counter-example initial configurations A and B]'),
	pretty_print([
	  msg('Case A:'),
	  %(C0a,[X]), % TODO: where?
	  (C0a,~append(_TraceA, [X])),
	  msg('Case B:'),
	  %(C0b,[Y]), % TODO: where?, 
	  (C0b,~append(_TraceB, [Y]))
        ]),
        (C0a = c(M,A,_) -> C1a = c(M,A); C1a= C0a), % If it has stack delete it
        (C0b = c(Mb,Ab,_) -> C1b = c(Mb,Ab); C1b= C0b),
        ~extended_information(C1a, C1b, _TraceA, _TraceB, OrCond). % For debugging not super stable

extended_information(C0a, C0b ,_TraceA, _TraceB, OrCond) :- !,
    C0a = c(_M, _A),
    C0b = c(_Mb, _Ab),
    % Map the Memory and the Assignments so we can access the
    % Afterwards filtered for memory /assignments which contain symbolic values
    ~sym_to_map(_M, Ma),
    ~sym_to_map(_Mb, Mb),
    %write(Ma), nl,
    ~sym_to_map(_A, Aa),
    ~sym_to_map(_Ab, Ab),
    ~filter_as(Aa, FAa),
    ~filter_as(Ab, FAb),
    ~filter_mem(Ma, FMa),
    ~filter_mem(Mb, FMb),
    nl, nl, nl,

    % All variables in all of these statements
    varset([C0a, _TraceA,C0b, _TraceB, FMa, FMb, FAa, FAb], Vars), % compute the whole variable set
    % unassign returns the values for each symbolic variable in the Map var
    unassign(Vars, Map),
    % Number all the variables (looks deterministic) and should be similar to the one in pretty print
    numbervars([Vars, Map], 0,_), nl,
    write('Assignments:'), nl,nl,
    write(Map), nl, nl,
    %write(_TraceA), nl,
    % we use noarr to filter sym(update(A3,sp,Y2,B3)) to sym(sp, Y2) dropping the config at start and at end
    % because we don't need them
    ~noarr_trace(_TraceA, FilterA),
    ~noarr_trace(_TraceB, FilterB),
    % I only want the speculative trace here
    % Or use differdisj to get stores and loads that differ from the trace 
    write('Trace Load and store different symbolic values on the speculative part of the trace'), nl,
    write('Note that they can evaluate to the same value but the symbolic variable that was used was different'), nl,
    % Filter trace to only contain store and loads
    ~filterSL(FilterA, FilterB2),
    ~filterSL(FilterB, FilterA2),
    % compute differences in symolic values used for same load e.g load(S3) =
    ~differences(FilterA2, FilterB2, Res),
    write(Res), nl,
    ~diff_trace(Res, Map),
    write('Looking at the differing Disjunction Spectector used in SMT formula'), nl, nl,
    ~print_diff_OrCond(OrCond, Map, Diff), nl,
    write('The Disjunction that was made unequal with Assignments'), nl, nl,
    write(Diff),
    nl, nl, 
    write('These Memory locations are different: '), nl,
    write('Printed as [MemLoc] = (SymVar = Val) \\/ (SymVar = Val)'), nl,
    % Mem location Sym var = Value)
    ~diff_memory(FMa, FMb, Map), !,
    nl, nl.
        

print_diff_OrCond([], Map) := [] :- !.
print_diff_OrCond([X \= Y|XS], Map) := Z  :-
    ~resolve_sym_trace(X, Map, V),
    ~resolve_sym_trace(Y, Map, V1),
    format('~q \\= ~q  => ~q \\= ~q ~n', [X, Y, V,V1]), !,
    ~print_diff_OrCond(XS, Map, ZS),
    (V \= V1 -> Z = [V \= V1| ZS]; Z = ZS).
    
diff_trace([], Map) :- !.

%store(X) = S3 +4 --- need to get symbolic vars from these expressions
diff_trace([ (store(X), store(Y)) | XS], Map) :-
    ~resolve_sym_trace(X, Map, V),
    ~resolve_sym_trace(Y, Map, V1),
    format('store(~q)    store(~q) ~n', [V,V1]), !, 
    ~diff_trace(XS, Map).

diff_trace([ (load(X), load(Y)) | XS], Map) :-
    ~resolve_sym_trace(X, Map, V),
    ~resolve_sym_trace(Y, Map, V1),
    format('load(~q)    load(~q) ~n', [V,V1]), !,
    ~diff_trace(XS, Map).

% Handles expression as well e.g (S3 + 4) for resolving. Just replaces sym var
resolve_sym_trace(X, []) := fail, !.
resolve_sym_trace(S, [S = V | YS]) := V :- !.
resolve_sym_trace(S/\ E, [S = V | YS]) := V /\ E :- !.
resolve_sym_trace(S + E, [S = V | YS]) := V + E :- !.
resolve_sym_trace(S - E, [S = V | YS]) := V - E:- !.
resolve_sym_trace(S<<E, [S = V | YS]) := V << E:- !.
resolve_sym_trace(X, [K = V | YS]) := ~resolve_sym_trace(X, YS) :- !.
                  
% Assumes list are in same order (Actually can have different amounts of values
diff_memory([], [], Map) :- !.
diff_memory(XS, [], Map) :- !.
diff_memory([], YS, Map) :- !.
diff_memory([X = S1| XS], [Y = S2 | YS], Map) :-
    ~resolve_sym(X = S1, Map, V), % Values of the symbolic value 
    ~resolve_sym(Y = S2, Map, V1),
    X = Y,
    (V \= V1 -> format('~d = (~q = ~d) \\/ (~q = ~d) ~n', [X, S1, V, S2, V1]); true),
    ~diff_memory(XS, YS, Map).

% lookup sym name in Map X=S 15785 = G4 for example
resolve_sym(X=S, []) := fail, !. % should not happen
resolve_sym(X=S, [S = V | YS]) := V :- !.
resolve_sym(X=S, [K = V | YS]) := ~resolve_sym(X=S, YS) :- !.

% Filter for store and loads in the trace
filterSL([]) := [] :- !.
filterSL([store(X) |XS]) := [store(X) | ~filterSL(XS)] :- !.
filterSL([load(X) |XS]) := [load(X) | ~filterSL(XS)] :- !.
filterSL([X |XS]) := ~filterSL(XS) :- !.

filter_mem([]) := [] :- !.
filter_mem([K = V | XS]) := ~filter_mem(XS) :- number(V), number(K), !.
filter_mem([X | XS]) := [X | ~filter_mem(XS)] :- !.
% bp = 23 register and value
filter_as([]) := [] :- !.
filter_as([R = V | XS]) := ~filter_as(XS) :- number(V), !.
filter_as([X | XS]) := [X | ~filter_as(XS)] :- !.


differences([], []) := [] :- !.
differences([store(X)| XS], [store(Y)| YS] ) := Z :-
    differences(XS, YS, ZS),
    (X \= Y -> Z = [(store(X),store(Y)) | ZS]; Z =ZS),
    !.
     
differences([load(X) | XS], [load(Y)|YS] ) := Z :-
    differences(XS, YS, ZS),
    (X \= Y -> Z = [(load(X),load(Y)) | ZS]; Z = ZS),
    !.

    
% Obtain a copy of the trace, unifying:
%  - variables corresponding to low memory and registers
%    (all registers in initial configuration are implicitly low)
%  - the nonspec obs
% ExtraA and ExtraB are used to apply renaming to additional
% constraints not in the traces.
rename_symspec(Low,
	       CSymA, InGoalA, TraceA, ExtraA,
	       CSymB, InGoalB, TraceB, ExtraB,
	       LowGoal) :-
    filter_nonspec(TraceA, NTraceA),
	copy_term((CSymA,InGoalA,TraceA,NTraceA,ExtraA),(CSymB,InGoalB,TraceB,NTraceB,ExtraB)),
	%
	CSymA = c(_,A), CSymB = c(_,A), % TODO: all registers are low (we could also pass them explicitly)
	unif_obs(NTraceA,NTraceB),
	unif_low_goal(Low, CSymA, CSymB, LowGoal, []).

% generate a goal to unify low values in both configurations Ca and Cb
unif_low_goal([], _, _) --> [].
unif_low_goal([X|Xs], Ca, Cb) -->
    unif_low_goal1(X, Ca, Cb),
	unif_low_goal(Xs, Ca, Cb).

unif_low_goal1(X, Ca, Cb) -->
	{ Ca = c(Ma, Aa) },
	{ Cb = c(Mb, Ab) },
	( { atom(X) } -> [element(Aa, X, V), element(Ab, X, V)] % TODO: not needed since all registers are low, fix solver to deal with these if needed
	; { integer(X) } -> [element(Ma, X, V), element(Mb, X, V)]
	; { throw(unknown_low(X)) }
	),
	!.

unif_obs([], []).
unif_obs([X|Xs], [Y|Ys]) :-
	( X = load(A) -> Y = load(A)
	; X = store(A) -> Y = store(A)
	; weak_sni, X = value(A) -> Y = value(A) % For weak speculative non-interference, we consider also value(N) observations in the non-speculative trace
	; true
	),
	unif_obs(Xs,Ys).

% Obtain a non-speculative trace from a speculative trace
% (removes rollbacks)
filter_nonspec([start(I)|Xs], NTrace) :-
	append(Spec, [End|Rest], Xs),
	( End = commit(I)
	; End = rollback(I)
	),
	!,
	( End = commit(I) -> Rest2 = ~append(Spec, Rest)
	; Rest2 = Rest
	),
	filter_nonspec(Rest2, NTrace).
filter_nonspec([X|Xs], [X|NTrace]) :-
	filter_nonspec(Xs, NTrace).
filter_nonspec([], []).

% Given traces Xs and Ys, obtain a disjunction representing that at
% least one of the load or store addresses differ.
differdisj([], [], []).
differdisj([X|Xs], [Y|Ys], OrCond) :-
	( differ(X, Y, Cond) -> OrCond = [Cond|OrCond0]
	; OrCond = OrCond0
	),
	differdisj(Xs, Ys, OrCond0).

or_cond([X]) := R :- !, R = X.
or_cond([X|Xs]) := X\/(~or_cond(Xs)).

differ(load(A), load(B), (A\=B)) :- A \== B.
differ(store(A), store(B), (A\=B)) :- A \== B.

% Obtain trace prefixes before a C cond(_) during speculation (fail if
% there are no more prefixes)
select_spec_cond([start(I)|Xs], Trace, C) :-
	append(Spec, [End|Rest], Xs),
	End = rollback(I), % only rollbacks
	!,
	( % obtain cond and drop the rest of spec
	  append(BeforeC, [sym(cond(C))|AfterC], Spec),
	  Trace = ~append([start(I)|BeforeC], Trace0),
%	  Trace0 = ~only_specmarks(~append(AfterC, Rest)) % drop all (wrong, not enough)
	  Trace0 = ~append(~only_specmarks(AfterC), [End|Rest]) % drop spec (it should be fine)
%	  Trace0 = ~append(AfterC, [End|Rest]) % drop spec (spurious during spec, is it fine?)
	; % continue with other speculative fragment
	  append([start(I)|Spec], [End|Trace0], Trace),
	  select_spec_cond(Rest, Trace0, C)
	).
select_spec_cond([X|Xs], [X|Trace], C) :-
	select_spec_cond(Xs, Trace, C).

only_specmarks([], []).
only_specmarks([X|Xs], [X|Ys]) :-
	( X = start(_) ; X = commit(_) ; X = rollback(_) ), !,
	only_specmarks(Xs, Ys).
only_specmarks([_|Xs], Ys) :-
	only_specmarks(Xs, Ys).

:- export(trace_length/2).
trace_length(Xs, N) :-
	trace_length_(Xs, 0, N).

trace_length_([], N, N).
trace_length_([X|Xs], N0, N) :-
	( X = sym(cond(_)) ->
	    N1 is N0 + 1
	; X = load(_) ->
	    N1 is N0 + 1
	; X = store(_) ->
	    N1 is N0 + 1
	; N1 = N0
	),
	trace_length_(Xs, N1, N).

all_conc_stats_unknown :-
	findall(St, conc_stats(_,_,St), L),
	L \= [],
	all_conc_stats_unknown_(L).

all_conc_stats_unknown_([unknown|L]) :- all_conc_stats_unknown_(L).
all_conc_stats_unknown_([]).

% ---------------------------------------------------------------------------
% (log messages)

:- use_module(engine(messages_basic), [message/2]).

log(X) :- message(user, X).

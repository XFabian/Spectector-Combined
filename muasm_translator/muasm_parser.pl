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

:- module(_, [parse_file/3], [assertions, dcg, fsyntax]).

:- doc(title, "µAsm assembly parser").

% A simple parser (partially) based on Prolog read/2 predicate

:- use_module(engine(messages_basic)). 
:- use_module(library(port_reify)). 
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(operators)).
:- use_module(library(stream_utils), [get_line/2]).

:- use_module(.(parser_aux)).

% Parse program from file F into R. Label dictionary is kept in Dic
parse_file(F, Dic, R) :-
    op(980, xfx, [(<-)]), % TODO: only for parsing cmov
    catch(parse_file_(F, R0), E, handle_err(E)),
    fix_labels(R0, Dic, R).

parse_file_(F, Insns) :-
    open(F, read, S),
    once_port_reify(parse_stream(S, Insns), Port),
    close(S),
    port_call(Port).

handle_err(syntax_error(Msg)) :- !,
    message(error, ['Could not parse:\n', $$(Msg)]), fail.
handle_err(E) :- throw(E).

parse_stream(S, Insns):-
    get_line(S, Cs),
    ( Cs = end_of_file ->
        Insns = []
    ; parse_line(Cs, Ins),
      ( Ins = '%' ->
          Insns = Insns0
      ; Insns = [Ins|Insns0]
      ),
      parse_stream(S, Insns0)
    ).

parse_line(Cs,X):- sent(X,Cs,[]), !.
parse_line(Cs,_):- throw(syntax_error(Cs)).

% ---------------------------------------------------------------------------

sent('%') --> empty, !.
sent('%') --> comment, !.
sent(label(Id)) --> label(Id), !.
sent(Ins) --> blanks, idcodes(Cs), blanks, "<-", !, blanks, oplist(Ys), { atom_codes(X, Cs), Ins =.. ['<-', X|Ys] }.
sent(Ins) --> instruction(Ins).

instruction(Ins) -->
    blanks, 
    idcodes(InsName1), { atom_codes(InsName,InsName1) },
    ( blanks1,
      oplist(Operands) -> []
    ; { Operands = [] }
    ),
    { Ins =.. [InsName|Operands] }.

oplist(Ops, Cs, Cs0) :-
    read_from_string_atmvars(Cs, Ops0),
    Cs0 = "",
    Ops = ~conj_to_list(Ops0).

conj_to_list(A) := Xs :- conj_to_list_(A, Xs, []).

conj_to_list_((A,B), Xs, Xs0) :- !,
    conj_to_list_(A, Xs, Xs1),
    conj_to_list_(B, Xs1, Xs0).
conj_to_list_(A, [A|Xs], Xs).

comment --> blanks, "%", !, ignore_rest.

label(Label) --> idcodes(Cs), ":", { atom_codes(Label, Cs) }, ( comment -> [] ; [] ).

% ---------------------------------------------------------------------------

:- use_module(library(dict)).

fix_labels(Insns, Dic, Insns2) :-
    scan_labels(Insns, Dic),
    replace_atms(Insns, Dic, Insns2).

scan_labels([], _Dic).
scan_labels([label(L)|Xs], Dic) :- !,
    dic_lookup(Dic, L, _),
    scan_labels(Xs, Dic).
scan_labels([_|Xs], Dic) :-
    scan_labels(Xs, Dic).

% generic, replace atoms defined in dic
replace_atms([], _, []).
replace_atms([X|Xs], Dic, [Y|Ys]) :-
    replace_atms1(X, Dic, Y),
    replace_atms(Xs, Dic, Ys).

replace_atms1(X, _Dic, Y) :- var(X), !, Y = X.
replace_atms1(X, Dic, Y) :- atom(X), !,
    ( dic_get(Dic, X, Y0) -> Y = Y0 ; Y = X ).
replace_atms1(X, Dic, Y) :-
    functor(X, N, A),
    functor(Y, N, A),
    X =.. [N|As],
    Y =.. [N|Bs],
    replace_atms_xs(As, Dic, Bs).

replace_atms_xs([], _Dic, []).
replace_atms_xs([X|Xs], Dic, [Y|Ys]) :-
    replace_atms1(X, Dic, Y),
    replace_atms_xs(Xs, Dic, Ys).

% ---------------------------------------------------------------------------

% TODO: ugly, fix operator precedence bugs in read_from_string
%:- use_module(library(read_from_string), [read_from_string_atmvars/3]).

:- use_module(engine(stream_basic), [pipe/2, close/1]).
:- use_module(library(stream_utils), [write_string/2]).
:- use_module(library(read), [read_term/3]).
:- use_module(library(port_reify)).

:- export(read_from_string_atmvars/2).
read_from_string_atmvars(String, Term) :-
    pipe(ReadFrom, WriteTo),
    write_string(WriteTo, String),
    display(WriteTo, '\n.'),
    close(WriteTo),
    once_port_reify(read_term(ReadFrom, Term, [variable_names(Ns)]), ReadR),
    close(ReadFrom),
    port_call(ReadR),
    unifvars(Ns).

% unify variables with their name
unifvars([]).
unifvars([X=X|Xs]) :- unifvars(Xs).
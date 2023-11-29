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

:- module(_, [parse_file_intel/2], [assertions, dcg, fsyntax]).

:- doc(title, "INTEL assembly parser").

% This is a parser for the AT&T style assembly files, for X86 instructions.
% See https://csiflabs.cs.ucdavis.edu/~ssdavis/50/att-syntax.htm

:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(port_reify)). 
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(lists)).
:- use_module(library(stream_utils), [get_line/2]).
:- use_module(library(dict)).

:- use_module(.(parser_aux)).
:- use_module(.(x86_table), [fixins/3, ins/4]).

% % (for testing)
% :- export(main/1).
% main([F]) :-
%       parse_file_intel(F, R),
%       ( member(I, R),
%           writeq(I), nl,
%           fail
%       ; true
%       ).

% Parse program from file F into R
parse_file_intel(F, R) :-
    catch(parse_file_(F, R), E, handle_err(E)).

parse_file_(F, Insns) :-
    open(F, read, S),
    once_port_reify(parse_stream(S, Insns), Port),
    close(S),
    port_call(Port).

handle_err(syntax_error(Msg)) :- !,
    message(error, ['Could not parse:\n', $$(Msg)]), fail.
handle_err(E) :- throw(E).

parse_stream(S, Insns):-
    parse_stream_(S, _Dic, Insns).

parse_stream_(S, Dic, Insns):-
    get_line(S, Cs),
    ( Cs = end_of_file ->
        Insns = []
    ; parse_line(Cs, Ins),
      ( Ins = const(N,V) -> % update constant dictionary
          dic_replace(Dic, N, V, Dic2),
          % display(user_error, const(N, V)), nl(user_error),
          Insns = Insns0
      ; Ins = '#' ->
          Dic2 = Dic,
          Insns = Insns0
      ; Dic2 = Dic,
        Ins2 = ~replace_const(Ins, Dic),
        Insns = [Ins2|Insns0]
      ),
      parse_stream_(S, Dic2, Insns0)
    ).

parse_line(Cs,X):- sent(X,Cs,[]), !.
parse_line(Cs,_):- throw(syntax_error(Cs)).

% Replace constants in addr/4
replace_const(X, _) := X :- var(X), !.
replace_const(X, _) := X :- atomic(X), !.
replace_const(X, Dic) := Y :- X = addr(SignedOffset,Base,Index,Scale), !,
    ( dic_get(Dic, SignedOffset, V) ->
        Y = addr(V,Base,Index,Scale)
    ; Y = X
    ).
replace_const(X, Dic) := Y :-
    X =.. [F|As],
    replace_const_xs(As, Dic, Bs),
    Y =.. [F|Bs].

replace_const_xs([], _) := [].
replace_const_xs([X|Xs], Dic) := [~replace_const(X, Dic)| ~replace_const_xs(Xs, Dic)].

% ---------------------------------------------------------------------------

sent('#') --> empty, !.
sent('#') --> comment, !.
sent('#') --> blanks, "?", !, ignore_rest. % TODO: !!
sent('#') --> blanks, "$pdata", !, ignore_rest. % TODO: !!
sent('#') --> blanks, "$unwind", !, ignore_rest. % TODO: !!
sent('#') --> blanks, idcodes2(_), blanks, "ENDP", !, ignore_rest. % Each munction has a start and an end
sent('#') --> blanks, idcodes2(_), blanks, "ENDS", !, ignore_rest. % Each munction has a start and an end
sent(symbol_direc(Name, Data)) --> directive_labeled(Name, Dir), process_directive(Dir, Data), !.
sent(Data) --> directive(Dir), process_directive(Dir, Data), !.
sent('#') --> directive(Dir), { skip_directive(Dir) }, ignore_rest, !. % TODO: finish
sent(const(N,V)) --> const(N,V), !.
sent(label(Id)) --> label(Id), !.
sent(Ins) --> instruction(Ins).

comment --> blanks, "#", !, ignore_rest.
comment --> blanks, ";", !, ignore_rest.

label(Label) --> idcodes2(Cs), { Cs = ["_"|_] }, !, blanks, ignore_rest, { atom_codes(Label, Cs) }.
label(Label) --> idcodes2(Cs), ":", !, { get_id(Cs, Label) }, ( comment -> [] ; [] ).
label(Label) --> idcodes2(Cs), blanks, "PROC", !, ignore_rest, { atom_codes(Label, Cs) }.
label(Label) --> idcodes2(Cs), blanks, "SEGMENT", !, ignore_rest, {atom_codes(Label,Cs)}.

const(N,V) --> blanks, idcodes2(Cs), blanks, "=", !, 
    { get_id(Cs, N) },
    blanks, num(V).

directive(X) --> blanks, idcodes(Dir), { atom_codes(X, Dir) }, !.
directive_labeled(Name, Dir) --> blanks, idcodes(N), { atom_codes(Name, N) }, blanks, idcodes(D), { atom_codes(Dir, D) }, !.

instruction(Ins) -->
    blanks, 
    insname(InsName1), { atom_codes(InsName, InsName1) },
    { ins(InsName, Fmt, N, _) }, % TODO: insert number of operands coherency
    ( blanks1,
      oplist(Operands) -> []
    ; { Operands = [] }
    ), blanks,
    ( ";" -> ignore_rest
    ; "#" -> ignore_rest
    ; []
    ),
    { length(Operands, N),
      reverse(Operands,IntelOperands),
      fixins(Fmt, IntelOperands, Operands2),
      Ins =.. [InsName|Operands2] }.

oplist(Ops)-->
    operand(Op),
    { Ops = [Op|Ops0] },
    blanks,
    ( "," ->
        blanks,
        oplist(Ops0)
    ; { Ops0 = [] }
    ).

reg(Reg) --> idcodes(Cs), { reg_(Reg, Cs, []) }, !.

reg_('%rax') --> "rax".
reg_('%eax') --> "eax".
reg_('%ax') --> "ax".
reg_('%al') --> "al".
reg_('%ah') --> "ah".
reg_('%rbx') --> "rbx".
reg_('%ebx') --> "ebx".
reg_('%bx') --> "bx".
reg_('%bl') --> "bl".
reg_('%bh') --> "bh".
reg_('%rcx') --> "rcx".
reg_('%ecx') --> "ecx".
reg_('%cx') --> "cx".
reg_('%cl') --> "cl".
reg_('%ch') --> "ch".
reg_('%rdx') --> "rdx".
reg_('%edx') --> "edx".
reg_('%dx') --> "dx".
reg_('%dh') --> "dh".
reg_('%dl') --> "dl".
reg_('%rsi') --> "rsi".
reg_('%esi') --> "esi".
reg_('%si') --> "si".
reg_('%sil') --> "sil".
reg_('%rsp') --> "rsp".
reg_('%esp') --> "esp".
reg_('%sp') --> "sp".
reg_('%rbp') --> "rbp".
reg_('%ebp') --> "ebp".
reg_('%bp') --> "bp".
reg_('%rdi') --> "rdi".
reg_('%edi') --> "edi".
reg_('%di') --> "di".
reg_('%dil') --> "dil".
reg_('%rip') --> "rip".
reg_('%eip') --> "eip".
reg_('%ip') --> "ip".
reg_('%r8') --> "r8".
reg_('%r8d') --> "r8d".
reg_('%r8b') --> "r8b".
reg_('%r9') --> "r9".
reg_('%r9d') --> "r9d".
reg_('%r9b') --> "r9b".
reg_('%r10') --> "r10".
reg_('%r10d') --> "r10d".
reg_('%r10b') --> "r10b".
reg_('%r11') --> "r11".
reg_('%r11d') --> "r11d".
reg_('%r11b') --> "r11b".
reg_('%r12') --> "r12".
reg_('%r12d') --> "r12d".
reg_('%r12b') --> "r12b".
reg_('%r13') --> "r13".
reg_('%r13d') --> "r13d".
reg_('%r13b') --> "r13b".
reg_('%r14') --> "r14".
reg_('%r14d') --> "r14d".
reg_('%r14b') --> "r14b".
reg_('%r15') --> "r15".
reg_('%r15d') --> "r15d".
reg_('%r15b') --> "r15b".

% TODO: segment-override memory addressing is not implemented. E.g.,
%   GAS "es:100", NASM "[es:100]"
%   GAS "%ds:-10(%ebp)", NASM "[ds:ebp-10]"

% TODO: make sure that we fully implement NASM syntax

% Address operand (see memory addressing syntax)
% GAS address operand has the form "Offset(Base,Index,Scale)",
% where:
%
%  - Offset is a signed offset (or symbolic value)
%  - Base is a register (or 0 if omitted)
%  - Index is a register (or 0 if omitted)
%  - Scale is a number (or 1 if omitted)
%
% It is equivalent to the memory address at "Base+Index*Scale+Offset"
% address (or "[Base+Index*Scale+Offset]" in NASM syntax).
%

%operand(_,B,[]) :- atom_codes(A,B), display(A).
operand(X) --> "DWORD ", operand(X). % To ignore
operand(X) --> "QWORD ", operand(X).
operand(X) --> "OFFSET ", operand(X).
operand(X) --> "FLAT:", operand(X).
operand(X) --> "PTR ", operand(X).
operand(X) --> "OFFSET ", operand(X).
operand(X) --> "BYTE ", operand(X).
operand(X) --> "SHORT ", operand(X).
operand(X) --> reg(X), !.
operand(X) --> num(X), !.
%operand(_,B,[]) :- atom_codes(A,B), display(A).
operand(addr(SignedOffset,Base,Index,Scale)) --> % TODO: Fix address
    ( offset2(SignedOffset) -> []
    ; { SignedOffset = 0 }
    ),
    "[",
    ( reg(Base) ->
        ( "*", num(Scale) -> [] ; {Scale = 1} ),
        ( "-" -> ( num(Index) -> [] ; reg(Index) )
        ; "+" -> ( num(Index) -> [] ; reg(Index) )
        ; { Index = 0 }
        )
    ; { Base = 0, Index = 0, Scale = 0 }
    ),
    "]",
    !.
operand(addr(SignedOffset,Base,Index,Scale)) --> % TODO: Fix address
    ( offset2(SignedOffset) -> []
    ; { SignedOffset = 0 }
    ),
    "[",
    ( reg(Base) ->
        "+", ( num(Index) -> [] ; reg(Index) ),
        "*", ( num(Scale) -> [] ; reg(Scale) )
    ; { Base = 0, Index = 0, Scale = 0}
    ),
    "]",
    !.
operand(addr(SignedOffset,0,0,0)) --> % TODO: not mem in "call printf"?
    offset2(SignedOffset),
    !.
operand(Z) --> idcodes2(Cs), !, { get_id(Cs, Z) }.

% TODO: like idcodes/3 but for intel assembly
idcodes2([X|Cs]) --> sym(X), !, idcodes2_(Cs).
idcodes2([X|Cs]) --> alpha(X), !, idcodes2_(Cs).

idcodes2_([X|Cs]) --> sym(X), !, idcodes2_(Cs).
idcodes2_([X|Cs]) --> digit(X), !, idcodes2_(Cs).
idcodes2_([X|Cs]) --> alpha(X), !, idcodes2_(Cs).
idcodes2_("") --> "".

sym(X) --> [X], { sym_(X) }.

sym_(0'?).
sym_(0'.).
sym_(0'_).
sym_(0'$).
sym_(0'@).

num_or_id2(Off) --> num(Off), !.
num_or_id2(Off) --> idcodes2(Cs), !, { get_id(Cs, Off) }.

offset2(A+B) --> num_or_id2(A), "+", !, num_or_id2(B).
offset2(A-B) --> num_or_id2(A), "-", !, num_or_id2(B).
offset2(A) --> num_or_id2(A), !.

% Simplified version, drops some $, @, ... % TODO: fix
get_id(Cs) := Id :-
    append(Cs1, "$", Cs),
    !,
    atom_codes(Id, Cs1).
get_id(Cs) := Id :-
    Cs = "$"||Cs1,
    !,
    atom_codes(Id, Cs1).
get_id(Cs) := Id :-
    Cs = "?"||Cs2,
    append(Cs1, "@"||_, Cs2),
    !,
    atom_codes(Id, Cs1).
get_id(Cs) := Id :-
    atom_codes(Id, Cs).


% ---------------------------------------------------------------------------
% Directives


% process_directive('PUBLIC', symbol(N)) --> 
%       blanks, idcodes(Name), { atom_codes(N, Name) }, ignore_rest.
% TODO: Process the size on init
process_directive('DD', direc(init, N)) --> blanks, num_intel(N), !, ignore_rest.
process_directive('DD', '#') --> blanks, idcodes2(_), blanks, "$", ignore_rest.
process_directive('DB', direc(init, N)) --> blanks, num_intel(N), ignore_rest.
process_directive('DQ', direc(init, N)) --> blanks, num_intel(N), ignore_rest.
process_directive('COMM', symbol_direc(N, direc(size, S))) --> 
    blanks, idcodes2(Name), { atom_codes(N, Name) }, ":", size(Size),
    ":", num_intel(Num), { S is Num*Size }, ignore_rest.

% TODO: how to process?
process_directive('_DATA', '#') --> ignore_rest.
process_directive('_BSS', '#') --> ignore_rest.

size(1) --> "BYTE".
size(8) --> "DWORD". % TODO: Well done?

% Skip
skip_directive('PUBLIC').
skip_directive('TITLE').
skip_directive('INCLUDELIB').
skip_directive('ALIGN').
skip_directive('DB').
skip_directive('END').
skip_directive('EXTRN').
skip_directive('ORG').
skip_directive('DB').
skip_directive('DQ').
skip_directive('include').
skip_directive('end').
skip_directive('_TEXT').

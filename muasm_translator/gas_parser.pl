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

:- module(_, [parse_file_gas/2], [assertions, dcg]).

:- doc(title, "GAS assembly parser").

% This is a parser for the AT&T style assembly files, for X86 instructions.
% For more details see:
%   http://sourceware.org/binutils/docs/as/
%   https://csiflabs.cs.ucdavis.edu/~ssdavis/50/att-syntax.htm

:- use_module(engine(messages_basic)).
:- use_module(library(port_reify)). 
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(stream_utils), [get_line/2]).
:- use_module(library(lists), [insert_last/3, length/2]).

:- use_module(.(parser_aux)).
:- use_module(.(x86_table), [fixins/3, ins/4]).

% % (for testing)
% :- export(main/1).
% main([F]) :-
%       parse_file_gas(F, R),
%       ( member(I, R),
%           writeq(I), nl,
%           fail
%       ; true
%       ).

% Parse program from file F into R
parse_file_gas(F, R) :-
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
    get_line(S, Cs),
    ( Cs = end_of_file ->
        Insns = []
    ; parse_line(Cs, Ins),
      ( Ins = '#' ->
          Insns = Insns0
      ; Insns = [Ins|Insns0]
      ),
      parse_stream(S, Insns0)
    ).
    
parse_line(Cs,X):- sent(X,Cs,[]), !.
parse_line(Cs,_):- throw(syntax_error(Cs)).

% ---------------------------------------------------------------------------

sent('#') --> empty, !.
sent('#') --> blanks, !. % Only for empty lines
sent('#') --> comment, !.
sent(Data) --> directive(Dir), process_directive(Dir,Data), !.
sent('#') --> directive(Dir), { skip_directive(Dir) }, ignore_rest, !. % TODO: finish
sent(label(Id)) --> blanks, label(Id), !.
sent(label_ins(Id,Ins)) --> blanks, label(Id), blanks, instruction(Ins), !.
sent(Ins) --> instruction(Ins).

comment --> blanks, "#", !, ignore_rest.

label(Label) --> idcodes(Cs), ":", { atom_codes(Label, Cs) }, blanks, ( comment -> [] ; [] ).

directive(X) --> blanks, idcodes(Cs), { Cs = "."||_, atom_codes(X, Cs) }, !.

% instruction(Ins) --> blanks, "lock;", instruction(Ins). % TODO: Maybe parse lock?
instruction(Ins) -->
    blanks, 
    insname(InsName1),
    { atom_codes(InsName, InsName1), ins(InsName, Fmt, N, _) },
    ( blanks1,
      oplist(Operands) -> []
    ; { Operands = [] }
    ), blanks,
    ( ";" -> ignore_rest
    ; "#" -> ignore_rest
    ; []
    ),
    { length(Operands, N) },
    { fixins(Fmt, Operands, Operands2) -> true ; Operands2 = Operands },
    { Ins =.. [InsName|Operands2] }.
instruction(unsupported_ins(Ins), Str, []) :- atom_codes(Ins, Str).

oplist(Ops)-->
    operand(Op),
    { Ops = [Op|Ops0] },
    blanks,
    ( "," ->
        blanks,
        oplist(Ops0)
    ; { Ops0 = [] }
    ).

reg(Reg) --> "%", idcodes(Cs), { atom_codes(Reg, [0'%|Cs]) }. % TODO: register!

% TODO: segment-override memory addressing is not implemented. E.g.,
%   GAS "es:100", NASM "[es:100]"
%   GAS "%ds:-10(%ebp)", NASM "[ds:ebp-10]"

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

% TODO: Jump to number of instruction defined by the register
% TODO: numeric positions of instructions as in x86
operand(addr(0,Base,Index,1)) --> % TODO: Well done? (i.e %fs:40)
    reg(Base), ":",
    ( reg(Index) -> []
    ; num(Index)
    ),
    !.
operand(indirect(Reg)) --> "*", reg(Reg).
operand(addr(SignedOffset,Base,Index,Scale)) -->
    ( offset(SignedOffset) -> []
    ; { SignedOffset = 0 }
    ),
    "(",
    ( reg(Base) -> []
    ; { Base = 0 }
    ),
    ( ",", reg(Index) ->
        ( ",", num(Scale) ->
            []
        ; { Scale = 1 }
        )
    ; { Index = 0, Scale = 0 } 
    ),
    ")",
    !.
operand(addr(SignedOffset,0,0,0)) --> % TODO: not mem in "call printf"?
    offset(SignedOffset),
    !.
operand(X) --> "$", num_or_id(X), !.
operand(X) --> reg(X), !.

% ---------------------------------------------------------------------------
% Directives


% Generate a initial value
process_directive('.type', symbol(Name)) --> % TODO: wrong
    blanks, idcodes(Cs), { atom_codes(Name, Cs) }, ignore_rest.
process_directive('.globl', symbol(Name)) --> % TODO: wrong
    blanks, idcodes(Cs), { atom_codes(Name, Cs) }, ignore_rest.
% TODO: Process the size on init
% TODO: process operations i.e: ".long   .LBB1_3-.LJTI1_0"
process_directive('.int', direc(init, N)) --> blanks, num(N), ignore_rest.
process_directive('.long', direc(init, N)) --> blanks, num(N), ignore_rest.
process_directive('.byte', direc(init, N)) --> blanks, num(N), ignore_rest.
process_directive('.short', direc(init, N)) --> blanks, num(N), ignore_rest.
process_directive('.word', direc(init, N)) --> blanks, num(N), ignore_rest.
process_directive('.value', direc(init, N)) --> blanks, num(N), ignore_rest. % TODO: Introduce list processing
process_directive('.zero', direc(zero, N)) --> blanks, num(N), ignore_rest.
process_directive('.size', symbol_direc(Name, direc(size, Size))) -->
    process_size(Name, Size).
process_directive('.comm', symbol_direc(Name, direc(size, Size))) -->
    process_size(Name, Size).
process_directive('.asciz', direc(cons, String)) -->
    process_directive('.ascii', direc(cons, S0)),
    { insert_last(S0, 0, String) }, ignore_rest.
process_directive('.string', direc(cons, String)) -->
    process_directive('.asciz', direc(cons, String)).
process_directive('.ascii', direc(cons, String)) -->
    blanks, string(String), ignore_rest.
process_directive('.quad', direc(init, label(Name))) --> blanks, idcodes(Cs), { atom_codes(Name, Cs) }, ignore_rest, !.
% Its not a label. its an identifier to a label similar to instructoin
% TODO: how to process?
process_directive('.bss', '#') --> ignore_rest.
process_directive('.data', '#') --> ignore_rest.

process_size(Name, Size) --> blanks, idcodes(Cs), ",", blanks,
    num(Size), { atom_codes(Name, Cs)}, ignore_rest.

% Skip
skip_directive('.file').
skip_directive('.ident').
skip_directive('.section').
skip_directive('.space').
skip_directive('.align').
skip_directive('.p2align').
skip_directive('.balign').
skip_directive('.cfi_restore').
skip_directive('.cfi_escape').
skip_directive('.cfi_restore_state').
skip_directive('.cfi_remember_state').
skip_directive('.cfi_offset').
skip_directive('.cfi_startproc').
skip_directive('.cfi_sections').
skip_directive('.cfi_endproc').
skip_directive('.cfi_def_cfa').
skip_directive('.cfi_def_cfa_offset').
skip_directive('.cfi_def_cfa_register').
% skip_directive('.quad').
skip_directive('.local').
skip_directive('.uleb128'). % TODO: Generate number unsigned
skip_directive('.sleb128'). % TODO: Generate signed number
skip_directive('.include').
skip_directive('.hidden').
skip_directive('.previous').
skip_directive('.pushsection').
skip_directive('.popsection').
skip_directive('.equ').
skip_directive('.loc').
skip_directive('.model').
skip_directive('.weak').
skip_directive('.text').
skip_directive('.set'). % TODO: For macros -> parse
% Always consider true the expression, so all the declared asembly will be generated
skip_directive('.if').
skip_directive('.endif').
skip_directive('.size'). % For other cases that doesn't match the pattern
skip_directive('.long'). % For other cases that doesn't match the pattern
skip_directive('.addrsig').
skip_directive('.addrsig_sym').


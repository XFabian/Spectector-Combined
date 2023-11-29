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

:- module(_, [translate_x86_to_muasm/9], [assertions, dcg, fsyntax, datafacts]).

:- use_module(engine(messages_basic)). 
:- use_module(library(dict)).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(write)).
:- use_module(.(gas_parser)).
:- use_module(.(intel_parser)).
:- use_module(.(x86_table), [ins/4]).
:- use_module(.(muasm_dump)).
:- use_module(library(streams)).

% (muasm syntax)
:- op(980, xfx, [(<-)]). % priority between (::) and (,)

% DB of seen labels
:- data label/1.

% ---------------------------------------------------------------------------

translate_x86_to_muasm(Format, F, UseDump, Dic, IgnNames, InitMem, HeapDir, MemLocs, Prg) :- % TODO: Do not return a configuration! just symbol table and initial mem
    ( UseDump = yes, has_dump(F, FDump) ->
        read_dump(FDump, PrgX86)
    ; ( Format = intel -> parse_file_intel(F, PrgX86)
      ; Format = gas -> parse_file_gas(F, PrgX86)
      ),
      write_dump(~dump_file(F), PrgX86)
    ),
    retractall_fact(label(_)),
    R = ~tr_insns(PrgX86),
    R2 = ~flatten(R),
    % write('Parsing in x86 translator'), nl,
    % write(R2), nl,
    % TODO: Declare a non-free variable as the 3rd argument
    MemLocs0 = memlocs([],[]),
    emit(R2, Dic, (<>, true), IgnNames, InitMem, (HeapDir, HeapDir, HeapDir), MemLocs0, MemLocs, Prg), !.
% TODO: Change intial Heap direction
translate_x86_to_muasm(_, _, _, _, _, _, _, _, _) :-
    throw(error(failed_to_parse, translate_x86_to_muasm/8)).

emit([], _, _, _, _, _, MemLocs, MemLocs) := [].
emit([X|Xs], Dic, CurrSymbol, IgnNames, InitMem, H, MemLocs0, MemLocs) :=
    ~emit_(X, Xs, Dic, CurrSymbol, IgnNames, InitMem, H, MemLocs0, MemLocs). % :- write(H), nl,nl.

% Resolve pending labels (this remove lookup_label/2 entries)
emit_(lookup_label(Label0,Label), Xs0, Dic, CurrSymbol, IgnNames, InitMem, H, MemLocs0, MemLocs) :=
    ~emit(Xs1, Dic, CurrSymbol, IgnNames, InitMem, H, MemLocs0, MemLocs) :- !,
    dic_lookup(Dic, Label0, Label),
    ( label(Label0) -> Xs1 = Xs0
    ; message(warning, ['Label not declared: ', Label0]),
      Xs1 = [unknown_pc(Label0)|Xs0]
    ).
% TODO: wrong; this should just add parameters to symbols!
emit_(symbol_direc(Name, Data), Xs, Dic, CurrSymbol, IgnNames, InitMem, H, MemLocs0, MemLocs) :=
    ~emit([symbol(Name), Data|Xs], Dic, CurrSymbol, IgnNames, InitMem, H, MemLocs0, MemLocs) :- !.
% Current symbol do not change
emit_(symbol(Name), Xs, Dic, CurrSymbol, IgnNames, InitMem, H, MemLocs0, MemLocs) :=
    ~emit(Xs, Dic, CurrSymbol, IgnNames, InitMem, H, MemLocs0, MemLocs) :- CurrSymbol = (Name, _), !.
emit_(symbol(Name), Xs, Dic, _, IgnNames, InitMem, (_H0, H1, H2), memlocs(M,A), MemLocs) :=
    ~emit(Xs, Dic, (Name, Ign), IgnNames, InitMem, (H, H, _), memlocs(M,[Name=H|A]), MemLocs) :- !,
    ( var(H2) -> H = H1 % If size hasn't been declared
    ; H = H2
    ),
    ( member(Name, IgnNames) -> Ign = true
    ; Ign = false
    ).
% TODO: Unify all dir processing

% If labels are written to memory. Happens for example for a jump table
% Set_mem is used because  load(ax,8+jump_table). Notice the idx into the table is 8. Next would be 16. If we use
%fill_mem without specifying where to start, there would be a mismatch. SO we just fill the whole memory for 8 bytes
emit_(direc(init, label(Id)), Xs, Dic, (Name, Ign), IgnNames, InitMem, (H0, H1, H2), memlocs(M0,A), MemLocs) :=
    ~emit(Xs, Dic, (Name, Ign), IgnNames, InitMem, (H0 ,H, H2), memlocs(M1,A), MemLocs) :- !,
   %  write('In direc for label '), write(Id), nl, write(H0), nl, write(H1), nl, write(H), nl,
    ( Ign = true -> H = H1, M1 = M0 % TODO: wrong!
    ; set_mem(8, Id, InitMem, H1, H, M1, M0)%, fill_mem([Id], InitMem, H1, H, M1, M0)
    ).

emit_(direc(init, N), Xs, Dic, (Name, Ign), IgnNames, InitMem, (H0, H1, H2), memlocs(M0,A), MemLocs) :=
    ~emit(Xs, Dic, (Name, Ign), IgnNames, InitMem, (H0 ,H, H2), memlocs(M1,A), MemLocs) :- !,
    ( Ign = true -> H = H1, M1 = M0 % TODO: wrong!
    ; fill_mem([N], InitMem, H1, H, M1, M0)
    ).

emit_(direc(cons, Values), Xs, Dic, (Name, Ign), IgnNames, InitMem, (H0, H1, H2), memlocs(M0, A), MemLocs) :=
    ~emit(Xs, Dic, (Name, Ign), IgnNames, InitMem, (H0, H, H2), memlocs(M1, A), MemLocs) :- !,
    ( Ign = true -> H = H1, M1 = M0 % TODO: wrong!
    ; fill_mem(Values, InitMem, H1, H, M1, M0)
    ).
emit_(direc(zero, N), Xs, Dic, (Name, Ign), IgnNames, InitMem, (H0, H1, H2), memlocs(M0, A), MemLocs) :=
    ~emit(Xs, Dic, (Name, Ign), IgnNames, InitMem, (H0, H, H2), memlocs(M1, A), MemLocs) :- !,
    ( Ign = true -> H = H1, M1 = M0 % TODO: wrong!
    ; set_mem(N, 0, InitMem, H1, H, M1, M0)
    ).
emit_(direc(size, V), Xs, Dic, Name, IgnNames, InitMem, (H0, H1, _H2), MemLocs0, MemLocs) :=
    ~emit(Xs, Dic, Name, IgnNames, InitMem, (H0, H1, H2), MemLocs0, MemLocs) :- !,
    H2 is H0 + V.
emit_(X, Xs, Dic, Name, IgnNames, InitMem, H, MemLocs0, MemLocs) :=
    [X|~emit(Xs, Dic, Name, IgnNames, InitMem, H, MemLocs0, MemLocs)] :- !.

% Translate all instructions
tr_insns([]) := [].
tr_insns([X|Xs]) := [~tr_ins(X)| ~tr_insns(Xs)].

% Translate one instruction (or label)
tr_ins(symbol(N)) := symbol(N).
% For a label in memory
tr_ins(direc(init, label(Label0))) := R :- !, R = [lookup_label(Label0, Label), direc(init, label(Label))],
    assertz_fact(label(Label0)).
tr_ins(direc(A, N)) := direc(A, N).
tr_ins(symbol_direc(A, N)) := symbol_direc(A, N).
tr_ins(label_ins(Label0,Ins_x86)) := R :- !, R = ~append(~tr_ins(label(Label0)), ~tr_ins(Ins_x86)).
tr_ins(label(Label0)) := R :- !, R = [lookup_label(Label0, Label), label(Label)],
    assertz_fact(label(Label0)).
tr_ins(unsupported_ins(I)) := unsupported_ins(I) :- !. % TODO: Finish on semantics
tr_ins(Ins_x86) := R :-
    Ins_x86 =.. [InsName|Ops],
    ins(InsName, _, N, InsSem), % TODO: Maybe use fmt?
    length(Ops, N),
    ( R = ~tr_ins_(InsSem, Ops) -> true
    ; throw(error(could_not_translate(Ins_x86), tr_ins/2))
    ).

tr_addr(addr(Offset,Base,Index,Scale)) := S :-
    tr_op(Offset,Offset2),
    tr_op(Base,Base2),
    tr_op(Index,Index2),
    scale_log2(Scale,Log),
    simpl(Base2+(Index2<<Log)+Offset2, S).

scale_log2(0,0).
scale_log2(1,0).
scale_log2(2,1).
scale_log2(4,2).
scale_log2(8,3).
scale_log2(16,4).
scale_log2(32,5).
scale_log2(64,6).
scale_log2(128,7).
scale_log2(256,8).
scale_log2(512,9).
scale_log2(1024,10).

% expression simplification
simpl(A+B) := ~simpl_add(~simpl(A), ~simpl(B)) :- !.
simpl(A<<B) := ~simpl_shl(~simpl(A), ~simpl(B)) :- !.
simpl(X) := X.

simpl_add(0,X) := X :- !.
simpl_add(X,0) := X :- !.
simpl_add(X,Y) := (X+Y).

simpl_shl(0,_) := 0 :- !.
simpl_shl(X,0) := X :- !.
simpl_shl(X,Y) := (X<<Y) :- !.

simpl_mul(1,B) := B :- !.
simpl_mul(A,1) := A :- !.
simpl_mul(-1,B) := -B :- !.
simpl_mul(A,-1) := -A :- !.
simpl_mul(0,_) := 0 :- !.
simpl_mul(_,0) := 0 :- !.
simpl_mul(A,B) := (B<<A2) :- !, A2 = ~scale_log2(A).
simpl_mul(A,B) := (A<<B2) :- !, B2 = ~scale_log2(B).
simpl_mul(A,B) := (A*B).

% High bits are ignored, this is needed to model correctly the
% execution using speculative load hardening
% TODO: needed for all memory accesses?
% TODO: assumes 64-bit addresses
spmask := 0x7fffffffffff. % (1<<47)-1

is_addr(addr(_,_,_,_)).

% TODO: Finish instructions -> Can give wrong results otherwise

% Note on temporary registers:
%  - e: used to load memory operands
%  - tmp: for ret
%  - c1,c2: registers for delayed comparison
%  - f: comparison

% Translate from intermediate semantics (from x86_table) into muasm

% Load effective address
tr_ins_(lea, [A,B]) := R :- !, tr_addr(A,A1), R = ~tr_assign(A1,B,no).
% X <- F(A) (and sometimes update flags) (X is implicit in the operation)
tr_ins_(exp1_to(F,B), [A]) := R :- !, R = ~tr_exp1(F,A,B).
% B <- F(A) (and sometimes update flags)
tr_ins_(exp1(F), [A,B]) := R :- !, R = ~tr_exp1(F,A,B).
% C <- F(B,A) (and sometimes update flags)
tr_ins_(exp2(F), [A,B,C]) := R :- !, R = ~tr_exp2(F,A,B,C).
% A <- F(A) (and sometimes update flags)
tr_ins_(assign_exp1(F), [A]) := R :- !, R = ~tr_exp1(F,A,A).
% B <- F(B,A) (and sometimes update flags)
tr_ins_(assign_exp2(F), [A,B]) := R :- !, R = ~tr_exp2(F,A,B,B).
% Exchange of operands % TODO: Change <x> By temp register
tr_ins_(xchg, [A,B]) := R :- !,
    R = [~tr_assign(A,'<x>',no),~tr_assign(B,A,no),~tr_assign('<x>',B,no)].
% Substraction with borrow % TODO: OK?
tr_ins_(subb, [A,B]) := R :- !,
    R = [(f<-ul(c1,c2)), ~tr_exp2(-,A,B,B), ~tr_exp2(-,f,B,B)].
% if c1>=c2 then (Result - 1) else Result
% Update flags
tr_ins_(uflags(F), [A,B]) := R :- !,
    tr_in([A,B],[A1,B1],R,R0),
    % TODO: fix semantics of test, which is very similar to cmp except for AF flag
    ( F = compare -> R0 = [~uflags(B1,A1)]
    ; F = test, A1=B1 -> R0 = [~uflags(A1,0)] % TODO: OK?
    ; F = test, integer(A1) -> R0 = [~uflags((B1 /\ A1), 0)] % TODO: OK?
    ; F = test -> R0 = [~uflags((B1 /\ A1), 0)] % TODO: OK?
    ; throw(error(unsupported_uflags(F,A,B), tr_ins_/3))
    ).
tr_ins_(branch(parity), [Label0]) := R :- !, % TODO: Well done?
    E =.. [=,c1,c2],
    R = [lookup_label(Label0,Label), (c1<-'c1%2'), (f<-E), beqz(f,Label)].
tr_ins_(branch(Cond), [Label0]) := R :- !,
    contrary(Cond,NCond),
    E =.. [NCond,c1,c2],
    R = [lookup_label(Label0,Label), (f<-E), beqz(f,Label)].
% Set A to 0 or 1 depending on condition
tr_ins_(condset(Cond), [A]) := R :- !,
    ( is_addr(A) -> tr_in([A], [A1], R, R0), End = ~tr_assign(A1, A, no)
    ; R = R0, End = [], A1 = A
    ),
    R0 = [~tr_assign(0, A1, no), ~tr_ins_(condmov(Cond), [1,A1])|End].
% Do B<-A depending on condition
tr_ins_(condmov(Cond), [A,B]) := R :- !,
    tr_in([A,B],[Av,Bv],R,R0),
    E =.. [Cond,c1,c2],
    R0 = [(f<-E), cmov(f,(Bv<-Av))].
% TODO: st and ld flags: on same memory possition without overlapping
% Do mem<-(c1,c2)
tr_ins_(st_flags, [A]) := R :- !, R = [~tr_assign(c1,A,no),~tr_assign(c2,A,no)].
% Do (c1,c2)<-mem
tr_ins_(ld_flags, [A]) := R :- !, R = [~tr_assign(A,c1,no),~tr_assign(A,c2,no)].
% Do B<-A
tr_ins_('<-', [A,B]) := R :- !, R = ~tr_assign(A,B,no).
% Push into the stack
tr_ins_(push, [A]) := R :- !,
    tr_in([A],[A1],R,R0),
    R0 = [(sp<-sp-8), store(A1, sp/\ (~spmask))].
% Pop from the stack
tr_ins_(pop, [A]) := R :- !,
    tr_in([A],[A1],R,R0),
    R0 = [load(A1, sp/\ (~spmask)), (sp<-sp+8)].
% Return from call
tr_ins_(ret, [void]) := R :- !, R = [retstart, ~tr_ins_(pop, [tmp]), jmp(tmp)].
tr_ins_(ret, [0]) := R :- !, R = [retstart, ~tr_ins_(pop, [tmp]), jmp(tmp)].
tr_ins_(ret, []) := R :- !, R = [retstart, ~tr_ins_(pop, [tmp]), jmp(tmp)].
% Do a call % TODO: Support if a register is given, by jumping to the number that it points to
tr_ins_(call, [A]) := R :- !, R = [callstart, ~tr_ins_(push, [pc+2]), ~tr_ins_(jmp, [A])].
% Restore stack pointer from BP and pop BP
tr_ins_(leave, []) := R :- !, R = [(sp <- bp), ~tr_ins_(pop, [bp])].
% 
tr_ins_(clt, []) := skip :- !. % TODO: fix, short form of "movslq %eax, %rax"
%
tr_ins_(skip, [_]) := skip :- !. % TODO: for "npad N"; why?
tr_ins_(skip, []) := skip :- !.
% Speculative barrier
tr_ins_(spbarr, []) := spbarr :- !.
% Endbranch instruction
tr_ins_(endbr, []) := endbr :- !.
% Stop
tr_ins_(stop_ins, []) := stop_ins :- !.
% Jump (to a register or label)
tr_ins_(jmp, [A]) := R :- !,
    ( A = indirect(Reg), is_reg(Reg) -> tr_op(Reg,Regv), R = indirect_jump(Regv)
    ; R = [lookup_label(A, Label), jmp(Label)]
    ).

% TODO: complete flag support! (with a parameter if needed)
% TODO: Division!! (Include also in the semantics)
tr_exp1(F,A,B) := R :- !,
    tr_in([A],[A1],R,R0),
    ( F = neg  -> X = -A1, UFlags = no
    ; F = not  -> X = A1 # (-1), UFlags = yes
    ; F = inc  -> X = A1+1, UFlags = yes
    ; F = dec  -> X = A1-1, UFlags = yes
    ; F = (>>) -> X = (A1>>1), UFlags = yes
    ; F = (<<) -> X = (A1<<1), UFlags = yes
    ; F = (*)  -> X = ~map_reg('%ax')*A1, UFlags = no
    % TODO: Set upper and lower parts ; F = (/)  -> X = ~map_reg('%ax')/A1, UFlags = no
    ; F = (ashr) -> X = ashr(A1,1), UFlags = yes
    ; throw(error(unsupported_exp1(F), tr_ins_/3))
    ),
    R0 = [~tr_assign(X,B,UFlags)].

% TODO: complete flag support! (with a parameter if needed)
% TODO: if sal implemented ,update flags
tr_exp2(F,A,B,C) := R :- !,
    tr_in([A,B],[A1,B1],R,R0),
    ( F = (*) -> ( X = ~simpl_mul(A1,B1)
                 ; X = (B1*A1)
                 ), UFlags = no
    ; F = (+) -> X = (B1+A1), UFlags = yes
    ; F = (-) -> X = (B1-A1), UFlags = yes
    ; F = (#) -> X = (B1#A1), UFlags = yes
    ; F = (pxor) -> X = (B1#A1), UFlags = no
    ; F = (>>) -> X = (B1>>A1), UFlags = yes
    ; F = (<<) -> X = (B1<<A1), UFlags = yes
    ; F = (ashr) -> X = ashr(B1,A1), UFlags = yes
    ; X =.. [F,B1,A1], UFlags = no
    ),
    ( C=X, UFlags = no -> R0 = [skip]
    ; R0 = [~tr_assign(X,C,UFlags)]
    ).

% TODO: both A and B cannot be addr at the same time (not valid in X86 but we could do it)!!
% TODO: implement tr_out/4 (or similar) to do it properly
tr_assign(A,B,UFlags) := R :- is_addr(A), !, UFlags = no,
    tr_addr(A,Av), tr_op(B,Bv),
    R = load(Bv,Av).
tr_assign(A,B,yes) := R :- is_addr(B), !,
    tr_op(A,A1), tr_addr(B,Bv),
    R = [(e <- A1), ~uflags(e,0), store(e,Bv)].
tr_assign(A,B,no) := R :- is_addr(B), !,
    tr_op(A,Av), tr_addr(B,Bv),
    R = store(Av,Bv).
tr_assign(A,B,yes) := R :- !,
    tr_op(A,A1), tr_op(B,B1),
    R = [(B1 <- A1), ~uflags(B1,0)].
tr_assign(A,B,no) := R :- !,
    tr_op(A,A1), tr_op(B,B1),
    R = (B1 <- A1).

% Translate input operands (introducing load if needed)
tr_in(Xs, Ys, R, R0) :-
    tr_in_(Xs, Ys, e, R, R0).

tr_in_([], [], _, R, R0) :- !, R = R0.
tr_in_([X|Xs], [Tmp|Ys], Tmp, [load(Tmp,Xv)|R1], R0) :- is_addr(X), !,
    tr_addr(X,Xv),
    tr_in_(Xs, Ys, ~next_tmp(Tmp), R1, R0).
tr_in_([X|Xs], [Xv|Ys], Tmp, R1, R0) :-
    tr_op(X,Xv),
    tr_in_(Xs, Ys, Tmp, R1, R0).

% TODO: only two temporary registers, but this should be enough for now
next_tmp(e,f).

% c1 and c2 are used as additional registers to do comparisons
uflags(A1,B1) := [(c1 <- A1), (c2 <- B1)].

% Translate operands (ignore memory operands)
tr_ops([X|Xs], [Y|Ys]) :- tr_op(X, Y), tr_ops(Xs, Ys).
tr_ops([],[]).

% Translate operand
tr_op(X,Y) :- ( is_reg(X), rename_reg(X,Y0) -> Y = Y0 ; Y = X ).

% X is a register name
is_reg(X) :- atom(X), atom_concat('%',_,X).

rename_reg(X,Y) :-
    ( map_reg(X,Y) -> true
    ; Y=X,
      message(warning, ['unknown register: ', X])
    ).

% TODO: ignoring size is an approximation
map_reg('%rax',ax).
map_reg('%eax',ax).
map_reg('%ax',ax).
map_reg('%al',ax).
%map_reg('%ah',ax). % TODO: wrong! high 8 bit!
map_reg('%rbx',bx).
map_reg('%ebx',bx).
map_reg('%bx',bx).
map_reg('%bl',bx).
%map_reg('%bh',bx). % TODO: wrong! high 8 bit!
map_reg('%rcx',cx).
map_reg('%ecx',cx).
map_reg('%cx',cx).
map_reg('%cl',cx).
%map_reg('%ch',cx). % TODO: wrong! high 8 bit!
map_reg('%rdx',dx).
map_reg('%edx',dx).
map_reg('%dx',dx).
%map_reg('%dh',dx). % TODO: wrong! high 8 bit!
map_reg('%dl',dx).
map_reg('%rsi',si).
map_reg('%esi',si).
map_reg('%sil',si).
map_reg('%si',si).
map_reg('%rsp',sp).
map_reg('%esp',sp).
map_reg('%spl',sp).
map_reg('%rbp',bp).
map_reg('%ebp',bp).
map_reg('%bpl',bp).
map_reg('%rdi',di).
map_reg('%edi',di).
map_reg('%dil',di).
map_reg('%di',di).
map_reg('%rip',0). % TODO: approximation for PIC code (which is fine before symbols are relocated but it may not for linked files)
map_reg('%r8',r8).
map_reg('%r8d',r8).
map_reg('%r8b',r8).
map_reg('%r9',r9).
map_reg('%r9d',r9).
map_reg('%r9b',r9).
map_reg('%r10',r10).
map_reg('%r10d',r10).
map_reg('%r10b',r10).
map_reg('%r11',r11).
map_reg('%r11d',r11).
map_reg('%r11b',r11).
map_reg('%r12',r12).
map_reg('%r12d',r12).
map_reg('%r12b',r12).
map_reg('%r13',r13).
map_reg('%r13d',r13).
map_reg('%r13b',r13).
map_reg('%r14',r14).
map_reg('%r14d',r14).
map_reg('%r14b',r14).
map_reg('%r15',r15).
map_reg('%r15d',r15).
map_reg('%r15b',r15).

% Because the condition of the branch is the contrary of the condition
contrary(uge,ul).
contrary(ug,ule).
contrary(ul,uge).
contrary(ule,ug).
contrary(>=,<).
contrary(>,=<).
contrary(=<,>).
contrary(<,>=).
contrary(=,\=).
contrary(\=,=).

% Fill memory with values Vs
fill_mem(Vs, no, H0, H, Mem, Mem0) :- !,
    length(Vs, N),
    H is H0 + N, Mem = Mem0.
fill_mem(Vs, _, H0, H, Mem, Mem0) :-
    fill_mem_(Vs, H0, H, Mem, Mem0).

fill_mem_([V|Vs], H0, H, [H0=V|Mem], Mem0) :- !,
    H1 is H0 + 1,
    fill_mem_(Vs, H1, H, Mem, Mem0).
fill_mem_([], H, H, Mem, Mem).

% Set memory with N times value V
set_mem(N, _V, no, H0, H, Mem, Mem0) :- !,
    H is H0 + N, Mem = Mem0.
set_mem(N, V, _, H0, H, Mem, Mem0) :-
    set_mem_(N, V, H0, H, Mem, Mem0).

set_mem_(N, V, H0, H, [H0=V|Mem], Mem0) :- N > 0, !,
    H1 is H0 + 1,
    N1 is N - 1,
    set_mem_(N1, V, H1, H, Mem, Mem0).
set_mem_(_, _, H, H, Mem, Mem).

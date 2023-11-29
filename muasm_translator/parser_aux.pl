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

:- module(_, [], [dcg]).

:- export(empty/2).
empty([],[]).

:- export(ignore_rest/2).
ignore_rest(_, []).

:- export(digit/3).
digit(X) --> [X], {X>=0'0, X=<0'9}.

:- export(digit16/3).
digit16(X) --> [X], {X>=0'0, X=<0'9}, !.
digit16(X) --> [X], {X>=0'a, X=<0'f}, !.
digit16(X) --> [X], {X>=0'A, X=<0'F}.

:- export(digit8/3).
digit8(X) --> [X], {X>=0'0, X=<0'7}.

:- export(alpha/3).
alpha(X) --> [X], { X>=0'a, X=<0'z -> true ; X>=0'A,X=<0'Z -> true ; fail }. 

:- export(blank/2).
blank --> [X], { X=<32 }.

:- export(blanks1/2).
blanks1 --> blank, blanks.

:- export(blanks/2).
blanks --> blank, !, blanks.
blanks --> [].

:- export(num/3).
num(X) --> "-", numcodes(Cs, Base), !, { number_codes(X1, Base, Cs), X is -X1 }.
num(X) --> numcodes(Cs, Base), !, { number_codes(X, Base, Cs) }.

:- export(num_intel/3).
num_intel(X) --> num_intel_(Cs), !, { number_codes(X, 16, Cs) }.

num_intel_([X|Cs]) --> digit16(X), numcodes16_(Cs), "H", !.

numcodes([X|Cs], 16) --> "0x", digit16(X), !, numcodes16_(Cs).
numcodes([X|Cs], 8) --> "0", digit8(X), !, numcodes8_(Cs).
numcodes([X|Cs], 10) --> digit(X), !, numcodes_(Cs).

numcodes_([X|Cs]) --> digit(X), !, numcodes_(Cs).
numcodes_("") --> "".

:- export(numcodes16_/3).
numcodes16_([X|Cs]) --> digit16(X), !, numcodes16_(Cs).
numcodes16_("") --> "".

oct_num(X) --> oct_numcodes(Cs), !, { number_codes(X, 8, Cs) }.

oct_numcodes([X|Cs]) --> digit8(X), !, numcodes8_(Cs). % TODO: 0 preceding?

numcodes8_([X|Cs]) --> digit8(X), !, numcodes8_(Cs).
numcodes8_("") --> "".


:- export(idcodes/3).
idcodes("_"||Cs) --> "_", !, idcodes_(Cs).
idcodes("."||Cs) --> ".", !, idcodes_(Cs).
idcodes([X|Cs]) --> alpha(X), !, idcodes_(Cs).

idcodes_("."||Cs) --> ".", !, idcodes_(Cs).
idcodes_("_"||Cs) --> "_", !, idcodes_(Cs).
idcodes_([X|Cs]) --> digit(X), !, idcodes_(Cs).
idcodes_([X|Cs]) --> alpha(X), !, idcodes_(Cs).
idcodes_("") --> "".

:- export(num_or_id/3).
num_or_id(Off) --> num(Off), !.
num_or_id(Off) --> idcodes(Off0), !, { atom_codes(Off, Off0) }.

:- export(insname/3). % TODO: Remove ambiguity if there's a way (i.e movs), with flags? Right now the resolution comes by the precedence in ins/3
insname([X|Cs]) --> alpha(X), insname_(Cs), suffix(_S1), suffix(_S2). % TODO: use extensions meaning

insname_([X|Cs]) --> alpha(X), insname_(Cs).
insname_([X|Cs]) --> digit(X), insname_(Cs).
insname_("") --> "".

% suffix(N) -> N means the number of bytes
suffix(4) --> "s".
suffix(8) --> "q".
suffix(1) --> "b".
suffix(4) --> "l". % TODO: Can be also 64-bit
suffix(2) --> "w".
suffix(10) --> "t".
suffix(8) --> "d". % TODO: OK?
suffix(none) --> "".

:- export(offset/3).
offset(A+B) --> num_or_id(A), "+", !, num_or_id(B).
offset(A-B) --> num_or_id(A), "-", !, num_or_id(B).
offset(A) --> num_or_id(A), !.

% TODO: finish, naive approach
:- export(string/3).
string(R) --> "\"", string_(R).

string_([]) --> "\"", !.
string_([C|D]) --> "\\", process_char(C), !, string_(D).
string_([C|D]) --> [C], string_(D).

process_char(X) --> oct_num(X).
process_char(8) --> "b".
process_char(12) --> "f".
process_char(10) --> "n".
process_char(13) --> "r".
process_char(9) --> "t".
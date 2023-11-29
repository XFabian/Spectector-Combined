% Copyright 2019 The Spectector authors
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

:- module(_, [], [assertions, dcg, fsyntax, datafacts]).

:- doc(title, "Dumps for faster program loading").

:- use_module(library(streams)).
:- use_module(library(stream_utils)).  
:- use_module(library(fastrw)).  
:- use_module(library(compiler/file_buffer)).
:- use_module(library(compiler/up_to_date), [up_to_date/2]).

:- export(dump_file/2).
% Name of dump file for F
dump_file(F) := ~atom_concat(F, '.dump').

:- export(has_dump/2).
% There is a dump and it is updated
has_dump(F, FDump) :-
    FDump = ~dump_file(F),
    up_to_date(FDump, F).

:- export(write_dump/2).
% Write Prg to FDump file
write_dump(FDump, Prg) :-
    file_buffer_begin(FDump, Buffer, Stream),
    current_output(CO),
    set_output(Stream),
    write_dump_(Prg),
    set_output(CO),
    ( file_buffer_commit(Buffer) ->
        true
    ; throw(error(could_not_write(FDump), write_dump/2))
    ).

write_dump_(Prg) :-
    ( % (failure-driven loop)
      member(X, Prg),
        fast_write(X),
        fail
    ; true
    ).

:- export(read_dump/2).
% Read Prg from the FDump file  
read_dump(FDump, Prg) :-
    open_input(FDump,OldInput),
    read_dump_(Prg),
    close_input(OldInput).

read_dump_(Prg) :-
    ( fast_read(R) ->
        Prg = [R|Prg0],
        read_dump_(Prg0)
    ; Prg = []
    ).

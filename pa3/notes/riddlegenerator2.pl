%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Jessica Marie Barre
% Program Name: riddlegenerator.pl
% Date Due : Dec 1, 2016
% Purpose: Solve a riddle DYNAMICALLY given a set of facts and a file of hints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(rules).
:- use_module(read_line).
:- use_module(library(pio)).
:- set_prolog_flag(double_quotes, codes).


  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The Hints 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generateRiddle(File) :-
   phrase_from_file(lines, File).
 
 % open(File, read, Stream),
  % read_lines(Stream),
 % close(Stream), nl.

lines --> call(eos), !.
lines --> line, { writeln('I found a line.') }, lines.

line --> ( "\n" ; call(eos) ), !.
line --> [_], line.

eos([], []).


% lines([])           --> call(eos), !.
% lines([Line|Lines]) --> read_line(Line), lines(Lines).
% eos([], []).
% line([])     --> ( "\n" ; call(eos) ), !.
% line([L|Ls]) --> [L], read_line(Ls).



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


  % ?- phrase(like(What), "I like it. Anything can follow!").
  % phrase_from_file(like(What), 'like.txt').
  % What = [i, t] ; false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The Hints 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generateRiddle(File) :-
  open(File,read,Str),
  read_hints(Str,Hints),
  close(Str),
  write(Hints),  nl. 
  
read_hints(Stream,[]):-
  at_end_of_stream(Stream).
   
read_hints(Stream,[X|L]):-
  \+  at_end_of_stream(Stream),
  readWord(Stream,X),
  read_hints(Stream,L).
  
readWord(InStream,W):-
  get_code(InStream,Char),
  checkCharAndReadRest(Char,Chars,InStream),
  atom_codes(W,Chars).
   
   
checkCharAndReadRest(10,[],_):-  !.
   
checkCharAndReadRest(32,[],_):-  !.
   
checkCharAndReadRest(-1,[],_):-  !.
   
checkCharAndReadRest(end_of_file,[],_):-  !.
   
checkCharAndReadRest(Char,[Char|Chars],InStream):-
  get_code(InStream,NextChar),
  checkCharAndReadRest(NextChar,Chars,InStream). 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Jessica Marie Barre
% Program Name: riddlegenerator.pl
% Date Due : Dec 1, 2016
% Purpose: Solve a riddle DYNAMICALLY given a set of facts and a file of hints


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT NOTES:
% - I was able to parse the file, but
% - I was not able to feed hints into system
% - To see that the second half of this program works, run student(Major).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(rules).
:- use_module(read_line).
:- use_module(library(pio)). 
:- use_module(grammar).
:- set_prolog_flag(double_quotes, codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Reading from File 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



generateRiddle(File) :-
  open(File,read,Stream),
  read_hints(Stream,Hints),
  process_hints(Hints).

read_hints(File, [L|Lines]) :-
    read_line_to_codes(File, Codes), Codes \= end_of_file,
    atom_codes(L, Codes),
    !, read_hints(File, Lines).
read_hints(_, []).


process_hints([]) :- !.
process_hints([Hint|Hints]) :-
             process_hint(Hint),
             process_hints(Hints).


process_hint(String) :-
  atom_chars(String, Chars),
  maplist(char_code, Chars, Codes),
  read_line(Hint, Codes), 
  
  % calling writeln(Hint) shows that the the line is properly parsed
  
  add(Hint). 
 

add(Hint):-
  writeln(Hint),
  sentence(Name,Subject,Object,Hint,[]), % HERE is the problem
  writeln("got here"),
  Fact=.. [Name,Subject,Object],
  writeln(Fact),
  add_rule(Students,Name,Subject,Object).

  feedHints(Students) :-
Hints = [ 
[the,architecture,major,occupies,the,office,with,the,ctrl+alt+del,comic,poster,'.'],
[the,cse,major,belongs,to,the,rpi,flying,club,'.'],
[the,gs,major,reads,sci,fi,'.'], [the,office,with,the,dilbert,comic,poster,is,on,the,left,of,the,office,with,the,calvin,and,hobbes,comic,poster,'.'],
[the,office,with,the,dilbert,comic,"poster's",occupant,reads,fantasy,'.'],
[the,student,who,eats,pepperoni,pizza,belongs,to,the,rcos,club,'.'],
[the,occupant,of,the,office,with,the,xkcd,comic,poster,eats,cheese,pizza,'.'],
[the,student,occupying,the,office,in,the,middle,reads,fiction,'.'],
[the,cs,major,occupies,the,first,office,'.'],
[the,student,who,eats,buffalo,chicken,pizza,occupies,the,office,next,to,the,one,who,belongs,to,the,r,gaming,alliance,club,'.'],
[the,student,who,belongs,to,the,cs,club,occupies,the,office,next,to,the,student,who,eats,cheese,pizza,'.'],
[the,student,who,eats,hawaiian,pizza,reads,poetry,'.'],
[the,itws,major,eats,broccoli,pizza,'.'],
[the,cs,major,occupies,the,office,next,to,the,one,with,the,phd,comics,comic,poster,'.'],
[the,student,who,eats,buffalo,chicken,pizza,has,an,office,neighbor,who,reads,history,'.']

],

feed(Students, Hints).
 
 
feed(_,[]).
feed(Students, [Hint|Hints]):-
  sentence(Name,Subject,Object,Hint,[]),
  add_rule(Students,Name,Subject,Object),
  feed(Students,Hints).
  
student(Major) :-
  feedHints(Students),
  % printStudents(Students, 1),
  % printStudents(Students, 2),
  % printStudents(Students, 3),
  % printStudents(Students, 4),
  % printStudents(Students, 5),
  exists(student(Major, _, _, _, taekwondo), Students).
  
% Debugging
printStudents(Students,1) :- Students = list(S,_,_,_,_), writeln(S).
printStudents(Students,2) :- Students = list(_,S,_,_,_), writeln(S).
printStudents(Students,3) :- Students = list(_,_,S,_,_), writeln(S).
printStudents(Students,4) :- Students = list(_,_,_,S,_), writeln(S).
printStudents(Students,5) :- Students = list(_,_,_,_,S), writeln(S).






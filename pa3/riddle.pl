%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Jessica Marie Barre
% Program Name: riddle.pl
% Date Due : Dec 1, 2016
% Purpose: Solve a riddle given a set of facts and hints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(rules).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  *SEE FACTS/RULES in rules.pl*
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Question: Who belongs to the Taekwondo club?
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

student(Major) :-
  hints(Students),
  exists(student(Major, _, _, _, taekwondo), Students).
  
% Debugging
printStudents(Students,1) :- Students = list(S,_,_,_,_), writeln(S).
printStudents(Students,2) :- Students = list(_,S,_,_,_), writeln(S).
printStudents(Students,3) :- Students = list(_,_,S,_,_), writeln(S).
printStudents(Students,4) :- Students = list(_,_,_,S,_), writeln(S).
printStudents(Students,5) :- Students = list(_,_,_,_,S), writeln(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The Hints  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hints(Students) :-

% The Architecture major occupies the office with the Ctrl+Alt+Del comic poster.
  exists(student(arch,ctrlaltdel,_,_,_), Students),
  
% The CSE major belongs to the RPI Flying Club.
  exists(student(cse,_,_,_,rpiflyingclub), Students),
  
% The GS major reads Sci Fi.
  exists(student(gs,_,scifi,_,_), Students),

% The office with the Dilbert comic poster is on the left of the office with the
% Calvin and Hobbes comic poster.   
  leftOf(student(_,dilbert,_,_,_), student(_,calvinhobbes,_,_,_), Students),

% The office with the Dilbert comic posters occupant reads Fantasy.
  exists(student(_,dilbert,fantasy,_,_), Students),

% The student who eats Pepperoni pizza belongs to the RCOS club.
% ^thats stereotyping!! lol.
  exists(student(_,_,_,pepperoni,rcosclub), Students),
 
% The occupant of the office with the xkcd comic poster eats Cheese pizza.
  exists(student(_,xkcd,_,cheese,_), Students),

% The student occupying the office in the middle reads Fiction.
  middle(student(_,_,fiction,_,_), Students),

% The CS major occupies the first office.
  first(student(cs,_,_,_,_), Students),

% The student who eats Buffalo Chicken pizza occupies the office next to the one
% who belongs to the R Gaming Alliance club.
  nextTo(student(_,_,_,buffalochicken,_), student(_,_,_,_,rgamingalliance), Students),

% The student who belongs to the CS club occupies the office next to the student
% who eats Cheese pizza.
  nextTo(student(_,_,_,_,csclub), student(_,_,_,cheese,_), Students),
% The student who eats Hawaiian pizza reads Poetry.
  exists(student(_,_,poetry,hawaiian,_), Students),
    
% The ITWS major eats Broccoli pizza.
  exists(student(itws,_,_,broccoli,_), Students),
    
% The CS major occupies the office next to the one with the PHD Comics comic poster.
  nextTo(student(cs,_,_,_,_), student(_,phdcomics,_,_,_), Students),
  
% The student who eats Buffalo Chicken pizza has an office neighbor who reads
% History.
  nextTo(student(_,_,_,buffalochicken,_), student(_,_,history,_,_), Students).






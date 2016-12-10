%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Jessica Marie Barre
% Program Name: riddle2.pl
% Date Due : Dec 1, 2016
% Purpose: Solve a riddle given a set of facts and hints (unfinished solution)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The Facts :
%
% There are 5 OFFICES with five different comic book POSTERS on the door.
% In each OFFICE is a STUDENT of a different MAJOR.
% These five STUDENTS each read a distinct book GENRE, eat a distinct type of      
%   PIZZA, and belong to a distinct CLUB. 
%
% Interpretation:
% Let students be a a list such that for each index I:
%  - I the position of the office in the list, and 
%  - the student is decribed by a distinct tuple of properties:
%     - (major, office poster, genre preference, pizza preference, club membership)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% a tuple containing all the information about each student in each office
students(0, []) :- !.
students(N, [(_Major,_Poster,_Genre,_Pizza,_Club)|T]) :- I is N-1, students(I,T).

% access the nth element of the list which corresponds with some information
nth_student(1, [H|_], H) :- !.
nth_student(N, [_|T], R) :- I is N-1, nth_student(I, T, R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Question: Who belongs to the Taekwondo club?
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
question([(Major,_,_,_,taekwondo)|_]) :- student(Major).
question([_|T]) :- question(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The Hints [Recall (_Major,_Poster,_Genre,_Pizza,_Club)]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The Architecture major occupies the office with the Ctrl+Alt+Del comic poster.
hint1([(arch,ctrlaltdel,_,_,_)|_]).
hint1([_|T]) :- hint1(T).

% The CSE major belongs to the RPI Flying Club.
hint2([(cse,_,_,_, rpiflying)|_]).
hint2([_|T]) :- hint2(T).

% The GS major reads Sci Fi.
hint3([(gs,_,scifi,_,_)|_]).
hint3([_|T]) :- hint3(T).

% The office with the Dilbert comic poster is on the left of the office with the
% Calvin and Hobbes comic poster.
hint4([(_,dilbert,_,_,_),(_,calvinhobbes,_,_,_)|_]).
hint4([_|T]) :- hint4(T).

% The office with the Dilbert comic posters occupant reads Fantasy.
hint5([(_,dilbert,fantasy,_,_)|_]).
hint5([_|T]) :- hint5(T).

% The student who eats Pepperoni pizza belongs to the RCOS club.
% ^thats stereotyping!! lol.
hint6([(_,_,_,pepperoni,rcos)|_]).
hint6([_|T]) :- hint6(T).
 
% The occupant of the office with the xkcd comic poster eats Cheese pizza.
hint7([(_,xkcd,_,cheese,_)|_]).
hint7([_|T]) :- hint7(T).

% The student occupying the office in the middle reads Fiction.
hint8(Students) :- nth_student(3, Students, (_,_,fiction,_,_)).

% The CS major occupies the first office.
hint9(Students) :- nth_student(1, Students, (cs,_,_,_,_)).

% The nth_student who eats Buffalo Chicken pizza occupies the office next to the one
% who belongs to the R Gaming Alliance club.
hint10([(_,_,_,buffalochicken,_),(_,_,_,_,rgamingalliance)|_]).
hint10([(_,_,_,_,rgamingalliance),(_,_,_,buffalochicken,_)|_]).
hint10([_|T]) :- hint10(T).

% The student who belongs to the CS club occupies the office next to the student
% who eats Cheese pizza.
hint11([(_,_,_,cheese,_),(_,_,_,_,cslub)|_]).
hint11([(_,_,_,_,csclub),(_,_,_,cheese,_)|_]).
hint11([_|T]) :- hint11(T).
    
% The student who eats Hawaiian pizza reads Poetry.
hint12([(_,_,poetry,hawaiian,_)|_]).
hint12([_|T]) :- hint12(T).
    
% The ITWS major eats Broccoli pizza.
hint13([(itws,_,_,broccoli,_)|_]).
hint13([_|T]) :- hint13(T).
    
% The CS major occupies the office next to the one with the PHD Comics comic poster.
hint14([(cs,_,_,_,_),(_,phdcomics,_,_,_)|_]).
hint14([(_,phdcomics,_,_,_),(cs,_,_,_,_)|_]).
hint14([_|T]) :- hint14(T).
    
% The student who eats Buffalo Chicken pizza has an office neighbor who reads
% History.
hint15([(_,_,_,buffalochicken,_),(_,_,history,_,_)|_]).
hint15([(_,_,history,_,_),(_,_,_,buffalochicken,_)|_]).
hint15([_|T]) :- hint15(T).

solution(Students) :-
  students(5, Students),
  hint1(Students),
  hint2(Students),
  hint3(Students),
  hint4(Students),
  hint5(Students),
  hint6(Students),
  hint7(Students),
  hint8(Students),
  hint9(Students),
  hint10(Students),
  hint11(Students),
  hint12(Students),
  hint13(Students),
  hint14(Students),
  hint15(Students),
  question(Students).

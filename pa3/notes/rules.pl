%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Jessica Marie Barre
% Program Name: rules.pl
% Date Due : Dec 1, 2016
% Purpose: Interprets a list of facts for general use
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(rules, [exists/2, leftOf/3, rightOf/3, middle/2, first/2, nextTo/3]). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The Facts :
%
% There are 5 OFFICES with five different comic book POSTERS on the door.
% In each OFFICE is a STUDENT of a different MAJOR.
% These five STUDENTS each read a distinct book GENRE, eat a distinct type of      
%   PIZZA, and belong to a distinct CLUB. 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% The Representation:
%
%    a list of offices: (i,_,_,_,_), such that at each position i
%
%    is a student defined as: student(Major,Poster,Genre,Pizza,Club)
%
% The Rules are:
%   exists(student(_,_,_,_,_), Students)
%   leftOf(student(_,_,_,_,_), student(_,_,_,_,_), Students)
%   rightOf(student(_,_,_,_,_), student(_,_,_,_,_), Students)
%   middle(student(_,_,_,_,_), Students)
%   first(student(_,_,_,_,_), Students)
%   nextTo(student(_,_,_,_,_), student(_,_,_,_,_), Students)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% In the following procedures let:
%   S, S1, S2 represent a student tuple
%   L represent left of, and
%   R represent right of

exists(S, list(S, _, _, _, _)).
exists(S, list(_, S, _, _, _)).
exists(S, list(_, _, S, _, _)).
exists(S, list(_, _, _, S, _)).
exists(S, list(_, _, _, _, S)).

rightOf(R, S, list(S, R, _, _, _)).
rightOf(R, S, list(_, S, R, _, _)).
rightOf(R, S, list(_, _, S, R, _)).
rightOf(R, S, list(_, _, _, S, R)).

leftOf(L, S, list(L, S, _, _, _)).
leftOf(L, S, list(_, L, S, _, _)).
leftOf(L, S, list(_, _, L, S, _)).
leftOf(L, S, list(_, _, _, L, S)).

middle(S, list(_, _, S, _, _)).

first(S, list(S, _, _, _, _)).

last(S, list(_, _, _, _, S)).

nextTo(S1, S2, list(S1, S2, _, _, _)).
nextTo(S1, S2, list(_, S1, S2, _, _)).
nextTo(S1, S2, list(_, _, S1, S2, _)).
nextTo(S1, S2, list(_, _, _, S1, S2)).
nextTo(S1, S2, list(S2, S1, _, _, _)).
nextTo(S1, S2, list(_, S2, S1, _, _)).
nextTo(S1, S2, list(_, _, S2, S1, _)).
nextTo(S1, S2, list(_, _, _, S2, S1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  read_line
%%%  Similar to read_sent in Pereira and Shieber, Prolog and
%%%        Natural Language Analysis, CSLI, 1987.
%%%
%%%  Examples:
%%%           % read_line(L).
%%%           The sky was blue, after the rain.
%%%           L = [the,sky,was,blue,',',after,the,rain,'.']
%%%           % read_line(L).
%%%           Which way to the beach?
%%%           L = [which,way,to,the, beach,'?']
%%%

:- module(read_line, [read_line/2]).

% Edited by Jessica Marie Barre to manipulate a string rather than read an io stream

read_line(Words, [C|Tail]) :- read_rest(C, Tail, Words).
                              
/* A period or question mark ends the input. */
read_rest(46,_,['.']) :- !.
read_rest(63,_,['?']) :- !.

/* Spaces and newlines between words are ignored. */
read_rest(C,[C1|Tail],Words) :- ( C=32 ; C=10 ) , !,
                                read_rest(C1, Tail, Words).

/* Commas between words are absorbed. */
read_rest(44,[C1|Tail],[','|Words]) :- !,
                                read_rest(C1, Tail, Words).

/* Otherwise get all of the next word. */
read_rest(C,Tail,[Word|Words]) :- 
                             read_word(C,Tail,Chars,Next, NewTail),
                             name(Word,Chars),
                             read_rest(Next, NewTail, Words).

/* Space, comma, newline, period or question mark separate words. */
read_word(C,Tail,[],C, Tail) :- ( C=32 ; C=44 ; C=10 ;
                         C=46 ; C=63) , !.

/* Otherwise, get characters, convert alpha to lower case. */
read_word(C,[Next|Tail],[LC|Chars],Last, NewTail) :- 
                                lower_case(C,LC),
                                read_word(Next,Tail,Chars,Last, NewTail).

/* Convert to lower case if necessary. */
lower_case(C,C) :- ( C <  65 ; C > 90 ) , !.
lower_case(C,LC) :- LC is C + 32.

/* for reference ... 
newline(10).
comma(44).
space(32).
period(46).
question_mark(63).
*/

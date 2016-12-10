major(cs).
major(itws).
major(cse).
major(gs).
major(architecture).

poster(dilbert).
poster(calvinandhobbes).
poster(phdcomics).
poster(ctrlaltdel).
poster(xkcd).

genre(fiction).
genre(history).
genre(fantasy).
genre(scifi).
genre(poetry).

pizza(hawaiian).
pizza(cheese).
pizza(pepperoni).
pizza(buffalochicken).
pizza(broccoli). 

club(csclub).
club(gaming).
club(flying).
club(rcos).
club(taekwondo).

office([student(_, _, _, _, _),student(_, _, _, _, _),student(_, _, _, _, _),student(_, _, _, _,_),student(_, _, _, _, _)]).

/* Additional facts */
%student(cs,phdcomics,_,_,_).
%student(cse,_,_,_,rcos).
%student(itws,_,fantasy,_,_).
%student(_,dilbert,hawaiian,_,_).
%student(_,_,_,pepperoni,gaming).
%student(_,calvinandhobbes,_,cheese,_).
%student(_,_,poetry,_,_).
%student(architecture,...).

/* Rules */
% student(nationality,color,drink,cigarette,pet)
student(A,B,C,D,E):-major(A),poster(B),genre(C),pizza(D),club(E).
student(Major) :- student(Major,_,_,_,taekwondo). 

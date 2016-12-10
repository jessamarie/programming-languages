:- use_module(rules).

  
/* maj(Major) :- student(Major, [cs, itws, cse, gs, architecture]).
post(Poster) :- student(Poster, [calvinandhobbes, dilbert, xkcd, ctrlaltdelete, phd]).
gen(Genre) :- student(Genre, [fish, dogs, birds, horses, cats]).
pizz(Pizza) :- student(Pizza, [tea, coffee, milk, beer, water]).
clu(Club) :- student(Club, [pall_mall, dunhill, blends, prince, blue_master]).
 


offices([
	student(_, _, _, _, _),
	student(_, _, _, _, _),
	student(_, _, _, _, _),
	student(_, _, _, _, _),
	student(_, _, _, _, _)
]). */


feedHints() :-
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

feed(Students, Hints),
writeln(Students),nl.
 
 
feed(_,[]).
feed(Students, [Hint|Hints]):-
  sentence(Name,Subject,Object,Hint,[]),
  Fact =.. [Name,Subject,Object],
  writeln(Fact),
  add_rule(Students,Name,Subject,Object),
  feed(Students,Hints).


sentence(Name,Subject,Object) --> noun_phrase(Subject), verb_phrase(Name,Object).

noun_phrase(_) --> punctuation.
noun_phrase(Subject) --> det, noun_phrase(Subject).
noun_phrase(Object) --> noun(Object), punctuation.
noun_phrase(Object) --> noun(Object).
noun_phrase(Object) --> det, noun(_), rel_clause(_,Object).
noun_phrase(Subject) --> det,noun(_), prep_phrase(Subject).
noun_phrase(Subject) --> det,noun(_), gerund_phrase(Subject).
noun_phrase(Name) --> det,direction(Name),noun_phrase(_).

verb_phrase(_,_) --> punctuation.
verb_phrase(Name,Object) --> rel_clause(Name,Object).
verb_phrase(Name, Object) -->  trans_verb(_), noun_phrase(_), direction(Name), rel_clause(_,Object).
verb_phrase(Name, Object) -->  trans_verb(_), noun_phrase(_), direction(Name), noun_phrase(Object).
verb_phrase(Name, Object) -->  trans_verb(Name),  noun_phrase(Object).
verb_phrase(Name, Object) -->  trans_verb(_), prep_phrase(Name), prep_phrase(Object).

rel_clause(Name,Object) --> rel_pronoun, verb_phrase(Name,Object).

prep_phrase(Subject) --> prep, noun_phrase(Subject).
prep_phrase(Name) --> prep, det, direction(Name).

gerund_phrase(Subject) --> gerund, noun_phrase(Subject).

trans_verb(reads) --> [reads].
trans_verb(eats) --> [eats].
trans_verb(belongs) --> [belongs, to].
trans_verb(occupies) --> [occupies].
trans_verb(is) --> [is].
trans_verb(has) --> [has].


gerund --> [occupying].

prep --> [with].
prep --> [in].
prep --> [of].
prep --> [on].

direction(left) --> [left].
direction(right) --> [right].
direction(middle) --> [middle].
direction(neighbor) --> [next,to] | [neighbor].
direction(first) --> [first].
direction(last) --> [last].

noun(student) --> [student] | [one] | [occupant].
noun(office) --> [office].

noun(architecture) --> [architecture, major].
noun(itws) --> [itws, major].
noun(cs) --> [cs, major].
noun(gs) --> [gs, major].
noun(cse) --> [cse, major].

%handle possesives?
noun(dilbert) --> [dilbert,comic,poster] | [dilbert,comic,"poster's", occupant].
noun(calvinandhobbes) --> [calvin,and,hobbes,comic,poster] | [calvin,and,hobbes,comic,"poster's",occupant].
noun(phdcomics) --> [phd,comics,comic,poster] |  [phd,comic,"poster's",occupant].
noun(ctrlaltdel) --> [ctrl+alt+del,comic,poster] |  [ctrl+alt+del,comic,"poster's", occupant].
noun(xkcd) --> [xkcd,comic,poster] | [xkcd,phd,comic,"poster's", occupant].

noun(fiction) --> [fiction].
noun(history) --> [history].
noun(fantasy) --> [fantasy].
noun(scifi) --> [sci,fi].
noun(poetry) --> [poetry].

noun(hawaiian) --> [hawaiian, pizza].
noun(cheese) --> [cheese, pizza].
noun(pepperoni) --> [pepperoni, pizza].
noun(buffalochicken) --> [buffalo, chicken, pizza].
noun(broccoli) --> [broccoli, pizza].

noun(csclub) --> [cs,club].
noun(gaming) --> [r,gaming,alliance,club].
noun(flying) --> [rpi,flying,club].
noun(rcos) --> [rcos,club].
noun(taekwondo) --> [taekwondo,club].

rel_pronoun --> [who] |[that].

det --> [a].
det --> [an].
det --> [the].


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


punctuation --> ['.'].
punctuation --> ['?'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The following are all the rules needed to match the students with the proper office.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%student(Major, Poster, Genre, Pizza, Club) :- major(Major), poster(Poster), genre(Genre), pizza(Pizza), club%(Club). 

%student(Major) :- major(Major), occupies(Major, taekwondo).
  


%%%%%%%%%%%%% exists %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_rule(Students,occupies,M,P) :- major(M), poster(P), exists(student(M,P,_,_,_), Students).
add_rule(Students,occupies,G,P) :- genre(G), poster(P), exists(student(_,P,G,_,_), Students).
add_rule(Students,occupies,Q,P) :- pizza(Q), poster(P), exists(student(_,P,_,Q,_), Students).
add_rule(Students,occupies,C,P) :- club(C),  poster(P), exists(student(_,P,_,_,C), Students).

add_rule(Students,reads,M,G) :- major(M),  genre(G), exists(student(M,_,G,_,_), Students).
add_rule(Students,reads,P,G) :- poster(P), genre(G), exists(student(_,P,G,_,_), Students).
add_rule(Students,reads,P,G) :- pizza(P),  genre(G), exists(student(_,_,G,P,_), Students).
add_rule(Students,reads,C,G) :- club(C),   genre(G), exists(student(_,_,G,_,C), Students).

add_rule(Students,eats,M,P) :- major(M), pizza(P),  exists(student(M,_,_,P,_), Students).
add_rule(Students,eats,S,P) :- poster(S), pizza(P), exists(student(_,S,_,P,_), Students).
add_rule(Students,eats,G,P) :- genre(G), pizza(P),  exists(student(_,_,G,P,_), Students).
add_rule(Students,eats,C,P) :- club(C), pizza(P),   exists(student(_,_,_,P,C), Students).

add_rule(Students,belongs,M,C) :- major(M), club(C),  exists(student(M,_,_,_,C), Students).
add_rule(Students,belongs,P,C) :- poster(P), club(C), exists(student(_,P,_,_,C), Students).
add_rule(Students,belongs,G,C) :- genre(G), club(C),  exists(student(_,_,G,_,C), Students).
add_rule(Students,belongs,P,C) :- pizza(P), club(C),  exists(student(_,_,_,P,C), Students).


%%%%%%%%%%%%%%%%%% next to, or Neighbor of %%%%%%%%%%%%%%%%%%%%%%%%%

add_rule(Students,neighbor,M,N) :- major(M), major(N),  nextTo(student(M,_,_,_,_),student(N,_,_,_,_), Students).
add_rule(Students,neighbor,M,P) :- major(M), poster(P), nextTo(student(M,_,_,_,_),student(_,P,_,_,_), Students).
add_rule(Students,neighbor,M,G) :- major(M), genre(G),  nextTo(student(M,_,_,_,_),student(_,_,G,_,_), Students).
add_rule(Students,neighbor,M,P) :- major(M), pizza(P),  nextTo(student(M,_,_,_,_),student(_,_,_,P,_), Students).
add_rule(Students,neighbor,M,C) :- major(M), club(C),   nextTo(student(M,_,_,_,_),student(_,_,_,_,C), Students).

add_rule(Students,neighbor,P,M) :- poster(P),  major(M),  nextTo(student(_,P,_,_,_),student(M,_,_,_,_), Students).
add_rule(Students,neighbor,P,Q) :- poster(P),  poster(Q), nextTo(student(_,P,_,_,_),student(_,Q,_,_,_), Students).
add_rule(Students,neighbor,P,G) :- poster(P),  genre(G),  nextTo(student(_,P,_,_,_),student(_,_,G,_,_), Students).
add_rule(Students,neighbor,P,Q) :- poster(P),  pizza(Q),  nextTo(student(_,P,_,_,_),student(_,_,_,Q,_), Students).
add_rule(Students,neighbor,P,C) :- poster(P),  club(C),   nextTo(student(_,P,_,_,_),student(_,_,_,_,C), Students).

add_rule(Students,neighbor,G,M) :- genre(G), major(M),  nextTo(student(_,_,G,_,_),student(M,_,_,_,_), Students).
add_rule(Students,neighbor,G,P) :- genre(G), poster(P), nextTo(student(_,_,G,_,_),student(_,P,_,_,_), Students).
add_rule(Students,neighbor,G,F) :- genre(G), genre(F),  nextTo(student(_,_,G,_,_),student(_,_,F,_,_), Students).
add_rule(Students,neighbor,G,P) :- genre(G), pizza(P),  nextTo(student(_,_,G,_,_),student(_,_,_,P,_), Students).
add_rule(Students,neighbor,G,C) :- genre(G), club(C),   nextTo(student(_,_,G,_,_),student(_,_,_,_,C), Students).

add_rule(Students,neighbor,P,M) :- pizza(P), major(M),  nextTo(student(_,_,_,P,_),student(M,_,_,_,_), Students).
add_rule(Students,neighbor,P,Q) :- pizza(P), poster(Q), nextTo(student(_,_,_,P,_),student(_,Q,_,_,_), Students).
add_rule(Students,neighbor,P,G) :- pizza(P), genre(G),  nextTo(student(_,_,_,P,_),student(_,_,G,_,_), Students).
add_rule(Students,neighbor,P,Q) :- pizza(P), pizza(Q),  nextTo(student(_,_,_,P,_),student(_,_,_,Q,_), Students).
add_rule(Students,neighbor,P,C) :- pizza(P), club(C),   nextTo(student(_,_,_,P,_),student(_,_,_,_,C), Students).

add_rule(Students,neighbor,C,M) :- club(C), major(M),  nextTo(student(_,_,_,_,C),student(M,_,_,_,_), Students).
add_rule(Students,neighbor,C,P) :- club(C), poster(P), nextTo(student(_,_,_,_,C),student(_,P,_,_,_), Students).
add_rule(Students,neighbor,C,G) :- club(C), genre(G),  nextTo(student(_,_,_,_,C),student(_,_,G,_,_), Students).
add_rule(Students,neighbor,C,P) :- club(C), pizza(P),  nextTo(student(_,_,_,_,C),student(_,_,_,P,_), Students).
add_rule(Students,neighbor,C,D) :- club(C), club(D),   nextTo(student(_,_,_,_,C),student(_,_,_,_,D), Students).


%%%%%%%%%%%%% Right of %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_rule(Students,right,M,N) :- major(M), major(N),  rightOf(student(M,_,_,_,_),student(N,_,_,_,_), Students).
add_rule(Students,right,M,P) :- major(M), poster(P), rightOf(student(M,_,_,_,_),student(_,P,_,_,_), Students).
add_rule(Students,right,M,G) :- major(M), genre(G),  rightOf(student(M,_,_,_,_),student(_,_,G,_,_), Students).
add_rule(Students,right,M,P) :- major(M), pizza(P),  rightOf(student(M,_,_,_,_),student(_,_,_,P,_), Students).
add_rule(Students,right,M,C) :- major(M), club(C),   rightOf(student(M,_,_,_,_),student(_,_,_,_,C), Students).

add_rule(Students,right,P,M) :- poster(P),  major(M),  rightOf(student(_,P,_,_,_),student(M,_,_,_,_), Students).
add_rule(Students,right,P,Q) :- poster(P),  poster(Q), rightOf(student(_,P,_,_,_),student(_,Q,_,_,_), Students).
add_rule(Students,right,P,G) :- poster(P),  genre(G),  rightOf(student(_,P,_,_,_),student(_,_,G,_,_), Students).
add_rule(Students,right,P,Q) :- poster(P),  pizza(Q),  rightOf(student(_,P,_,_,_),student(_,_,_,Q,_), Students).
add_rule(Students,right,P,C) :- poster(P),  club(C),   rightOf(student(_,P,_,_,_),student(_,_,_,_,C), Students).

add_rule(Students,right,G,M) :- genre(G), major(M),  rightOf(student(_,_,G,_,_),student(M,_,_,_,_), Students).
add_rule(Students,right,G,P) :- genre(G), poster(P), rightOf(student(_,_,G,_,_),student(_,P,_,_,_), Students).
add_rule(Students,right,G,F) :- genre(G), genre(F),  rightOf(student(_,_,G,_,_),student(_,_,F,_,_), Students).
add_rule(Students,right,G,P) :- genre(G), pizza(P),  rightOf(student(_,_,G,_,_),student(_,_,_,P,_), Students).
add_rule(Students,right,G,C) :- genre(G), club(C),   rightOf(student(_,_,G,_,_),student(_,_,_,_,C), Students).

add_rule(Students,right,P,M) :- pizza(P), major(M),  rightOf(student(_,_,_,P,_),student(M,_,_,_,_), Students).
add_rule(Students,right,P,Q) :- pizza(P), poster(Q), rightOf(student(_,_,_,P,_),student(_,Q,_,_,_), Students).
add_rule(Students,right,P,G) :- pizza(P), genre(G),  rightOf(student(_,_,_,P,_),student(_,_,G,_,_), Students).
add_rule(Students,right,P,Q) :- pizza(P), pizza(Q),  rightOf(student(_,_,_,P,_),student(_,_,_,Q,_), Students).
add_rule(Students,right,P,C) :- pizza(P), club(C),   rightOf(student(_,_,_,P,_),student(_,_,_,_,C), Students).

add_rule(Students,right,C,M) :- club(C), major(M),  rightOf(student(_,_,_,_,C),student(M,_,_,_,_), Students).
add_rule(Students,right,C,P) :- club(C), poster(P), rightOf(student(_,_,_,_,C),student(_,P,_,_,_), Students).
add_rule(Students,right,C,G) :- club(C), genre(G),  rightOf(student(_,_,_,_,C),student(_,_,G,_,_), Students).
add_rule(Students,right,C,P) :- club(C), pizza(P),  rightOf(student(_,_,_,_,C),student(_,_,_,P,_), Students).
add_rule(Students,right,C,D) :- club(C), club(D),   rightOf(student(_,_,_,_,C),student(_,_,_,_,D), Students).

%%%%%%%%%%%%% Left of %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


add_rule(Students,left,M,N) :- major(M), major(N),  leftOf(student(M,_,_,_,_),student(N,_,_,_,_), Students).
add_rule(Students,left,M,P) :- major(M), poster(P), leftOf(student(M,_,_,_,_),student(_,P,_,_,_), Students).
add_rule(Students,left,M,G) :- major(M), genre(G),  leftOf(student(M,_,_,_,_),student(_,_,G,_,_), Students).
add_rule(Students,left,M,P) :- major(M), pizza(P),  leftOf(student(M,_,_,_,_),student(_,_,_,P,_), Students).
add_rule(Students,left,M,C) :- major(M), club(C),   leftOf(student(M,_,_,_,_),student(_,_,_,_,C), Students).

add_rule(Students,left,P,M) :- poster(P),  major(M),  leftOf(student(_,P,_,_,_),student(M,_,_,_,_), Students).
add_rule(Students,left,P,Q) :- poster(P),  poster(Q), leftOf(student(_,P,_,_,_),student(_,Q,_,_,_), Students).
add_rule(Students,left,P,G) :- poster(P),  genre(G),  leftOf(student(_,P,_,_,_),student(_,_,G,_,_), Students).
add_rule(Students,left,P,Q) :- poster(P),  pizza(Q),  leftOf(student(_,P,_,_,_),student(_,_,_,Q,_), Students).
add_rule(Students,left,P,C) :- poster(P),  club(C),   leftOf(student(_,P,_,_,_),student(_,_,_,_,C), Students).

add_rule(Students,left,G,M) :- genre(G), major(M),  leftOf(student(_,_,G,_,_),student(M,_,_,_,_), Students).
add_rule(Students,left,G,P) :- genre(G), poster(P), leftOf(student(_,_,G,_,_),student(_,P,_,_,_), Students).
add_rule(Students,left,G,F) :- genre(G), genre(F),  leftOf(student(_,_,G,_,_),student(_,_,F,_,_), Students).
add_rule(Students,left,G,P) :- genre(G), pizza(P),  leftOf(student(_,_,G,_,_),student(_,_,_,P,_), Students).
add_rule(Students,left,G,C) :- genre(G), club(C),   leftOf(student(_,_,G,_,_),student(_,_,_,_,C), Students).

add_rule(Students,left,P,M) :- pizza(P), major(M),  leftOf(student(_,_,_,P,_),student(M,_,_,_,_), Students).
add_rule(Students,left,P,Q) :- pizza(P), poster(Q), leftOf(student(_,_,_,P,_),student(_,Q,_,_,_), Students).
add_rule(Students,left,P,G) :- pizza(P), genre(G),  leftOf(student(_,_,_,P,_),student(_,_,G,_,_), Students).
add_rule(Students,left,P,Q) :- pizza(P), pizza(Q),  leftOf(student(_,_,_,P,_),student(_,_,_,Q,_), Students).
add_rule(Students,left,P,C) :- pizza(P), club(C),   leftOf(student(_,_,_,P,_),student(_,_,_,_,C), Students).

add_rule(Students,left,C,M) :- club(C), major(M),  leftOf(student(_,_,_,_,C),student(M,_,_,_,_), Students).
add_rule(Students,left,C,P) :- club(C), poster(P), leftOf(student(_,_,_,_,C),student(_,P,_,_,_), Students).
add_rule(Students,left,C,G) :- club(C), genre(G),  leftOf(student(_,_,_,_,C),student(_,_,G,_,_), Students).
add_rule(Students,left,C,P) :- club(C), pizza(P),  leftOf(student(_,_,_,_,C),student(_,_,_,P,_), Students).
add_rule(Students,left,C,D) :- club(C), club(D),   leftOf(student(_,_,_,_,C),student(_,_,_,_,D), Students).


%%%%%%%%%%%%% First %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_rule(Students,_,first,M) :- major(M), middle(student(M,_,_,_,_), Students).
add_rule(Students,_,first,G) :- genre(G),    middle(student(_,_,G,_,_), Students).
add_rule(Students,_,first,P) :- pizza(P),     middle(student(_,_,_,P,_), Students).
add_rule(Students,_,first,C) :- club(C),   middle(student(_,_,_,_,C), Students).
add_rule(Students,_,M,first) :- major(M), middle(student(M,_,_,_,_), Students).
add_rule(Students,_,G,first) :- genre(G),    middle(student(_,_,G,_,_), Students).
add_rule(Students,_,P,first) :- pizza(P),     middle(student(_,_,_,P,_), Students).
add_rule(Students,_,C,first) :- club(C),   middle(student(_,_,_,_,C), Students).


%%%%%%%%%%%%% Last %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_rule(Students,_,last,M) :- major(M), middle(student(M,_,_,_,_), Students).
add_rule(Students,_,last,G) :- genre(G),    middle(student(_,_,G,_,_), Students).
add_rule(Students,_,last,P) :- pizza(P),     middle(student(_,_,_,P,_), Students).
add_rule(Students,_,last,C) :- club(C),   middle(student(_,_,_,_,C), Students).
add_rule(Students,_,M,last) :- major(M), middle(student(M,_,_,_,_), Students).
add_rule(Students,_,G,last) :- genre(G),    middle(student(_,_,G,_,_), Students).
add_rule(Students,_,P,last) :- pizza(P),     middle(student(_,_,_,P,_), Students).
add_rule(Students,_,C,last) :- club(C),   middle(student(_,_,_,_,C), Students).

%%%%%%%%%%%%% Middle %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_rule(Students,_,middle,M) :- major(M), middle(student(M,_,_,_,_), Students).
add_rule(Students,_,middle,G) :- genre(G),    middle(student(_,_,G,_,_), Students).
add_rule(Students,_,middle,P) :- pizza(P),     middle(student(_,_,_,P,_), Students).
add_rule(Students,_,middle,C) :- club(C),   middle(student(_,_,_,_,C), Students).
add_rule(Students,_,M,middle) :- major(M), middle(student(M,_,_,_,_), Students).
add_rule(Students,_,G,middle) :- genre(G),    middle(student(_,_,G,_,_), Students).
add_rule(Students,_,P,middle) :- pizza(P),     middle(student(_,_,_,P,_), Students).
add_rule(Students,_,C,middle) :- club(C),   middle(student(_,_,_,_,C), Students).

Author            : Jessica Marie Barre
Due Date          : Thursday, 12/01, 7:00PM
Extended Due Date : Friday, 12/02, 7:00PM
Date Submitted    : Sunday, 12/04
Late Days Used    : 2 


Features:
- rules.pl defines the list to hold the students, and methods nextTo,Left,Right,etc... to find
  the proper placements of the students.
  
- riddle.pl works, the student(Major) it returns is itws
  - be sure to compile [riddle] and [rules]
  - no bugs
  
- riddlegenerator.pl does not fully work (see bugs). 
    Features:
    - Be sure to compile [riddlegenerator], [grammar],[read_line], and [rules]
    - run student(major) to test the grammars.
    - Parsing a file works, but the [type of the?] parsed sentences are not compatible with my grammar parser
    - I manually created a list of parsed sentences and fed them into the grammar parser
    Bugs:
    - after calling riddleGenerator("hints.txt"), the lines are properly parsed, however
      once the add(Hint) predicate is reached, calling sentence(Name,Subject,Object,Hint,[])
      returns false, even though the Hint is parsed correctly. I am assuming this is a type
      error which I cannot figure out.
    - student(Major) populates the database with the list of hints I manually created. I did
      this in order to show that at least the grammar parsing works.
  
- grammar.pl is an extra module which contains all the context-free grammar and extra rules for 
  part 2 of the homework.
  - you can test it with sentence(Name,Sub,Obj,[Put array of words here],[]).
  - for example, sentence(Name,Sub,Obj,[the,cse,major,belongs,to,the,rcos,club,'.'],[]).
    will return N=belongs Sub=cse, Obj=rcos
  - Name, Sub, and Obj are used in the add_rule(Students, Name,sub,obj) predicate, which 
    adds to the knowledge base about the hint

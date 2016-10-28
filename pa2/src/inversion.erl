%% @author Jessica Marie Barre
%% @doc @todo description

-module(inversion).
-export([run/0]).


%%----------------------------------------------------------------------
%% Function: run/2
%% Purpose: runs the program
%% Args:   InputSequence  : A list of DNA chemical bases that are out of order
%%         TargetSequence : A list of DNA chemicals bases in the proper order
%%----------------------------------------------------------------------
run() ->
  InputSequence = "agga",
  TargetSequence = "gaag",
  %io:format("list starts as ~w ~n",[InputSequence]),
  Seq = map_get_sequence(fun get_index/2, TargetSequence, InputSequence, 1),
  %io:format("Final sequence: ~w ~n",[Seq]),
  {Merged, Count} = mergesort(Seq, 0),
  io:format("Inversion Count : ~w ~n",[Count]).


%%----------------------------------------------------------------------
%% Function: map_get_sequence/4
%% Purpose: Maps a function to iterate over a list
%% Args:   F     : the function to map_get_sequence (get_index)
%%         [H|T] : The list to iterate over to the function (TargetSequence)
%%         List  : The list to send to the function (InputSequence)
%%         N     : The index of the element at [List]
%% Returns: A list which replaces elements in the InputSequence with 
%%          an index of the correct location
%% Example: Input = agga, Node = g, N=1
%%  Effect: I = 2, Input = a1ga
%%----------------------------------------------------------------------
map_get_sequence(_, [], List, _)    -> List;
map_get_sequence(F, [H|T], List, N) -> 
  I = F(H, List),
  %io:format("Target: ~c = ~w index: ~w ~n",[H, N, I]), 
  NewList = replace_nth_node(I, N, List),
  %io:format("Our list is now ~w ~n",[NewList]),
  map_get_sequence(F, T, NewList, N+1).


%%----------------------------------------------------------------------
%% Function: get_index/2
%% Purpose: Finds the index I of the first occurence of Node in InputSeq 
%%          and replaces the element at I with N
%% Args:   InputSeq : The input sequence
%%         Node     : The current node from the TargetSequence
%% Returns: The index of the first occurence of Node 
%%----------------------------------------------------------------------
get_index(Node, InputSeq) -> 
  string:chr(InputSeq, Node).


%%----------------------------------------------------------------------
%% Function: get_inversion_count/2
%% Purpose:  counts the number of inversions in a sequence
%% Args:  [H|T] : The sequence to compare
%%        Count : Holds the current number of inversions
%% Returns: The total inversion count
%%----------------------------------------------------------------------
get_inversion_count([H|T], Count) -> 
  Count2 = compare(H,T,Count),
  get_inversion_count(T,Count2);
get_inversion_count([], Count) -> Count.
  

%%----------------------------------------------------------------------
%% Function: compare/3
%% Purpose:  counts pairwise inversions
%% Args:  H1     : The Node to compare 
%%        [H2|T] : The sequence to compare
%%        Count  : Holds the number of inversions
%% Returns: Count
%%----------------------------------------------------------------------
compare(_,  [], Count) -> Count;
compare(H1, [H2|T], Count) -> 
  if
    H1 > H2  ->  compare(H1, T, Count+1);
	true -> compare(H1, T, Count)
  end.


%%----------------------------------------------------------------------
%% Function: replace_nth_node/3
%% Purpose:  replaces the element at N in List with I
%% Args:    N     : The index to replace in the list
%%          I     : The item to replace the element at N with 
%%                  (The index corresponding with TargetSequence)
%%          [H|T] : The list to replace at N
%% Returns: the new list with the element at N replaced with I.
%%----------------------------------------------------------------------
replace_nth_node(_, _, []) -> [];
replace_nth_node(1, I, [_|T]) -> [I|T];
replace_nth_node(N, I, [H|T]) -> [H | replace_nth_node(N-1,I, T)].




%%%%% Unused functions %%%%%%%%

%%% This mergesort and merge code is modeled after:
% https://github.com/hugopeixoto/mergesort/blob/master/erlang/mergesort.erl
mergesort([], Count) -> {[], Count};
mergesort([E], Count) -> {[E], Count};
mergesort(List, Count) ->
  {A, B} = lists:split(trunc(length(List)/2), List),
   io:format("Split ~w ~w ~w ~n",[A, B, Count]),

  {Left, CountA} = mergesort(A,Count),
  {Right, CountB} = mergesort(B,Count),
  
  CountC = CountA + CountB,
  {Merged, TotalCount} = merge(Left, Right, CountC).

%io:format("Merged ~w ~w ~n",[Merged, TotalCount]).

% Merges
%
% [List1] : The first half of the unordered sequence
% [List2] : The second half of the unordered sequence
merge(A, [], Count) -> {A, Count};
merge([], B, Count) -> {B, Count};
merge([Ha|Ta], [Hb|Tb], Count) when Ha < Hb ->
	{List, CountTwo} = merge(Ta, [Hb|Tb], Count),
	{[Ha|List], CountTwo};
merge([Ha|Ta], [Hb|Tb], Count) ->
	{List, CountTwo} = merge([Ha|Ta], Tb, Count+1),
	{[Hb|List], CountTwo}.

%% Provided in homework details
get_random_node() ->
  get_nth_node(rand:uniform(length(net_adm:world())), net_adm:world()).

get_nth_node(1, [H|_]) -> H;
get_nth_node(_, [H|[]]) -> H;
get_nth_node(N, [_|T]) -> get_nth_node(N-1, T).

remove_nth_node(_, []) -> [];
remove_nth_node(1, [_|T]) -> T;
remove_nth_node(N, [H|T]) -> [H | remove_nth_node(N-1, T)].


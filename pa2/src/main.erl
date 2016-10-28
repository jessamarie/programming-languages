-module(main).
-compile(export_all).

do_work(StartSeq, TargetSeq, PID) ->
  Node = get_random_node(),
  
  spawn(Node, main, run, [StartSeq, TargetSeq, self()]),
  timer:sleep(10000),
  receive Inversions ->
	io:format("Inversion Count: ~w ~n",[Inversions])
  end,
  PID ! {ok, 4}.
  
wait_for_done() ->
  receive
    {ok, Result} -> io:fwrite("~B~n", [Result])
  end.

start() ->
  {ok, [StartSequence]} = io:fread("", "~s"),
  {ok, [TargetSequence]} = io:fread("", "~s"),
  spawn(main, do_work, [StartSequence, TargetSequence, self()]),
  wait_for_done().

%%% Start Homework %%%%%

%%----------------------------------------------------------------------
%% Function: run/2
%% Purpose: runs the program
%% Args:   InputSequence  : A list of DNA chemical bases that are out of order
%%         TargetSequence : A list of DNA chemicals bases in the proper order
%%----------------------------------------------------------------------
run(InputSequence, TargetSequence, PID) ->
  Seq = map_get_sequence(fun get_index/2, TargetSequence, InputSequence, 1), 
  spawn(main, mergesort, [Seq, 0, self(), parent]),
  receive {_, Inversions, _, _} -> 
     PID ! Inversions
  end.


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
%% Effect: I = 2, Input = a1ga
%%----------------------------------------------------------------------
map_get_sequence(_, [], List, _)    -> List;
map_get_sequence(F, [H|T], List, N) -> 
  I = F(H, List),
  NewList = replace_nth_node(I, N, List),
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
%% Function: mergesort/4
%% Purpose : Sorts a list recursively
%% Args:  [List] : The List
%%        Inversions  : The number of inversions
%%        Parent      : The Parent PID 
%%        Position    : A marker to keep track of the recursive tree (left, or right) 
%% Returns: The sorted list, total number of inversions, pid, and position
%%----------------------------------------------------------------------
mergesort([], Count, Parent, Position) -> Parent ! {[], Count, self(), Position};
mergesort([E], Count, Parent, Position) -> Parent ! {[E], Count, self(), Position};
mergesort(List, Inversions, Parent, Position) ->
	{Left, Right} = lists:split(trunc(length(List)/2), List),
	spawn(main, mergesort, [Left, Inversions, self(), left]),
	spawn(main, mergesort, [Right, Inversions, self(), right]),
	
	receiveInfo(Parent, Position).


%%----------------------------------------------------------------------
%% Function: receiveInfo/2
%% Purpose : recieves sorted data of the left and right subtrees
%% Args:  Parent : The parent pid
%%        Pos : The position of the list (left/right)
%% Returns: The sorted list, total number of inversions, pid, and position
%%----------------------------------------------------------------------
receiveInfo(Parent, Pos) ->
	receive {L1, Inv1, _, Pos1} ->
				receive {L2, Inv2, _, _} ->
							if Pos1 == left ->
								   {MList, TotalInv} = merge(L1, L2, Inv1 + Inv2),
								   Parent ! {MList, TotalInv, self(), Pos};
							   true -> 
								   {MList, TotalInv} = merge(L2,L1, Inv1 + Inv2),
								   Parent ! {MList, TotalInv, self(), Pos}
							end
				end 
	end.


%%----------------------------------------------------------------------
%% Function: merge/3
%% Purpose : sorts lists by comparing the first element in two arrays and combining
%%           them in the correct order
%% Args:  [A] : The left half of the unordered list
%%        [B] : The right half of the unordered list
%%        Inversions : the  current number of inversions
%% Returns: The total number of inversions and the new ordered list
%%----------------------------------------------------------------------
merge(A, [], Inversions) -> {A, Inversions};
merge([], B, Inversions) -> {B, Inversions};
merge([Ha|Ta], [Hb|Tb], Inversions) when Ha < Hb ->
	{List, NewInversions} = merge(Ta, [Hb|Tb], Inversions),
	{[Ha|List], NewInversions};
merge([Ha|Ta], [Hb|Tb], Inversions) ->
	{List, NewInversions} = merge([Ha|Ta], Tb, Inversions+1),
	{[Hb|List], NewInversions}.

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


%%--------------------------------------
%% Code provided in homework description
%%--------------------------------------
get_random_node() ->
  Length = length(net_adm:world()),
  World = net_adm:world(),
  rget_random_node(Length, World).

rget_random_node(1, [H|_]) -> H;
rget_random_node(_, [H|[]]) -> H;
rget_random_node(N, [_|T]) -> rget_random_node(N-1, T).
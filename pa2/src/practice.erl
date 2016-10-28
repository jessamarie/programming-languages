-module(practice).
-author("Jessica Barre").
-vsn(1.0).

%% Compiling
% $ erl
% > compile:file(module, [flags]).
% > c(module, [flags]).
% > module:function()
%%
%% -export([Function1/Arity, Function2/Arity, ..., FunctionN/Arity]).
-export([add/2, hello/0, greet_and_add_two/1]).

%% -compile([debug_info, export_all]).

%% -import(Module, [Function1/Arity, ..., FunctionN/Arity]).
%% -import(io, [format/1]).

%% MACROS %%
% -define(MACRO, some_value)
% used as ?MACRO inside any function
% -define(sub(X,Y), X-Y) a function macro
% used like ?sub(23,47)
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
% avoid using circular dependencies
% module A should not call a module B that also calls module A
%%

%% Methods! name(Args) -> Body.
add(A,B) ->
	A+B.

hello() ->
	%% if importing io (generally a bad practice):
	%% format("Hello, world!~n").
	io:format("Hello, world!~n").


greet_and_add_two(X) ->
	hello(),
	add(X,2).

greet(male, Name) -> 
	io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) -> 
	io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) -> 
	io:format("Hello, ~s!", [Name]).




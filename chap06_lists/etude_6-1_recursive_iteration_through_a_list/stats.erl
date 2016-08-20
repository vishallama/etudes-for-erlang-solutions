%% @author Vishal Lama
%% @doc Functions for calculating basic statistics on a list of numbers.
%% @copyright 2016 Vishal Lama
%% @version 0.1

-module(stats).
-export([mininum/1, maximum/1]).

%% @doc Returns the mininum item in a list of numbers. Fails when given an
%% empty list.

-spec(mininum(list(number())) -> number()).

mininum(List) -> mininum(List, hd(List)).

mininum([], Current) ->
  Current;
mininum([H|T], Current) when H < Current ->
  mininum(T, H);
mininum([_|T], Current) ->
  mininum(T, Current).

%% @doc Returns the maximum item in a list of numbers. Fails when given an
%% empty list.

-spec(maximum(list(number())) -> number()).

maximum(List) -> maximum(List, hd(List)).

maximum([], Current) ->
  Current;
maximum([H|T], Current) when H > Current ->
  maximum(T, H);
maximum([_|T], Current) ->
  maximum(T, Current).

